use core::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use core::time::Duration;
use crate::std::sync::Arc;
use crate::std::panic::{catch_unwind, AssertUnwindSafe};
use crate::std::process::abort;
use parking_lot::{RwLock, RwLockWriteGuard};

use crate::interface::SyncEventDispatch;

#[derive(Debug)]
enum Status<D: SyncEventDispatch> {
    Active(D),
    Shutdown,
}

struct VisibleRefcountHandle<'a>(&'a AtomicUsize);
impl <'a> VisibleRefcountHandle<'a> {
    pub fn inc(atomic: &'a AtomicUsize) -> VisibleRefcountHandle {
        atomic.fetch_add(1, Ordering::Relaxed);
        VisibleRefcountHandle(atomic)
    }
}
impl <'a> Drop for VisibleRefcountHandle<'a> {
    fn drop(&mut self) {
        self.0.fetch_sub(1, Ordering::Relaxed);
    }
}

#[derive(Debug)]
struct HandleData<D: SyncEventDispatch> {
    status: RwLock<Status<D>>, visible_refcount: AtomicUsize, shutdown_initialized: AtomicBool,
}

/// A [`EventDispatch`] wrapped for use in applications that dispatch events concurrently.
#[derive(Debug)]
pub struct DispatchHandle<D: SyncEventDispatch>(Arc<HandleData<D>>);
impl <D: SyncEventDispatch> Clone for DispatchHandle<D> {
    fn clone(&self) -> Self {
        DispatchHandle(self.0.clone())
    }
}
impl <D: SyncEventDispatch> DispatchHandle<D> {
    pub fn new(d: D) -> DispatchHandle<D> {
        DispatchHandle(Arc::new(HandleData {
            status: RwLock::new(Status::Active(d)),
            visible_refcount: AtomicUsize::new(0),
            shutdown_initialized: AtomicBool::new(false),
        }))
    }

    // Gets whether this
    pub fn is_shutdown(&self) -> bool {
        self.0.shutdown_initialized.load(Ordering::SeqCst) || match &*self.0.status.read() {
            Status::Active(_) => false,
            Status::Shutdown => true,
        }
    }

    /// Gets the number of active locks on this handle, or handles cloned from it.
    pub fn lock_count(&self) -> usize {
        self.0.visible_refcount.load(Ordering::Relaxed)
    }

    fn initialize_shutdown(&self) {
        if self.0.shutdown_initialized.compare_and_swap(false, true, Ordering::SeqCst) {
            panic!("Attempt to shutdown a DispatchHandle twice!");
        }
    }
    fn internal_shutdown(&self, mut lock: RwLockWriteGuard<Status<D>>) {
        if let Status::Active(_) = &*lock {
            *lock = Status::Shutdown;
        } else {
            panic!("Attempt to shutdown a DispatchHandle twice! (unreachable case?)");
        }
    }

    /// Stops any further messages from being sent to this `DispatchHandle`, and drops the
    /// underlying event handler.
    ///
    /// This blocks until all locks on this handle are cleared.
    ///
    /// # Panics
    ///
    /// This function panics if an attempt is made to shutdown a handle twice.
    pub fn shutdown(&self) {
        self.initialize_shutdown();
        self.internal_shutdown(self.0.status.write());
    }

    /// Stops any further messages from being sent to this `DispatchHandle`, and drops the
    /// underlying event handler.
    ///
    /// This blocks until all locks on this handle are cleared. The closure provided is
    /// called at the specified interval to provide feedback to the user.
    ///
    /// # Panics
    ///
    /// This function panics if an attempt is made to shutdown a handle twice.
    #[must_use]
    pub fn shutdown_with_progress(&self, interval: Duration, mut progress_fn: impl FnMut()) {
        self.initialize_shutdown();
        loop {
            if let Some(lock) = self.0.status.try_write_for(interval) {
                self.internal_shutdown(lock);
                return
            }
            if catch_unwind(AssertUnwindSafe(|| progress_fn())).is_err() {
                abort();
            }
        }
    }

    /// Calls the given closure with the underlying [`EventDispatch`] and returns its return
    /// value wrapped in a [`Some`].
    ///
    /// If this handle has been shut down, then it instead returns [`None`] and does not call
    /// the closure.
    #[must_use]
    pub fn try_lock<T>(&self, f: impl FnOnce(&D) -> T) -> Option<T> {
        match &*self.0.status.read() {
            Status::Active(dispatch) => {
                let _handle = VisibleRefcountHandle::inc(&self.0.visible_refcount);
                Some(f(dispatch))
            },
            Status::Shutdown => None,
        }
    }

    /// Calls the given closure with the underlying [`EventDispatch`] and returns its return
    /// value.
    ///
    /// # Panics
    ///
    /// This function panics if this `DispatchHandle` has already been shut down.
    pub fn lock<T>(&self, f: impl FnOnce(&D) -> T) -> T {
        self.try_lock(f).expect("DispatchHandle has already been shut down.")
    }
}
