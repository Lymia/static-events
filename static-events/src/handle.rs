use core::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use core::time::Duration;
use std::sync::Arc;
use std::panic::{catch_unwind, AssertUnwindSafe};
use std::process::abort;
use parking_lot::{RwLock, RwLockWriteGuard, RwLockReadGuard, MappedRwLockReadGuard};

use crate::interface::{Event, EventDispatch, SyncEventDispatch};

#[derive(Debug)]
enum Status<D: SyncEventDispatch> {
    Inactive,
    Active(D),
    Shutdown,
}

struct RefcountHandle<'a>(&'a AtomicUsize);
impl <'a> RefcountHandle<'a> {
    pub fn inc(atomic: &'a AtomicUsize) -> RefcountHandle {
        atomic.fetch_add(1, Ordering::Relaxed);
        RefcountHandle(atomic)
    }
}
impl <'a> Drop for RefcountHandle<'a> {
    fn drop(&mut self) {
        self.0.fetch_sub(1, Ordering::Relaxed);
    }
}

pub struct DispatchHandleLock<'a, D: SyncEventDispatch> {
    _refcount: RefcountHandle<'a>, lock: MappedRwLockReadGuard<'a, D>,
}
impl <'a, D: SyncEventDispatch> EventDispatch for DispatchHandleLock<'a, D> {
    fn dispatch<E: Event>(&self, ev: E) -> E::RetVal {
        self.lock.dispatch(ev)
    }
    fn downcast_ref<D2: 'static>(&self) -> Option<&D2> {
        self.lock.downcast_ref()
    }
}

#[derive(Debug)]
struct HandleData<D: SyncEventDispatch> {
    status: RwLock<Status<D>>, display_refcount: AtomicUsize, shutdown_initialized: AtomicBool,
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
    pub fn new() -> DispatchHandle<D> {
        DispatchHandle(Arc::new(HandleData {
            status: RwLock::new(Status::Inactive),
            display_refcount: AtomicUsize::new(0),
            shutdown_initialized: AtomicBool::new(false),
        }))
    }

    // Sets the handler underlying this DispatchHandle. May only be called once.
    pub fn activate_handle(&self, handler: D) {
        let mut lock = self.0.status.write();
        if let Status::Inactive = *lock {
            *lock = Status::Active(handler);
        } else {
            panic!("DispatchHandle already activated.")
        }
    }

    // Gets whether this DispatchHandle is active.
    pub fn is_shutdown(&self) -> bool {
        self.0.shutdown_initialized.load(Ordering::SeqCst) || match &*self.0.status.read() {
            Status::Inactive => panic!("DispatchHandle not yet active."),
            Status::Active(_) => false,
            Status::Shutdown => true,
        }
    }

    /// Gets the number of active locks on this handle, or handles cloned from it.
    pub fn lock_count(&self) -> usize {
        self.0.display_refcount.load(Ordering::Relaxed)
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

    /// Returns a lock to the underlying `DispatchHandle` wrapped in a [`Some`], or [`None`]
    /// if it has already been shut down.
    pub fn lock(&self) -> Option<DispatchHandleLock<D>> {
        let lock = self.0.status.read();
        match &*lock {
            Status::Inactive => panic!("DispatchHandle not yet active."),
            Status::Active(_) => {
                let lock = RwLockReadGuard::map(lock, |x| match x {
                    Status::Active(dispatch) => dispatch,
                    _ => unreachable!(),
                });
                let refcount = RefcountHandle::inc(&self.0.display_refcount);
                Some(DispatchHandleLock { lock, _refcount: refcount })
            },
            Status::Shutdown => None,
        }
    }
}
