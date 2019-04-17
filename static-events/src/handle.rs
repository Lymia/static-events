//! A handle holding a reference to an [`Handler`] for use in concurrent applications.

use std::ops::Deref;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::Arc;
use std::time::Duration;
use std::panic::{catch_unwind, AssertUnwindSafe};
use std::process::abort;
use parking_lot::{RwLock, RwLockWriteGuard, RwLockReadGuard, MappedRwLockReadGuard};

use crate::handlers::{Handler, SyncEvents};

#[derive(Debug)]
enum Status<E: SyncEvents> {
    Inactive,
    Active(Handler<E>),
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

pub struct EventsHandleLock<'a, E: SyncEvents> {
    _refcount: RefcountHandle<'a>, lock: MappedRwLockReadGuard<'a, Handler<E>>,
}
impl <'a, E: SyncEvents> Deref for EventsHandleLock<'a, E> {
    type Target = Handler<E>;
    fn deref(&self) -> &Handler<E> {
        &self.lock
    }
}

#[derive(Debug)]
struct HandleData<E: SyncEvents> {
    status: RwLock<Status<E>>, display_refcount: AtomicUsize, shutdown_initialized: AtomicBool,
}

/// A [`Handler`] wrapped for use in applications that dispatch events concurrently.
#[derive(Debug)]
pub struct EventsHandle<E: SyncEvents>(Arc<HandleData<E>>);
impl <E: SyncEvents> Clone for EventsHandle<E> {
    fn clone(&self) -> Self {
        EventsHandle(self.0.clone())
    }
}
impl <E: SyncEvents> EventsHandle<E> {
    pub fn new() -> EventsHandle<E> {
        EventsHandle(Arc::new(HandleData {
            status: RwLock::new(Status::Inactive),
            display_refcount: AtomicUsize::new(0),
            shutdown_initialized: AtomicBool::new(false),
        }))
    }

    // Sets the handler underlying this EventsHandle. May only be called once.
    pub fn activate_handle(&self, events: E) {
        let mut lock = self.0.status.write();
        if let Status::Inactive = *lock {
            *lock = Status::Active(Handler::new(events));
        } else {
            panic!("EventsHandle already activated.")
        }
    }

    // Gets whether this DispatchHandle is active.
    pub fn is_shutdown(&self) -> bool {
        self.0.shutdown_initialized.load(Ordering::SeqCst) || match &*self.0.status.read() {
            Status::Inactive => panic!("EventsHandle not yet active."),
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
            panic!("Attempt to shutdown a EventsHandle twice!");
        }
    }
    fn internal_shutdown(&self, mut lock: RwLockWriteGuard<Status<E>>) {
        if let Status::Active(_) = &*lock {
            *lock = Status::Shutdown;
        } else {
            panic!("Attempt to shutdown a EventsHandle twice! (unreachable case?)");
        }
    }

    /// Stops any further messages from being sent to this `EventsHandle`, and drops the
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

    /// Stops any further messages from being sent to this `EventsHandle`, and drops the
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

    /// Returns a lock to the underlying [`Handler`] wrapped in a [`Some`], or [`None`] if it has
    /// already been shut down.
    pub fn lock(&self) -> Option<EventsHandleLock<E>> {
        let lock = self.0.status.read();
        match &*lock {
            Status::Inactive => panic!("DispatchHandle not yet active."),
            Status::Active(_) => {
                let lock = RwLockReadGuard::map(lock, |x| match x {
                    Status::Active(dispatch) => dispatch,
                    _ => unreachable!(),
                });
                let refcount = RefcountHandle::inc(&self.0.display_refcount);
                Some(EventsHandleLock { lock, _refcount: refcount })
            },
            Status::Shutdown => None,
        }
    }
}
