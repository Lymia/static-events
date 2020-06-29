//! A handle holding a reference to an [`Handler`] for use in concurrent applications.

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::thread;
use std::time::{Duration, Instant};
use std::panic::{catch_unwind, AssertUnwindSafe};
use std::process::abort;
use parking_lot::{RwLock, RwLockWriteGuard};

use crate::handlers::{Handler, Events};

#[derive(Debug)]
enum Status<E: Events> {
    Inactive,
    Active(Handler<E>),
    InShutdown(Handler<E>),
    Shutdown,
    InvalidState,
}

#[derive(Debug)]
struct HandleData<E: Events> {
    status: RwLock<Status<E>>,
    shutdown_initialized: AtomicBool,
}

#[inline(never)]
#[cold]
pub fn already_shut_down() -> ! {
    panic!("Handle already shut down!")
}

/// A wrapper around a [`Handler`] designed to help with graceful shutdowns.
#[derive(Debug)]
pub struct EventsHandle<E: Events>(Arc<HandleData<E>>);
impl <E: Events> Clone for EventsHandle<E> {
    fn clone(&self) -> Self {
        EventsHandle(self.0.clone())
    }
}
impl <E: Events> EventsHandle<E> {
    pub fn new() -> EventsHandle<E> {
        EventsHandle(Arc::new(HandleData {
            status: RwLock::new(Status::Inactive),
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
            Status::InShutdown(_) | Status::Shutdown => true,
            Status::InvalidState => panic!("EventsHandle in invalid state."),
        }
    }

    /// Gets the number of active handlers from this handle, or handles cloned from it.
    pub fn lock_count(&self) -> usize {
        match &*self.0.status.read() {
            Status::Inactive => panic!("EventsHandle not yet active."),
            Status::Active(handler) | Status::InShutdown(handler) =>
                handler.refcount() - 1,
            Status::Shutdown => 0,
            Status::InvalidState => panic!("EventsHandle in invalid state."),
        }
    }

    fn initialize_shutdown(&self) {
        if self.0.shutdown_initialized.compare_and_swap(false, true, Ordering::SeqCst) {
            panic!("Attempt to shutdown a EventsHandle twice!");
        }
    }
    fn internal_shutdown(&self, mut lock: RwLockWriteGuard<Status<E>>) {
        if let Status::Active(handler) = std::mem::replace(&mut *lock, Status::InvalidState) {
            *lock = Status::InShutdown(handler);
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
        while self.lock_count() != 0 {
            thread::sleep(Duration::from_millis(1));
        }
        *self.0.status.write() = Status::Shutdown;
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
    pub fn shutdown_with_progress(&self, interval: Duration, mut progress_fn: impl FnMut()) {
        self.initialize_shutdown();

        let mut is_shutdown = false;
        let mut next_message = Instant::now() + interval;
        loop {
            if !is_shutdown {
                if let Some(lock) = self.0.status.try_write_for(interval) {
                    self.internal_shutdown(lock);
                    is_shutdown = true;
                }
            }

            if is_shutdown && self.lock_count() == 0 {
                break
            }

            let now = Instant::now();
            if now > next_message {
                if catch_unwind(AssertUnwindSafe(|| progress_fn())).is_err() {
                    abort();
                }
                next_message = now + interval;
                thread::sleep(Duration::from_millis(1));
            }
        }
        *self.0.status.write() = Status::Shutdown;
    }

    /// Returns the underlying [`Handler`], or panics if it has already been shut down.
    pub fn lock(&self) -> Handler<E> {
        match self.try_lock() {
            Some(v) => v,
            _ => already_shut_down(),
        }
    }

    /// Returns the underlying [`Handler`] wrapped in a [`Some`], or [`None`] if it has already
    /// been shut down.
    pub fn try_lock(&self) -> Option<Handler<E>> {
        if self.0.shutdown_initialized.load(Ordering::SeqCst) {
            None
        } else {
            let lock = self.0.status.read();
            match &*lock {
                Status::Inactive => panic!("DispatchHandle not yet active."),
                Status::Active(handler) => Some(handler.clone()),
                Status::InShutdown(_) | Status::Shutdown => None,
                Status::InvalidState => panic!("EventsHandle in invalid state."),
            }
        }
    }
}
