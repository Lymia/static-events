extern crate std;

use parking_lot::RwLock;
use self::std::sync::Arc;

use crate::interface::SyncEventDispatch;

#[derive(Debug)]
enum Status<D: SyncEventDispatch> {
    Active(D),
    Shutdown,
}

/// A [`EventDispatch`] wrapped for use in applications that dispatch events concurrently.
#[derive(Debug)]
pub struct DispatchHandle<D: SyncEventDispatch>(Arc<RwLock<Status<D>>>);
impl <D: SyncEventDispatch> Clone for DispatchHandle<D> {
    fn clone(&self) -> Self {
        DispatchHandle(self.0.clone())
    }
}
impl <D: SyncEventDispatch> DispatchHandle<D> {
    pub fn new(d: D) -> DispatchHandle<D> {
        DispatchHandle(Arc::new(RwLock::new(Status::Active(d))))
    }

    /// Stops any further messages from being sent to this `DispatchHandle`, and drops the
    /// underlying event handler.
    pub fn shutdown(&self) {
        let mut lock = self.0.write();
        if let Status::Active(_) = &*lock {
            *lock = Status::Shutdown;
        } else {
            panic!("Attempt to shut down a DispatchHandle twice!");
        }
    }

    /// Calls the given closure with the underlying [`EventDispatch`] and returns its return
    /// value wrapped in a [`Some`].
    ///
    /// If this handle has been shut down, then it instead returns [`None`] and does not call
    /// the closure.
    #[must_use]
    pub fn try_lock<T>(&self, f: impl FnOnce(&D) -> T) -> Option<T> {
        match &*self.0.read() {
            Status::Active(dispatch) => Some(f(dispatch)),
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
