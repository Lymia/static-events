//! The underlying traits used to define event handlers.

use crate::events::*;
use std::future::Future;

pub(crate) mod private {
    pub trait Sealed { }
}

/// A sealed trait that marks the phases of an event.
pub trait EventPhase: private::Sealed { }
macro_rules! phases {
    ($(($name:ident, $doc:literal))*) => {$(
        #[doc = $doc]
        pub enum $name { }
        impl private::Sealed for $name { }
        impl EventPhase for $name { }
    )*}
}
phases! {
    (EvInit       , "The first phase of event execution. Intended to be used to set up an event.")
    (EvCheck      , "The second phase of event execution. Intended to be used to check \
                     conditions required for the execution of the event.")
    (EvBeforeEvent, "The third phase of event execution. Intended to be used for hooks that \
                     execute before the main actions of an event.")
    (EvOnEvent    , "The fourth phase of event execution. Intended to be used for the main \
                     actions of an event.")
    (EvAfterEvent , "The fifth phase of event execution. Intended to be used for hooks that \
                     execute after the main actions of an event.")
}

/// Distinguisher for the actual default handler invoked by [`Handler`]
pub enum DefaultHandler { }

/// The base trait used to mark event dispatchers.
pub trait Events: 'static + Sized {
    /// Gets a service from this event dispatch.
    fn get_service<S>(&self) -> Option<&S>;
}

/// A trait that defines a phase of handling a particular event.
///
/// # Type parameters
/// * `E`: The type of event handler this event is being dispatched into.
/// * `Ev`: The event this handler is for.
/// * `P`: The event phase this handler is for.
/// * `D`: A distinguisher used internally by the `#[event_dispatch]` to allow overlapping event
///        handler implementations. This defaults to [`DefaultHandler`], which is what [`Handler`]
///        actually invokes events with/
pub trait EventHandler<E: Events, Ev: Event, P: EventPhase, D = DefaultHandler>: Events {
    /// `true` if this `EventHandler` actually does anything. Used for optimizations.
    const IS_IMPLEMENTED: bool;

    /// Runs a phase of this event.
    fn on_phase(
        &self, target: &Handler<E>, ev: &mut Ev, state: &mut Ev::State,
    ) -> EventResult;

    /// The type of the future used by this event handler.
    type FutureType: Future<Output = EventResult>;

    /// Runs a phase of this event asynchronously.
    ///
    /// # Safety
    /// This function erases the implicit bound of the output on all the input parameters. Care
    /// must be taken to restore them or avoid breaking lifetime restrictions.
    unsafe fn on_phase_async(ctx: AsyncDispatchContext<Self, E, Ev>) -> Self::FutureType;
}

/// A wrapper for the parameters to [`EventHandler::on_phase_async`] to preserve [`Sync`]/[`Send`]
/// status across the pointers used.
///
/// This is part of a hack around a bug currently existing with `existential_type`.
pub struct AsyncDispatchContext<T: Events, E: Events, Ev: Event> {
    pub(crate) this: *const T,
    pub(crate) target: *const Handler<E>,
    pub(crate) ev: *mut Ev,
    pub(crate) state: *mut Ev::State,
}
impl <T: Events, E: Events, Ev: Event> AsyncDispatchContext<T, E, Ev> {
    pub unsafe fn this(&self) -> &T { &*(self.this) }
    pub unsafe fn target(&self) -> &Handler<E> { &*(self.target) }
    pub unsafe fn ev(&self) -> &mut Ev { &mut *(self.ev) }
    pub unsafe fn state(&self) -> &mut Ev::State { &mut *(self.state) }
}
unsafe impl <T: Events, E: Events, Ev: Event> Send for AsyncDispatchContext<T, E, Ev>
    where for <'a> &'a T: Send,
          for <'a> &'a Handler<E>: Send,
          for <'a> &'a mut Ev: Send,
          for <'a> &'a mut Ev::State: Send { }
unsafe impl <T: Events, E: Events, Ev: Event> Sync for AsyncDispatchContext<T, E, Ev>
    where for <'a> &'a T: Sync,
          for <'a> &'a Handler<E>: Sync,
          for <'a> &'a mut Ev: Sync,
          for <'a> &'a mut Ev::State: Sync { }

#[repr(transparent)]
/// A wrapper for [`Events`] that allows dispatching events into them.
pub struct Handler<E: Events>(E);
impl <E: Events> Handler<E> {
    pub fn new(e: E) -> Self {
        Handler(e)
    }

    pub fn get_service<S>(&self) -> Option<&S> {
        self.0.get_service()
    }

    pub fn downcast_ref<E2: Events>(&self) -> Option<&Handler<E2>> {
        crate::private::CheckDowncast::<Handler<E2>>::downcast_ref(self)
    }

    pub fn dispatch<Ev: Event>(&self, mut ev: Ev) -> Ev::RetVal {
        let mut state = ev.starting_state(self);
        macro_rules! do_phase {
            ($phase:ident) => {
                if crate::private::is_implemented::<E, E, Ev, $phase, DefaultHandler>() {
                    match crate::private::on_phase::<E, E, Ev, $phase, DefaultHandler>(
                        &self.0, self, &mut ev, &mut state
                    ) {
                        EventResult::EvOk | EventResult::EvCancelStage => { }
                        EventResult::EvCancel => return ev.to_return_value(self, state),
                    }
                }
            }
        }
        do_phase!(EvInit);
        do_phase!(EvCheck);
        do_phase!(EvBeforeEvent);
        do_phase!(EvOnEvent);
        do_phase!(EvAfterEvent);
        ev.to_return_value(self, state)
    }

    pub async fn dispatch_async<'a, Ev: Event + 'a>(&'a self, mut ev: Ev) -> Ev::RetVal {
        let mut state = ev.starting_state(self);
        macro_rules! do_phase {
            ($phase:ident) => {
                if crate::private::is_implemented::<E, E, Ev, $phase, DefaultHandler>() {
                    match await!(crate::private::on_phase_async::<E, E, Ev, $phase, DefaultHandler>(
                        &self.0, self, &mut ev, &mut state
                    )) {
                        EventResult::EvOk | EventResult::EvCancelStage => { }
                        EventResult::EvCancel => return ev.to_return_value(self, state),
                    }
                }
            }
        }
        do_phase!(EvInit);
        do_phase!(EvCheck);
        do_phase!(EvBeforeEvent);
        do_phase!(EvOnEvent);
        do_phase!(EvAfterEvent);
        ev.to_return_value(self, state)
    }
}

/*
/// A [`RawEventDispatch`] that can be shared between threads.
pub trait SyncRawEventDispatch: RawEventDispatch + Sync + Send { }
impl <T: RawEventDispatch + Sync + Send> SyncEventDispatch for T { }

/// A [`EventDispatch`] that can be shared between threads.
pub trait SyncEventDispatch: EventDispatch + Sync + Send { }
impl <T: EventDispatch + Sync + Send> SyncEventDispatch for T { }
*/