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
/// * `'a`: The lifetime the async portion of this event handler is linked to.
/// * `E`: The type of event handler this event is being dispatched into.
/// * `Ev`: The event this handler is for.
/// * `P`: The event phase this handler is for.
/// * `D`: A distinguisher used internally by the `#[event_dispatch]` to allow overlapping event
///        handler implementations. This defaults to [`DefaultHandler`], which is what [`Handler`]
///        actually invokes events with/
pub trait EventHandler<'a, E: Events, Ev: Event + 'a, P: EventPhase, D = DefaultHandler>: Events {
    /// `true` if this `EventHandler` actually does anything. Used for optimizations.
    const IS_IMPLEMENTED: bool;

    /// Runs a phase of this event.
    fn on_phase(
        &'a self, target: &'a Handler<E>, ev: &'a mut Ev, state: &'a mut Ev::State,
    ) -> EventResult;

    /// The type of the future used by this event handler.
    type FutureType: Future<Output = EventResult> + 'a;

    /// Runs a phase of this event asynchronously.
    fn on_phase_async(
        &'a self, target: &'a Handler<E>, ev: &'a mut Ev, state: &'a mut Ev::State,
    ) -> Self::FutureType;
}

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

    #[inline(never)]
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

    #[inline(never)]
    pub async fn dispatch_async<'a, Ev: Event + 'a>(&'a self, mut ev: Ev) -> Ev::RetVal {
        let mut state = ev.starting_state(self);
        'outer: loop {
            macro_rules! do_phase {
                ($phase:ident) => {
                    if crate::private::is_implemented::<E, E, Ev, $phase, DefaultHandler>() {
                        match await!(unsafe { crate::private::on_phase_async::<
                            E, E, Ev, $phase, DefaultHandler,
                        >(
                            &self.0, self, &mut ev, &mut state
                        ) }) {
                            EventResult::EvOk | EventResult::EvCancelStage => { }
                            EventResult::EvCancel => break 'outer,
                        }
                    }
                }
            }
            do_phase!(EvInit);
            do_phase!(EvCheck);
            do_phase!(EvBeforeEvent);
            do_phase!(EvOnEvent);
            do_phase!(EvAfterEvent);
            break 'outer
        };
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