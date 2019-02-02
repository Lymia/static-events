//! The underlying traits used to define event handlers.

use crate::events::*;

mod private {
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

/// The generic base trait used to define [`EventDispatch`]s.
///
/// Each method of [`RawEventDispatch`] takes the [`EventDispatch`] the event was
/// originally dispatched into in the `target` parameter, the event itself in the `ev`
/// parameter, and the event's current state in the `state` parameter.
///
/// The methods are called in the following order:<br>
/// `EvInit` -> `EvCheck` -> `EvBeforeEvent` -> `EvOnEvent` -> `EvAfterEvent`
///
/// This trait should only be implemented directly for advanced manipulations of the
/// events system. Most users should rely on `#[derive(RawEventDispatch)]` and `#[event_dispatch]`
/// instead.
pub trait RawEventDispatch: Sized + 'static {
    fn on_phase<E: Event, P: EventPhase, D: EventDispatch>(
        &self, target: &D, ev: &mut E, state: &mut E::State,
    ) -> EventResult;
}

/// A handler that receives [`Event`]s and processes them in some way.
///
/// This is not meant to define be defined directly. See [`RawEventDispatch`] instead.
pub trait EventDispatch: Sized + 'static {
    /// Dispatches an event and returns its result.
    fn dispatch<E: Event>(&self, _: E) -> E::RetVal;
}
impl <T: RawEventDispatch> EventDispatch for T {
    fn dispatch<E: Event>(&self, mut ev: E) -> E::RetVal {
        let mut state = ev.starting_state(self);
        macro_rules! do_phase {
            ($phase:ident) => {
                match self.on_phase::<E, $phase, Self>(self, &mut ev, &mut state) {
                    EventResult::EvOk | EventResult::EvCancelStage => { }
                    EventResult::EvCancel => return ev.to_return_value(self, state),
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

/// A [`RawEventDispatch`] that can be shared between threads.
pub trait SyncRawEventDispatch: RawEventDispatch + Sync + Send { }
impl <T: RawEventDispatch + Sync + Send> SyncEventDispatch for T { }

/// A [`EventDispatch`] that can be shared between threads.
pub trait SyncEventDispatch: EventDispatch + Sync + Send { }
impl <T: EventDispatch + Sync + Send> SyncEventDispatch for T { }