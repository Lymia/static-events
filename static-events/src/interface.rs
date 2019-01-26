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

/// The result of a stage of a event handler.
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub enum EventResult {
    /// Continues normally.
    EvOk,
    /// Cancels further event handlers from processing the current phase of the current event.
    /// (i.e. `init`, `check`, `before_event`, `on_event`, and `after_event`)
    EvCancelStage,
    /// Cancels further event handlers from processing the current event.
    EvCancel,
}
impl Default for EventResult {
    fn default() -> Self {
        EvOk
    }
}
impl From<()> for EventResult {
    fn from(_: ()) -> Self {
        EvOk
    }
}
pub use self::EventResult::{EvOk, EvCancelStage, EvCancel};

/// The generic trait that defines an event.
pub trait Event {
    /// The type of the state maintained between event handler calls.
    type State;
    /// The type of the state that is passed to an event handler's methods.
    type StateArg;
    /// The return value of an event handler's methods.
    type MethodRetVal;
    /// The ultimate return type of a call to this event.
    type RetVal;

    /// The starting state when an event is dispatched.
    fn starting_state(&self, _: &impl EventDispatch) -> Self::State;
    /// Borrows the state passed to an event handler's methods from its internal representation.
    fn borrow_state<'a>(&self, _: &'a mut Self::State) -> &'a mut Self::StateArg;
    /// The default return value for handlers without an explicit implementation of this event.
    fn default_return(&self) -> Self::MethodRetVal;
    /// Extracts an [`EventResult`] from the method return value.
    fn to_event_result(&self, _: &mut Self::State, _: Self::MethodRetVal) -> EventResult;
    /// Derives the output value of the event dispatch from the current state.
    fn to_return_value(&self, _: &impl EventDispatch, _: Self::State) -> Self::RetVal;
}

/// An [`Event`] that does not use the `MethodRetVal` or `StateArg` mechanisms.
pub trait SimpleInterfaceEvent {
    /// The type of the state maintained between event handler calls.
    type State;
    /// The ultimate return type of a call to this event.
    type RetVal;

    /// The starting state when an event is dispatched.
    fn starting_state(&self, _: &impl EventDispatch) -> Self::State;
    /// Derives the output value of the event dispatch from the current state.
    fn to_return_value(&self, _: &impl EventDispatch, _: Self::State) -> Self::RetVal;
}
impl <T : SimpleInterfaceEvent> Event for T {
    type State = T::State;
    type StateArg = T::State;
    type MethodRetVal = EventResult;
    type RetVal = T::RetVal;

    fn starting_state(&self, target: &impl EventDispatch) -> T::State {
        SimpleInterfaceEvent::starting_state(self, target)
    }
    fn borrow_state<'a>(&self, state: &'a mut T::State) -> &'a mut T::State {
        state
    }
    fn default_return(&self) -> EventResult {
        EvOk
    }
    fn to_event_result(&self, _: &mut T::State, result: EventResult) -> EventResult {
        result
    }
    fn to_return_value(&self, target: &impl EventDispatch, state: T::State) -> T::RetVal {
        SimpleInterfaceEvent::to_return_value(self, target, state)
    }
}

/// An [`Event`] that returns `State` directly when called.
pub trait SimpleEvent {
    /// The type of the state maintained between event handler calls and returned from this event.
    type State;
    /// The starting state when an event is dispatched.
    fn starting_state(&self, _: &impl EventDispatch) -> Self::State;
}
impl <T : SimpleEvent> SimpleInterfaceEvent for T {
    type State = T::State;
    type RetVal = T::State;

    fn starting_state(&self, target: &impl EventDispatch) -> T::State {
        SimpleEvent::starting_state(self, target)
    }
    fn to_return_value(&self, _: &impl EventDispatch, state: T::State) -> T::State {
        state
    }
}

/// An [`Event`] that returns no value adn stores no state.
pub trait VoidEvent { }
impl <T : VoidEvent> SimpleEvent for T {
    type State = ();
    fn starting_state(&self, _: &impl EventDispatch) {  }
}

/// The generic base trait used to define [`EventDispatch`]s.
///
/// Each method of [`RawEventDispatch`] takes the [`EventDispatch`] the event was
/// originally dispatched into in the `target` parameter, the event itself in the `ev`
/// parameter, and the event's current state in the `state` parameter.
///
/// The methods are called in the following order:<br>
/// `EvInit` -> `EvCheck` -> `EvBeforeEvent` -> `EvOnEvent` -> `EvAfterEvent`
pub trait RawEventDispatch: Sized {
    fn on_phase<E: Event, P: EventPhase, D: EventDispatch>(
        &self, target: &D, ev: &mut E, state: &mut E::State,
    ) -> EventResult;
}

/// A trait implements [`EventDispatch`] using [`EventHandler`]s.
pub trait RootEventDispatch { }

/// The main trait used to define [`RootEventDispatch`]s.
///
/// Each explicit implementation of this trait defines how one reacts to a particular
/// type of event during a particular phase.
pub trait EventHandler<E: Event, P: EventPhase> : RootEventDispatch {
    fn on_phase(
        &self, _: &impl EventDispatch, event: &mut E, _: &mut E::StateArg,
    ) -> E::MethodRetVal;
}

trait UniversalEventHandler<E: Event, P: EventPhase> {
    fn on_phase(
        &self, _: &impl EventDispatch, event: &mut E, _: &mut E::StateArg,
    ) -> E::MethodRetVal;
}
impl <E: Event, P: EventPhase, T> UniversalEventHandler<E, P> for T {
    default fn on_phase(
        &self, _: &impl EventDispatch, event: &mut E, _: &mut E::StateArg,
    ) -> E::MethodRetVal {
        event.default_return()
    }
}
impl <E: Event, P: EventPhase, T: EventHandler<E, P>> UniversalEventHandler<E, P> for T {
    default fn on_phase(
        &self, target: &impl EventDispatch, event: &mut E, state: &mut E::StateArg,
    ) -> E::MethodRetVal {
        EventHandler::on_phase(self, target, event, state)
    }
}

impl <T: RootEventDispatch> RawEventDispatch for T {
    default fn on_phase<E: Event, P: EventPhase, D: EventDispatch>(
        &self, target: &D, ev: &mut E, state: &mut E::State,
    ) -> EventResult {
        let result = {
            let state_arg = ev.borrow_state(state);
            UniversalEventHandler::<E, P>::on_phase(self, target, ev, state_arg)
        };
        ev.to_event_result(state, result)
    }
}

/// A handler that receives [`Event`]s and processes them in some way.
///
/// This is not meant to define be defined directly. Derived event handlers should instead be
/// defined through the [`RootEventDispatch`] interface, and root event handlers should be
/// defined through [`RootEventDispatch`].
pub trait EventDispatch: Sized {
    /// Dispatches an event and returns its result.
    fn dispatch<E: Event>(&self, _: E) -> E::RetVal;
}
impl <T: RawEventDispatch> EventDispatch for T {
    fn dispatch<E: Event>(&self, mut ev: E) -> E::RetVal {
        let mut state = ev.starting_state(self);
        macro_rules! do_phase {
            ($phase:ident) => {
                match self.on_phase::<E, $phase, Self>(self, &mut ev, &mut state) {
                    EvOk | EvCancelStage => { }
                    EvCancel => return ev.to_return_value(self, state),
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