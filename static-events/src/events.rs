//! The underlying traits used to define events.
//!
//! # Defining events
//!
//! An hierarchy of helper traits exists for defining event types:
//! * [`Event`] exposes the full power of the events system, and is required for events that
//!   only pass part of the dispatch state into handlers, or expect handlers to return a different
//!   type than usual.
//! * [`SimpleInterfaceEvent`] is required for events for event that return a different type from
//!   their internal state.
//! * [`SimpleEvent`] is the most general common type of event. It directly returns its internal
//!   state to the caller.
//! * [`VoidEvent`] is the simplest type of event, that maintains no state and returns no value.
//!
//! In addition, helper macros exist to help define specific types of common events;
//! * [`simple_event!`] for events that directly return their state to the caller, or do not
//!   use state at all.
//! * [`failable_event!`] for events that can fail and return [`Result`]s.
//! * [`ipc_event!`] for events that should only have one listener processing it.
//!   [`IpcEventHandler`] should be used instead of [`EventHandler`] for events of this type.

use crate::handlers::*;

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
        EventResult::EvOk
    }
}
impl From<()> for EventResult {
    fn from(_: ()) -> Self {
        EventResult::EvOk
    }
}

/// The generic trait that defines an event.
pub trait Event {
    /// The type of state stored on the stack during event dispatch.
    ///
    /// A value of this type is constructed at the start of event dispatch by calling the
    /// `starting_state` method.
    type State;
    /// The type of the state that event handlers receive. This can be distinct from `State`,
    /// allowing a part of the state to be hidden from event handlers.
    ///
    /// This is derived from `State` by calling the `borrow_state` method.
    ///
    /// Note that the full `State` value is still exposed to [`RawEventDispatch`].
    type StateArg;
    /// The return value of an event handler's methods.
    ///
    /// [`EventDispatch`]s default to calling `default_return` if no explicit handler for this
    /// event type is defined. Note that this can happen multiple times during one event dispatch
    /// for merged [`EventDispatch`]s.
    ///
    /// After an event handler is called, whether default or not, `to_event_result` is called
    /// to decide how to proceed with event dispatch and update the state according to its
    /// return value.
    type MethodRetVal;
    /// The type returned to the method that originally started the event dispatch.
    ///
    /// This is derived from the internal state by calling `to_return_value`
    type RetVal;

    /// Constructs the state maintained during an event dispatch.
    fn starting_state(&self, _: &impl EventDispatch) -> Self::State;
    /// Borrows the part of the state that is passed to event dispatches.
    fn borrow_state<'a>(&self, _: &'a mut Self::State) -> &'a mut Self::StateArg;
    /// The default return value for handlers without an explicit implementation of this event.
    fn default_return(&self) -> Self::MethodRetVal;
    /// Extracts an [`EventResult`] and updates state based on an event handler's return value.
    fn to_event_result(&self, _: &mut Self::State, _: Self::MethodRetVal) -> EventResult;
    /// Derives the output of the event dispatch from the current state.
    fn to_return_value(&self, _: &impl EventDispatch, _: Self::State) -> Self::RetVal;
}

/// An [`Event`] that does not use the `MethodRetVal` or `StateArg` mechanisms.
pub trait SimpleInterfaceEvent {
    /// The type of state stored on the stack during event dispatch, and passed to event handlers.
    ///
    /// A value of this type is constructed at the start of event dispatch by calling the
    /// `starting_state` method.
    type State;
    /// The ultimate return type of a call to this event.
    type RetVal;

    /// Constructs the state maintained during an event dispatch.
    fn starting_state(&self, _: &impl EventDispatch) -> Self::State;
    /// Derives the output of the event dispatch from the current state.
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
        EventResult::EvOk
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
    ///
    /// A value of this type is constructed at the start of event dispatch by calling the
    /// `starting_state` method.
    type State;
    /// Constructs the state maintained during an event dispatch.
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

/// An [`Event`] that returns no value and stores no state.
pub trait VoidEvent { }
impl <T : VoidEvent> SimpleEvent for T {
    type State = ();
    fn starting_state(&self, _: &impl EventDispatch) {  }
}
