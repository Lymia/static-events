#[allow(unused_imports)] use crate::RootEventDispatch;

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

macro_rules! raw_event_dispatch {
    ($($ev:ident)*) => {
        /// The generic base trait used to define [`EventDispatch`]s.
        ///
        /// Each method of [`RawEventDispatch`] takes the [`EventDispatch`] the event was
        /// originally dispatched into in the `target` parameter, the event itself in the `ev`
        /// parameter, and the event's current state in the `state` parameter.
        ///
        /// The methods are called in the following order:<br>
        /// `init` -> `check` -> `before_event` -> `on_event` -> `after_event`
        pub trait RawEventDispatch: Sized {
            $(
                fn $ev<E: Event>(
                    &self, target: &impl EventDispatch, ev: &mut E, state: &mut E::State,
                ) -> EventResult;
            )*
        }
    };
}
raw_event_dispatch!(init check before_event on_event after_event);

/// A handler that receives [`Event`]s and processes them in some way.
///
/// This is not meant to define be defined directly. Derived event handlers should instead be
/// defined through the [`RootEventDispatch`] interface, and root event handlers should be
/// defined through [`RootEventDispatch`].
pub trait EventDispatch {
    /// Dispatches an event and returns its result.
    fn dispatch<E: Event>(&self, _: E) -> E::RetVal;
}
impl <T: RawEventDispatch> EventDispatch for T {
    fn dispatch<E: Event>(&self, mut ev: E) -> E::RetVal {
        let mut state = ev.starting_state(self);
        macro_rules! do_phase {
            ($ev:ident) => { match self.$ev(self, &mut ev, &mut state) {
                EvOk | EvCancelStage => { }
                EvCancel => return ev.to_return_value(self, state),
            } }
        }
        do_phase!(init);
        do_phase!(check);
        do_phase!(before_event);
        do_phase!(on_event);
        do_phase!(after_event);
        ev.to_return_value(self, state)
    }
}

#[doc(hidden)]
#[macro_export]
macro_rules! merged_event_dispatch_internal {
    (@method, $ev:ident, $($field_name:ident)*) => {
        fn $ev<E: $crate::Event>(
            &self, target: &impl $crate::EventDispatch, ev: &mut E, state: &mut E::State,
        ) -> $crate::EventResult {
            $(
                match $crate::RawEventDispatch::$ev(&self.$field_name, target, ev, state) {
                    $crate::EvOk => { }
                    e => return e,
                }
            )*
            $crate::EvOk
        }
    }
}

/// Creates an [`RawEventDispatch`] implementation for a struct by merging several
/// [`RawEventDispatch`]s. Each field of the struct must implement [`RawEventDispatch`].
///
/// The individual handlers will be called in the order that the fields are declared in.
///
/// At some point, this will be replaced with a procedural derive, i.e.
/// `#[derive(RawEventDispatch)]`.
#[macro_export]
#[allow_internal_unstable]
macro_rules! merged_event_dispatch {
    ($(
        $(#[$meta:meta])*
        $vis:vis struct $name:ident $(<$($ty_param:ident $(: $ty_bound:path)?),* $(,)?>)? {
            $(
                $(#[$field_meta:meta])* $field_vis:vis $field_name:ident: $field_type:ty
            ),* $(,)?
        }
    )*) => {$(
        $(#[$meta])*
        $vis struct $name $(<$($ty_param $(: $ty_bound)?,)*>)? {
            $($(#[$field_meta])* $field_vis $field_name: $field_type,)*
        }
        impl $(<$($ty_param $(: $ty_bound)?,)*>)?
            $crate::RawEventDispatch for $name $(<$($ty_param)*>)?
        {
            merged_event_dispatch_internal!(@method, init        , $($field_name)*);
            merged_event_dispatch_internal!(@method, check       , $($field_name)*);
            merged_event_dispatch_internal!(@method, before_event, $($field_name)*);
            merged_event_dispatch_internal!(@method, on_event    , $($field_name)*);
            merged_event_dispatch_internal!(@method, after_event , $($field_name)*);
        }
    )*}
}
