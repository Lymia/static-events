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

/// A handler that receives [`Event`]s and dispatches it to various handlers. This is not meant to
/// be defined directly, and instead [`EventDispatch`]s should be defined through [`EventRoot`],
/// or [`merged_event_handler!`].
// TODO: Update docs
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
macro_rules! merged_event_handler_internal {
    ($ev:ident, $($field_name:ident)*) => {
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
macro_rules! merged_event_handler {
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
            merged_event_handler_internal!(init        , $($field_name)*);
            merged_event_handler_internal!(check       , $($field_name)*);
            merged_event_handler_internal!(before_event, $($field_name)*);
            merged_event_handler_internal!(on_event    , $($field_name)*);
            merged_event_handler_internal!(after_event , $($field_name)*);
        }
    )*}
}
