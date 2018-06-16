#![no_std]
#![feature(specialization, macro_vis_matcher, macro_at_most_once_rep, allow_internal_unstable)]

// TODO: Use custom derive rather than the merged_eventset! macro.
// TODO: Add a parallel set of traits for Futures

//! A generic zero-cost event handler system. Event dispatches should get compiled down to a
//! plain function that executes all handlers involved with no dynamic dispatches.
//!
//! As this crate depends on specialization, you must enable `#![feature(specialization)]` in
//! all crates that use this library:
//!
//! ```
//! #![feature(specialization)]
//! #[macro_use] extern crate static_events;
//! ```
//!
//! # Basic model
//!
//! Events can be any type that implements [`Event`], and the type itself is used as the key
//! that event handlers to distinguish different events.
//!
//! Event handlers are primarily defined by types that implement [`EventRoot`], which provides
//! a default noop [`EventHandler`] implementation for all [`Event`]s, which can be further
//! overwritten with explicit [`EventHandler`] implementations.
//!
//! [`EventSet`] is a further abstraction over [`EventRoot`], that has methods that are
//! individually generic over all [`Event`]s, rather than having an impl of [`EventHandler`]
//! for all [`Event`]s like [`EventRoot`].
//!
//! Finally, [`EventDispatch`] is the highest level event handling trait, with a function that
//! takes a [`Event`], and returns its `RetVal`.
//!
//! # Event dispatch
//!
//! Events dispatches fundamentally behave as function calls built up extensibly from individual
//! event handlers. They take an event, and return a value (which may simply be `()`).
//!
//! At the beginning of an event dispatch, [`Event::starting_state`] is called once to create
//! a temporary state value. This will be passed to all event handlers, and is used to store
//! transient state and as an accumulator for the final return value.
//!
//! The lower level event handlers, [`EventHandler`] and [`EventSet`] each contain 5 methods
//! that are executed in the following order:<br>
//! `init` -> `check` -> `before_event` -> `on_event` -> `after_event`
//!
//! They take both the event itself and the current state as mutable borrows, and return a
//! status value controlling the rest of the event dispatch:
//! * [`EvOk`] continues the event dispatch as normal.
//! * [`EvCancelStage`] prevents the execution of the currently executing method any further
//!   event handlers.
//! * [`EvCancel`] immediately stops the event dispatch, proceeding to the calculation of the
//!   return value.
//!
//! Finally, at the end of event dispatch, [`Event::to_return_value`] is called on the state
//! to compute the final return value. In many cases, this will simply be a noop.
//!
//! # Defining events
//!
//! Any module can define an event. Events are normal types that implement the [`Event`] trait,
//! which can be declared with various macros;
//! * [`simple_event!`] for events that directly return their state to the caller, or do not
//!   use state at all.
//! * [`failable_event!`] for events that can fail and return [`Result`]s.
//! * [`ipc_event!`] for events that should only have one listener processing it.
//!
//! ```
//! # #[macro_use] extern crate static_events;
//! # use static_events::*;
//! pub struct MyEvent(u32);
//! simple_event!(MyEvent, u32, 0);
//! ```
//!
//! While [`Event`] is stable API, and can be manually implemented, this should only be done in
//! special cases.
//!
//! # Defining event handlers
//!
//! Individual event handlers are defined using a combination of [`EventRoot`] (a marker trait),
//! and any number of [`EventHandler`] implementations:
//!
//! ```
//! # #![feature(specialization)]
//! # #[macro_use] extern crate static_events;
//! # use static_events::*;
//! # struct MyEvent(u32);
//! # simple_event!(MyEvent, u32, 0);
//! struct MyEventHandler;
//! impl EventRoot for MyEventHandler { }
//! impl EventHandler<MyEvent> for MyEventHandler {
//!     fn on_event(&self, _: &impl EventDispatch, ev: &mut MyEvent, i: &mut u32) -> EventResult {
//!         *i += ev.0;
//!         EvOk
//!     }
//! }
//!
//! assert_eq!(MyEventHandler.dispatch(MyEvent(42)), 42);
//! ```
//!
//! [`simple_event_handler!`] may also be used instead for handlers with no state or parameters.
//! A [`event_handler!`] macro also exists which adds [`EventHandler`] impls to an existing
//! struct, rather than creating a new one:
//! ```
//! # #![feature(specialization)]
//! # #[macro_use] extern crate static_events;
//! # use static_events::*;
//! # struct MyEvent(u32);
//! # simple_event!(MyEvent, u32, 0);
//! simple_event_handler!(MyEventHandler, MyEvent: {
//!     on_event: |_, ev, i| { *i += ev.0 }
//! });
//! # assert_eq!(MyEventHandler.dispatch(MyEvent(42)), 42);
//! ```
//!
//! Finally, multiple event handlers may be merged using the [`merged_eventset!`] macro:
//! ```
//! # #![feature(specialization)]
//! # #[macro_use] extern crate static_events;
//! # use static_events::*;
//! # struct MyEvent(u32);
//! # simple_event!(MyEvent, u32, 0);
//! simple_event_handler!(MyEventHandler, MyEvent: {
//!     on_event: |_, ev, i| { *i += ev.0 }
//! });
//! simple_event_handler!(MyOtherEventHandler, MyEvent: {
//!     on_event: |_, ev, i| { *i *= ev.0 }
//! });
//!
//! merged_eventset! {
//!     #[derive(Default)]
//!     struct SquaringEventHandler<T: EventSet> {
//!         evh_a: MyEventHandler, evh_b: T,
//!     }
//! }
//!
//! assert_eq!(SquaringEventHandler::<MyOtherEventHandler>::default().dispatch(MyEvent(9)), 81);
//! ```
//!
//! # Limitations
//!
//! A fundamental limitation to this approach is that event handlers cannot be dynamically added
//! or removed at runtime, and sets of handlers can only be defined at compile-time.
//!
//! As all event handlers are passed around using immutable pointers, locking or cells must be
//! used to store state in handlers.


#[allow(unused_imports)] extern crate unhygienic;
#[doc(hidden)] pub use unhygienic::*;
#[allow(unused_imports)] use core::fmt::Debug; // for doc

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

/// A helper macro to define events that directly return their state with no further processing.
///
/// The first argument is the event type, the second is the type of the event state, and the
/// third is the starting value of the event state.
///
/// If the third argument is omitted, it is assumed to be [`Default::default`]. If the second
/// argument is omitted, it assumed to be `()`.
///
/// # Example
///
/// ```
/// # #[macro_use] extern crate static_events;
/// # use static_events::*;
/// pub struct MyEventA(u32);
/// simple_event!(MyEventA);
///
/// pub struct MyEventB(u32);
/// simple_event!(MyEventB, u32);
///
/// pub struct MyEventC(u32);
/// simple_event!(MyEventC, u32, 42);
/// ```
#[macro_export]
#[allow_internal_unstable]
macro_rules! simple_event {
    ($ev:ty $(,)?) => {
        simple_event!($ev, (), ());
    };
    ($ev:ty, $state:ty $(,)?) => {
        simple_event!($ev, $state, Default::default());
    };
    ($ev:ty, $state:ty, $starting_val:expr $(,)?) => {
        impl $crate::Event for $ev {
            type State = $state;
            type StateArg = $state;
            type MethodRetVal = EventResult;
            type RetVal = $state;
            fn starting_state(&self, _: &impl $crate::EventDispatch) -> $state {
                $starting_val
            }
            fn borrow_state<'a>(&self, state: &'a mut $state) -> &'a mut $state {
                state
            }
            fn default_return(&self) -> EventResult {
                Default::default()
            }
            fn to_event_result(&self, _: &mut $state, result: EventResult) -> EventResult {
                result
            }
            fn to_return_value(
                &self, _: &impl $crate::EventDispatch, state: $state,
            ) -> $state {
                state
            }
        }
    };
}

/// A helper macro to define events that can fail.
///
/// The first argument is the event type, the second is the type of the event state, and the
/// third is the type of the error type, and the fourth is the starting value of the initial
/// state.
///
/// If the fourth argument is omitted, it is assumed to be [`Default::default`].
///
/// Handlers for events defined with this macro return a `Result<EventResult, E>`, and return
/// the error to the caller of the event, cancelling all further events.
///
/// # Example
///
/// ```
/// # #[macro_use] extern crate static_events;
/// # use static_events::*;
/// pub struct MyEvent(u32);
/// failable_event!(MyEvent, u32, ::std::io::Error);
/// ```
#[macro_export]
#[allow_internal_unstable]
macro_rules! failable_event {
    ($ev:ty, $state:ty, $error:ty $(,)?) => {
        failable_event!($ev, $state, $error, Default::default());
    };
    ($ev:ty, $state:ty, $error:ty, $starting_val:expr $(,)?) => {
        impl $crate::Event for $ev {
            type State = Result<$state, $error>;
            type StateArg = $state;
            type MethodRetVal = Result<EventResult, $error>;
            type RetVal = Result<$state, $error>;
            fn starting_state(&self, _: &impl $crate::EventDispatch) -> Result<$state, $error> {
                Ok($starting_val)
            }
            fn borrow_state<'a>(&self, state: &'a mut Result<$state, $error>) -> &'a mut $state {
                state.as_mut().expect("Continuing already failed event?")
            }
            fn default_return(&self) -> Result<EventResult, $error> {
                Ok(Default::default())
            }
            fn to_event_result(
                &self, state: &mut Result<$state, $error>, result: Result<EventResult, $error>,
            ) -> EventResult {
                match result {
                    Ok(result) => result,
                    Err(err) => {
                        *state = Err(err);
                        EvCancel
                    }
                }
            }
            fn to_return_value(
                &self, _: &impl $crate::EventDispatch, state: Result<$state, $error>,
            ) -> Result<$state, $error> {
                state
            }
        }
    };
}

/// A helper macro to define events that are used to perform a particular method call, and are
/// not meant to be significantly extended.
///
/// The first argument is the event type, the second is the type of the return value. If the
/// second argument is omitted, it is assumed to be `()`.
///
/// This is meant to be used with the `on_call` wrapper in [`event_handler!`].
///
/// # Example
///
/// ```
/// # #[macro_use] extern crate static_events;
/// # use static_events::*;
/// pub struct MyEvent(u32);
/// ipc_event!(MyEvent, u32);
///
/// pub struct MyVoidEvent(u32);
/// ipc_event!(MyVoidEvent);
/// ```
#[macro_export]
#[allow_internal_unstable]
macro_rules! ipc_event {
    ($ev:ty $(,)?) => {
        ipc_event!($ev, ());
    };
    ($ev:ty, $state:ty $(,)?) => {
        impl $crate::Event for $ev {
            type State = Option<$state>;
            type StateArg = Option<$state>;
            type MethodRetVal = EventResult;
            type RetVal = $state;
            fn starting_state(&self, _: &impl $crate::EventDispatch) -> Option<$state> {
                None
            }
            fn borrow_state<'a>(&self, state: &'a mut Option<$state>) -> &'a mut Option<$state> {
                state
            }
            fn default_return(&self) -> EventResult {
                Default::default()
            }
            fn to_event_result(&self, _: &mut Option<$state>, result: EventResult) -> EventResult {
                result
            }
            fn to_return_value(
                &self, _: &impl $crate::EventDispatch, state: Option<$state>,
            ) -> $state {
                state.expect(concat!("No listeners responded to ", stringify!($ev), "!"))
            }
        }
    };
}

/// The result of a stage of a [`EventHandler`] or [`EventSet`].
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub enum EventResult {
    /// Continues normally.
    EvOk,
    /// Cancels further [`EventHandler`]s from processing the current phase of the current event.
    /// (i.e. `init`, `check`, `before_event`, `on_event`, and `after_event`)
    EvCancelStage,
    /// Cancels further [`EventHandler`]s from processing the current event.
    EvCancel,
}
pub use self::EventResult::{EvOk, EvCancelStage, EvCancel};

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

macro_rules! handlers {
    ($($ev:ident)*) => {
        /// A subcomponent of a [`EventRoot`] that defines how it responds to a particular type of
        /// [`Event`].
        pub trait EventHandler<E: Event> : EventRoot {
            $(
                fn $ev(
                    &self, _: &impl EventDispatch, event: &mut E, _: &mut E::StateArg,
                ) -> E::MethodRetVal {
                    event.default_return()
                }
            )*
        }
        impl <E: Event, T: EventRoot> EventHandler<E> for T {
            $(
                default fn $ev(
                    &self, _: &impl EventDispatch, event: &mut E, _: &mut E::StateArg,
                ) -> E::MethodRetVal {
                    event.default_return()
                }
            )*
        }

        /// An [`EventSet`] that defines its response to events using individual [`EventHandler`]
        /// impls for every event it responds to.
        pub trait EventRoot: Sized { }

        /// The generic base trait used to define [`EventDispatch`]s.
        ///
        /// Each method of [`EventSet`] takes the [`EventDispatch`] the event was originally
        /// dispatched into in the `target` parameter, the event itself in the `ev` parameter,
        /// and the event's current state in the `state` parameter.
        ///
        /// The methods are called in the following order:<br>
        /// `init` -> `check` -> `before_event` -> `on_event` -> `after_event`
        pub trait EventSet: Sized {
            $(
                fn $ev<E: Event>(
                    &self, target: &impl EventDispatch, ev: &mut E, state: &mut E::State,
                ) -> EventResult;
            )*
        }
        impl <T: EventRoot> EventSet for T {
            $(
                fn $ev<E: Event>(
                    &self, target: &impl EventDispatch, ev: &mut E, state: &mut E::State,
                ) -> EventResult {
                    let result = {
                        let state_arg = ev.borrow_state(state);
                        EventHandler::$ev(self, target, ev, state_arg)
                    };
                    ev.to_event_result(state, result)
                }
            )*
        }
    };
}
handlers!(init check before_event on_event after_event);

#[macro_export]
#[doc(hidden)]
macro_rules! event_handler_internal {
    (@verify_name init $($rest:tt)*) => { $($rest)* };
    (@verify_name check $($rest:tt)*) => { $($rest)* };
    (@verify_name before_event $($rest:tt)*) => { $($rest)* };
    (@verify_name on_event $($rest:tt)*) => { $($rest)* };
    (@verify_name after_event $($rest:tt)*) => { $($rest)* };
    (@verify_name $unknown:ident $($rest:tt)*) => {
        compile_error!(concat!("Unknown event handler stage '", stringify!($unknown), "'."))
    };

    ($ty:ty, $event:ty, on_call: |$target:pat, $ev:pat,| $ev_func:expr) => {
        fn on_event(
            &self, target: &impl $crate::EventDispatch,
                   ev: &mut $event,
                   state: &mut Option<<$event as $crate::Event>::RetVal>,
        ) -> <$event as $crate::Event>::MethodRetVal {
            use $crate::{Event as __static_events__generated_Event,
                         EventDispatch as __static_events__generated_EventDispatch};
            trait SelfHack {
                fn callback(
                    &self, _: &impl $crate::EventDispatch,
                           _: &mut $event,
                ) -> <$event as $crate::Event>::RetVal;
            }
            unhygienic_item! {
                impl SelfHack for $ty {
                    fn callback(
                        &self, $target: &impl __static_events__generated_EventDispatch,
                               $ev: &mut $event,
                    ) -> <$event as __static_events__generated_Event>::RetVal {
                        $ev_func
                    }
                }
            }
            if state.is_some() {
                panic!(concat!("Duplicate listeners responding to '", stringify!($event), "'."));
            }
            let call_result = <Self as SelfHack>::callback(self, target, ev);
            *state = Some(call_result);
            Default::default()
        }
    };
    ($ty:ty, $event:ty, $call_name:ident: |$target:pat, $ev:pat, $state:pat,| $ev_func:expr) => {
        event_handler_internal!(@verify_name $call_name);
        fn $call_name(
            &self, target: &impl $crate::EventDispatch,
                   ev: &mut $event,
                   state: &mut <$event as $crate::Event>::StateArg,
        ) -> <$event as $crate::Event>::MethodRetVal {
            use $crate::{Event as __static_events__generated_Event,
                         EventDispatch as __static_events__generated_EventDispatch};
            trait SelfHack {
                fn callback(
                    &self, _: &impl $crate::EventDispatch,
                           _: &mut $event,
                           _: &mut <$event as $crate::Event>::StateArg,
                ) -> <$event as $crate::Event>::MethodRetVal;
            }
            unhygienic_item! {
                impl SelfHack for $ty {
                    fn callback(
                        &self, $target: &impl __static_events__generated_EventDispatch,
                               $ev: &mut $event,
                               $state: &mut <$event as __static_events__generated_Event>::StateArg,
                    ) -> <$event as __static_events__generated_Event>::MethodRetVal {
                        $ev_func.into()
                    }
                }
            }
            <Self as SelfHack>::callback(self, target, ev, state)
        }
    };
    ($ty:ty, $event:ty, $call_name:ident: |$($bind:pat,)*| $ev_func:expr) => {
        event_handler_internal!(@verify_name $call_name
            compile_error!(concat!("Wrong number of parameters for event handler stage '",
                                   stringify!($call_name), "'."))
        );
    };
}

/// A helper macro to declare [`EventHandler`]s with simpler syntax.
///
/// # Syntax
///
/// The first parameter is the type to generate [`EventHandler`] impls for. This is followed by
/// a comma delimited list of [`Event`] types and the body containing the handlers for that event,
/// separated by colons, similar to struct syntax:
///
/// ```
/// # #![feature(specialization)]
/// # #[macro_use] extern crate static_events;
/// # use static_events::*;
/// # struct EventA; simple_event!(EventA);
/// # struct EventB; simple_event!(EventB);
/// # struct EventC; simple_event!(EventC);
/// # struct MyEventHandler; impl EventRoot for MyEventHandler { }
/// event_handler!(MyEventHandler,
///     EventA: { /* body */ },
///     EventB: { /* body */ },
///     EventC: { /* body */ },
/// );
/// ```
///
/// The body of the handler for each type also follows struct syntax. Each record has the
/// name of a method in [`EventHandler`] followed by a closure. It takes the same parameters
/// as the corresponding method of [`EventHandler`]. The return value of this closure can be
/// any type that implements `Into<E::MethodRetVal>`:
/// ```
/// # #![feature(specialization)]
/// # #[macro_use] extern crate static_events;
/// # use static_events::*;
/// # struct Event; simple_event!(Event);
/// # struct MyEventHandler; impl EventRoot for MyEventHandler { }
/// event_handler!(MyEventHandler,
///     Event: {
///         init        : |_, _, _| println!("init"),
///         check       : |_, _, _| println!("check"),
///         before_event: |_, _, _| println!("before_event"),
///         on_event    : |_, _, _| println!("on_event"),
///         after_event : |_, _, _| println!("after_event"),
///     },
/// );
/// ```
///
/// In addition, `on_call` may be used for events defined by [`ipc_event!`]. The closure takes
/// the first two parameters of `on_event`, but does not receive the state argument. It must
/// return a value of the type specified in the [`ipc_event!`] invocation. Note that this
/// generates a [`EventHandler::on_event`] function, and cannot be used with `on_event`:
/// ```
/// # #![feature(specialization)]
/// # #[macro_use] extern crate static_events;
/// # use static_events::*;
/// # struct IpcEvent; ipc_event!(IpcEvent);
/// # struct MyEventHandler; impl EventRoot for MyEventHandler { }
/// event_handler!(MyEventHandler,
///     IpcEvent: {
///         on_call: |_, _| println!("on_call"),
///     },
/// );
/// ```
///
/// # Examples
///
/// With regular events:
///
/// ```
/// # #![feature(specialization)]
/// # #[macro_use] extern crate static_events;
/// # use static_events::*;
/// struct MyEvent(u32);
/// simple_event!(MyEvent, u32, 0);
///
/// struct MyEventHandler;
/// impl EventRoot for MyEventHandler { }
/// event_handler!(MyEventHandler, MyEvent: {
///     on_event: |_, ev, i| { *i += ev.0 }
/// });
///
/// assert_eq!(MyEventHandler.dispatch(MyEvent(42)), 42);
/// ```
///
/// With IPC events:
///
/// ```
/// # #![feature(specialization)]
/// # #[macro_use] extern crate static_events;
/// # use static_events::*;
/// struct MyEvent(u32);
/// ipc_event!(MyEvent, u32);
///
/// struct MyEventHandler;
/// impl EventRoot for MyEventHandler { }
/// event_handler!(MyEventHandler, MyEvent: {
///     on_call: |_, ev| ev.0 * ev.0
/// });
///
/// assert_eq!(MyEventHandler.dispatch(MyEvent(12)), 144);
/// ```
#[macro_export]
#[allow_internal_unstable]
macro_rules! event_handler {
    (
        $name:ty, $(
            $event:ty: {
                $($call_name:ident: |$($bind:pat),* $(,)?| $ev_func:expr),*
                $(,)?
            }
        ),*
        $(,)?
    ) => {$(
        impl $crate::EventHandler<$event> for $name {
            $( event_handler_internal!($name, $event, $call_name: |$($bind,)*| $ev_func); )*
        }
    )*};
}

/// A helper macro to declare stateless [`EventSet`]s with simpler syntax.
///
/// This uses the same syntax as [`event_handler!`], except a visibility modifier or attributes
/// can be placed in front. It automatically declares an empty struct and [`Copy`], [`Clone`],
/// [`Debug`], [`Default`], and [`EventRoot`] impls for it.
///
/// # Example
///
/// ```
/// # #![feature(specialization)]
/// # #[macro_use] extern crate static_events;
/// # use static_events::*;
/// pub struct MyEvent(u32);
/// simple_event!(MyEvent, u32, 0);
///
/// simple_event_handler!(
///     /// An event handler
///     pub MyEventHandler, MyEvent: {
///         on_event: |_, ev, i| { *i += ev.0 }
///     }
/// );
///
/// assert_eq!(MyEventHandler.dispatch(MyEvent(42)), 42);
/// ```
#[macro_export]
#[allow_internal_unstable]
macro_rules! simple_event_handler {
    (
        $(#[$meta:meta])* $vis:vis $name:ident, $(
            $event:ty: {
                $($call_name:ident: |$($bind:pat),* $(,)?| $ev_func:expr),*
                $(,)?
            }
        ),*
        $(,)?
    ) => {
        #[derive(Copy, Clone, Debug, Default)]
        $(#[$meta])*
        $vis struct $name;
        impl $crate::EventRoot for $name { }
        event_handler!($name, $(
            $event: {$($call_name: |$($bind,)*| $ev_func,)*},
        )*);
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! merged_eventset_handler {
    ($ev:ident, $($field_name:ident)*) => {
        fn $ev<E: $crate::Event>(
            &self, target: &impl $crate::EventDispatch, ev: &mut E, state: &mut E::State,
        ) -> $crate::EventResult {
            $(
                match $crate::EventSet::$ev(&self.$field_name, target, ev, state) {
                    $crate::EvOk => { }
                    e => return e,
                }
            )*
            $crate::EvOk
        }
    }
}

/// Creates an [`EventSet`] implementation for a struct by merging several [`EventSet`]s. Each
/// field of the struct must implement [`EventSet`].
///
/// The individual handlers will be called in the order that the fields are declared in.
///
/// At some point, this will be replaced with a procedural derive, i.e. `#[derive(EventSet)]`.
#[macro_export]
#[allow_internal_unstable]
macro_rules! merged_eventset {
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
        impl $(<$($ty_param $(: $ty_bound)?,)*>)? $crate::EventSet for $name $(<$($ty_param)*>)? {
            merged_eventset_handler!(init        , $($field_name)*);
            merged_eventset_handler!(check       , $($field_name)*);
            merged_eventset_handler!(before_event, $($field_name)*);
            merged_eventset_handler!(on_event    , $($field_name)*);
            merged_eventset_handler!(after_event , $($field_name)*);
        }
    )*}
}

/// A handler that receives [`Event`]s and dispatches it to various handlers. This is not meant to
/// be defined directly, and instead [`EventDispatch`]s should be defined through [`EventRoot`],
/// or [`merged_eventset!`].
pub trait EventDispatch {
    /// Dispatches an event and returns its result.
    fn dispatch<E: Event>(&self, _: E) -> E::RetVal;
}
impl <T: EventSet> EventDispatch for T {
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
