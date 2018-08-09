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
/// Declaration:
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
///
/// Usage:
/// ```
/// # #![feature(specialization)]
/// # #[macro_use] extern crate static_events;
/// # use static_events::*;
/// # pub struct MyEventB(u32); simple_event!(MyEventB, u32);
/// struct MyEventHandler;
/// impl RootEventDispatch for MyEventHandler { }
/// impl EventHandler<MyEventB> for MyEventHandler {
///     fn on_event(&self, _: &impl EventDispatch, ev: &mut MyEventB, state: &mut u32) -> EventResult {
///         *state = ev.0 * ev.0;
///         EvOk
///     }
/// }
/// assert_eq!(MyEventHandler.dispatch(MyEventB(12)), 144);
/// ```
#[macro_export]
#[allow_internal_unstable]
macro_rules! simple_event {
    ($(<$($bounds:tt)*>)? $ev:ty $(,)?) => {
        simple_event!($(<$($bounds)*>)? $ev, (), ());
    };
    ($(<$($bounds:tt)*>)? $ev:ty, $state:ty $(,)?) => {
        simple_event!($(<$($bounds)*>)? $ev, $state, Default::default());
    };
    ($(<$($bounds:tt)*>)? $ev:ty, $state:ty, $starting_val:expr $(,)?) => {
        impl $(<$($bounds)*>)? $crate::SimpleEvent for $ev {
            type State = $state;
            fn starting_state(&self, _: &impl $crate::EventDispatch) -> $state {
                $starting_val
            }
        }
    };
}

#[doc(hidden)] pub use core::result::{Result as __StaticEvents_Macro_Result};

/// A helper macro to define events that can fail.
///
/// The first argument is the event type, the second is the type of the event state, and the
/// third is the type of the error type, and the fourth is the starting value of the initial
/// state.
///
/// If the fourth argument is omitted, it is assumed to be [`Default::default`].
///
/// Handlers for events defined with this macro return a `Result<EventResult, E>`, and return
/// errors to the caller of the event, cancelling all further handlers.
///
/// # Example
///
/// Declaration:
///
/// ```
/// # #[macro_use] extern crate static_events;
/// # use static_events::*; use std::io;
/// pub struct MyEvent(u32);
/// failable_event!(MyEvent, u32, io::Error);
/// ```
///
/// Usage:
///
/// ```
/// # #![feature(specialization)]
/// # #[macro_use] extern crate static_events;
/// # use static_events::*; use std::io;
/// # pub struct MyEvent(u32); failable_event!(MyEvent, u32, ::std::io::Error);
/// struct MyEventHandler;
/// impl RootEventDispatch for MyEventHandler { }
/// impl EventHandler<MyEvent> for MyEventHandler {
///     fn on_event(
///         &self, _: &impl EventDispatch, ev: &mut MyEvent, state: &mut u32,
///     ) -> io::Result<EventResult> {
///         if ev.0 > 50 { Err(io::Error::new(io::ErrorKind::Other, "too large!"))? }
///         *state = ev.0 * ev.0;
///         Ok(EvOk)
///     }
/// }
/// assert_eq!(MyEventHandler.dispatch(MyEvent(12)).ok(), Some(144));
/// assert!(MyEventHandler.dispatch(MyEvent(100)).is_err());
/// ```
#[macro_export]
#[allow_internal_unstable]
macro_rules! failable_event {
    ($(<$($bounds:tt)*>)? $ev:ty, $state:ty, $error:ty $(,)?) => {
        failable_event!($(<$($bounds)*>)? $ev, $state, $error, Default::default());
    };
    ($(<$($bounds:tt)*>)? $ev:ty, $state:ty, $error:ty, $starting_val:expr $(,)?) => {
        impl $(<$($bounds)*>)? $crate::Event for $ev {
            type State = $crate::__StaticEvents_Macro_Result<$state, $error>;
            type StateArg = $state;
            type MethodRetVal = $crate::__StaticEvents_Macro_Result<EventResult, $error>;
            type RetVal = $crate::__StaticEvents_Macro_Result<$state, $error>;
            fn starting_state(
                &self, _: &impl $crate::EventDispatch,
            ) -> $crate::__StaticEvents_Macro_Result<$state, $error> {
                Ok($starting_val)
            }
            fn borrow_state<'a>(
                &self, state: &'a mut $crate::__StaticEvents_Macro_Result<$state, $error>,
            ) -> &'a mut $state {
                state.as_mut().expect("Continuing already failed event?")
            }
            fn default_return(&self) -> $crate::__StaticEvents_Macro_Result<EventResult, $error> {
                Ok(Default::default())
            }
            fn to_event_result(
                &self, state: &mut $crate::__StaticEvents_Macro_Result<$state, $error>,
                 result: $crate::__StaticEvents_Macro_Result<EventResult, $error>,
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
                &self, _: &impl $crate::EventDispatch, 
                state: $crate::__StaticEvents_Macro_Result<$state, $error>,
            ) -> $crate::__StaticEvents_Macro_Result<$state, $error> {
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
/// This is meant to be used with [`IpcEventHandler`].
///
/// # Example
///
/// Declaration:
///
/// ```
/// # #![feature(specialization)]
/// # #[macro_use] extern crate static_events;
/// # use static_events::*;
/// struct MyEvent(u32);
/// ipc_event!(MyEvent, u32);
///
/// struct VoidEvent();
/// ipc_event!(VoidEvent);
/// ```
///
/// Usage:
///
/// ```
/// # #![feature(specialization)]
/// # #[macro_use] extern crate static_events;
/// # use static_events::*;
/// # struct MyEvent(u32); ipc_event!(MyEvent, u32);
/// struct MyEventHandler;
/// impl RootEventDispatch for MyEventHandler { }
/// impl IpcEventHandler<MyEvent> for MyEventHandler {
///     fn on_call(&self, _: &impl EventDispatch, ev: &mut MyEvent) -> u32 {
///         ev.0 * ev.0
///     }
/// }
/// assert_eq!(MyEventHandler.dispatch(MyEvent(12)), 144);
/// ```
#[macro_export]
#[allow_internal_unstable]
macro_rules! ipc_event {
    ($(<$($bounds:tt)*>)? $ev:ty $(,)?) => {
        ipc_event!($(<$($bounds)*>)? $ev, ());
    };
    ($(<$($bounds:tt)*>)? $ev:ty, $state:ty $(,)?) => {
        impl $(<$($bounds)*>)? $crate::SimpleInterfaceEvent for $ev {
            type State = Option<$state>;
            type RetVal = $state;
            fn starting_state(&self, _: &impl $crate::EventDispatch) -> Option<$state> {
                None
            }
            fn to_return_value(
                &self, _: &impl $crate::EventDispatch, state: Option<$state>,
            ) -> $state {
                state.expect(concat!("No listeners responded to ", stringify!($ev), "!"))
            }
        }
    };
}