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