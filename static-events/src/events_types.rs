// TODO: Document `[T: Bounds]` syntax.

// for documentation
#[allow(unused_imports)] use crate::*;

/// A helper macro to define events that directly return their state with no further processing.
///
/// The first argument is the event type, the second is the type of the event state, and the
/// third is the starting value of the event state.
///
/// If the third argument is omitted, it is assumed to be [`Default::default`]. If the second
/// argument is omitted, it assumed to be `()`.
///
/// Event handlers for events defined using this macro should return either an [`EventResult`]
/// or `()`.
///
/// # Example
///
/// Declaration:
///
/// ```
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
/// # use static_events::*;
/// # pub struct MyEventB(u32); simple_event!(MyEventB, u32);
/// struct MyEventHandler;
/// #[event_dispatch]
/// impl MyEventHandler {
///     #[event_handler]
///     fn handle_event(ev: &MyEventB, state: &mut u32) {
///         *state = ev.0 * ev.0;
///     }
/// }
/// assert_eq!(MyEventHandler.dispatch(MyEventB(12)), 144);
/// ```
#[macro_export]
macro_rules! simple_event {
    ([$($bounds:tt)*] $ev:ty $(,)?) => {
        simple_event!([$($bounds)*] $ev, (), ());
    };
    ([$($bounds:tt)*] $ev:ty, $state:ty $(,)?) => {
        simple_event!([$($bounds)*] $ev, $state, Default::default());
    };
    ([$($bounds:tt)*] $ev:ty, $state:ty, $starting_val:expr $(,)?) => {
        impl <$($bounds)*> $crate::events::SimpleEvent for $ev {
            type State = $state;
            fn starting_state(&self, _: &impl $crate::EventDispatch) -> $state {
                $starting_val
            }
        }
    };
    ($ev:ty $(,)?) => {
        simple_event!([] $ev);
    };
    ($ev:ty, $state:ty $(,)?) => {
        simple_event!([] $ev, $state);
    };
    ($ev:ty, $state:ty, $starting_val:expr $(,)?) => {
        simple_event!([] $ev, $state, $starting_val);
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
/// Handlers for events defined with this macro return a `Result<EventResult, E>` or a
/// `Result<(), E>`, and return errors to the caller of the event, cancelling all further handlers.
///
/// # Example
///
/// Declaration:
///
/// ```
/// # use static_events::*; use std::io;
/// pub struct MyEvent(u32);
/// failable_event!(MyEvent, u32, io::Error);
/// ```
///
/// Usage:
///
/// ```
/// # use static_events::*; use std::io;
/// # pub struct MyEvent(u32); failable_event!(MyEvent, u32, ::std::io::Error);
/// struct MyEventHandler;
/// #[event_dispatch]
/// impl MyEventHandler {
///     #[event_handler]
///     fn handle_event(ev: &MyEvent, state: &mut u32) -> io::Result<()> {
///         if ev.0 > 50 { Err(io::Error::new(io::ErrorKind::Other, "too large!"))? }
///         *state = ev.0 * ev.0;
///         Ok(())
///     }
/// }
/// assert_eq!(MyEventHandler.dispatch(MyEvent(12)).ok(), Some(144));
/// assert!(MyEventHandler.dispatch(MyEvent(100)).is_err());
/// ```
#[macro_export]
macro_rules! failable_event {
    ([$($bounds:tt)*] $ev:ty, $state:ty, $error:ty $(,)?) => {
        failable_event!([$($bounds)*] $ev, $state, $error, Default::default());
    };
    ([$($bounds:tt)*] $ev:ty, $state:ty, $error:ty, $starting_val:expr $(,)?) => {
        impl <$($bounds)*> $crate::Event for $ev {
            type State = $crate::private::Result<$state, $error>;
            type StateArg = $state;
            type MethodRetVal = $crate::private::FailableReturn<$error>;
            type RetVal = $crate::private::Result<$state, $error>;
            fn starting_state(
                &self, _: &impl $crate::EventDispatch,
            ) -> $crate::private::Result<$state, $error> {
                Ok($starting_val)
            }
            fn borrow_state<'a>(
                &self, state: &'a mut $crate::private::Result<$state, $error>,
            ) -> &'a mut $state {
                state.as_mut().expect("Continuing already failed event?")
            }
            fn to_event_result(
                &self, state: &mut $crate::private::Result<$state, $error>,
                result: $crate::private::FailableReturn<$error>,
            ) -> $crate::EventResult {
                match result.0 {
                    Ok(result) => result,
                    Err(err) => {
                        *state = Err(err);
                        $crate::EvCancel
                    }
                }
            }
            fn to_return_value(
                &self, _: &impl $crate::EventDispatch, 
                state: $crate::private::Result<$state, $error>,
            ) -> $crate::private::Result<$state, $error> {
                state
            }
        }
    };
    ($ev:ty, $state:ty, $error:ty $(,)?) => {
        failable_event!([] $ev, $state, $error, Default::default());
    };
    ($ev:ty, $state:ty, $error:ty, $starting_val:expr $(,)?) => {
        failable_event!([] $ev, $state, $error, $starting_val);
    };
}
