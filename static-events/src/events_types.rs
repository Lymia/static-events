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
/// use static_events::prelude_sync::*;
///
/// pub struct MyEventA(u32);
/// simple_event!(MyEventA);
///
/// pub struct MyEventB(u32);
/// simple_event!(MyEventB, u32);
///
/// pub struct MyEventC<T: Send>(T);
/// simple_event!([T: Send] MyEventC<T>, u32, 42);
/// ```
///
/// Usage:
/// ```
/// use static_events::prelude_sync::*;
///
/// # pub struct MyEventB(u32); simple_event!(MyEventB, u32);
/// #[derive(Events)]
/// struct MyEventHandler;
///
/// #[events_impl]
/// impl MyEventHandler {
///     #[event_handler]
///     fn handle_event(ev: &MyEventB, state: &mut u32) {
///         *state = ev.0 * ev.0;
///     }
/// }
///
/// let handler = Handler::new(MyEventHandler);
/// assert_eq!(handler.dispatch(MyEventB(12)), 144);
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
            fn starting_state(&self) -> $state {
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

/// A helper macro to define events that return themselves as output.
///
/// Event handlers for events defined using this macro should return either an [`EventResult`]
/// or `()`.
///
/// # Example
///
/// Declaration:
///
/// ```
/// use static_events::prelude_sync::*;
/// pub struct MyEvent(u32);
/// self_event!(MyEvent);
/// ```
///
/// Usage:
///
/// ```
/// use static_events::prelude_sync::*;
///
/// # #[derive(PartialEq, Debug)] pub struct MyEvent(u32);
/// # self_event!(MyEvent);
/// #[derive(Events)]
/// struct MyEventHandler;
///
/// #[events_impl]
/// impl MyEventHandler {
///     #[event_handler]
///     fn handle_event(ev: &mut MyEvent) {
///         ev.0 *= 10;
///     }
/// }
///
/// let handler = Handler::new(MyEventHandler);
/// assert_eq!(handler.dispatch(MyEvent(10)), MyEvent(100));
/// ```
#[macro_export]
macro_rules! self_event {
    ([$($bounds:tt)*] $ev:ty $(,)?) => {
        impl <$($bounds)*> $crate::events::SimpleInterfaceEvent for $ev {
            type State = ();
            type RetVal = $ev;
            fn starting_state(&self) { }
            fn to_return_value(self, _: ()) -> $ev {
                self
            }
        }
    };
    ($ev:ty $(,)?) => {
        self_event!([] $ev);
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
/// use static_events::prelude_sync::*;
/// # use std::io;
///
/// pub struct MyEvent(u32);
/// failable_event!(MyEvent, u32, io::Error);
///
/// pub struct MyEvent2<T: Send>(T);
/// failable_event!([T: Send] MyEvent2<T>, u32, io::Error);
/// ```
///
/// Usage:
///
/// ```
/// use static_events::prelude_sync::*;
/// # use std::io;
/// # pub struct MyEvent(u32); failable_event!(MyEvent, u32, io::Error);
///
/// #[derive(Events)]
/// struct MyEventHandler;
///
/// #[events_impl]
/// impl MyEventHandler {
///     #[event_handler]
///     fn handle_event(ev: &MyEvent, state: &mut u32) -> io::Result<()> {
///         if ev.0 > 50 { Err(io::Error::new(io::ErrorKind::Other, "too large!"))? }
///         *state = ev.0 * ev.0;
///         Ok(())
///     }
/// }
///
/// let handler = Handler::new(MyEventHandler);
/// assert_eq!(handler.dispatch(MyEvent(12)).ok(), Some(144));
/// assert!(handler.dispatch(MyEvent(100)).is_err());
/// ```
#[macro_export]
macro_rules! failable_event {
    ([$($bounds:tt)*] $ev:ty, $state:ty, $error:ty $(,)?) => {
        failable_event!([$($bounds)*] $ev, $state, $error, Default::default());
    };
    ([$($bounds:tt)*] $ev:ty, $state:ty, $error:ty, $starting_val:expr $(,)?) => {
        impl <$($bounds)*> $crate::events::Event for $ev {
            type State = $crate::private::Result<$state, $error>;
            type StateArg = $state;
            type MethodRetVal = $crate::private::FailableReturn<$error>;
            type RetVal = $crate::private::Result<$state, $error>;
            fn starting_state(&self) -> $crate::private::Result<$state, $error> {
                Ok($starting_val)
            }
            fn borrow_state<'__state>(
                &self, state: &'__state mut $crate::private::Result<$state, $error>,
            ) -> &'__state mut $state {
                state.as_mut().expect("Continuing already failed event?")
            }
            fn to_event_result(
                &self, state: &mut $crate::private::Result<$state, $error>,
                result: $crate::private::FailableReturn<$error>,
            ) -> $crate::events::EventResult {
                match result.0 {
                    Ok(result) => result,
                    Err(err) => {
                        *state = Err(err);
                        $crate::events::EventResult::EvCancel
                    }
                }
            }
            fn to_return_value(
                self, state: $crate::private::Result<$state, $error>,
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

/// A helper macro to define events that return themselves as output and can fail.
///
/// The first argument is the event type, the second is the type of error type.
///
/// Event handlers for events defined using this macro should return either a
/// `Result<EventResult>` or `Result<()>`.
///
/// # Example
///
/// Declaration:
///
/// ```
/// # use std::io;
/// use static_events::prelude_sync::*;
/// pub struct MyEvent(u32);
/// failable_self_event!(MyEvent, io::Error);
/// ```
///
/// Usage:
///
/// ```
/// use static_events::prelude_sync::*;
///
/// # use std::io;
/// # #[derive(PartialEq, Debug)] pub struct MyEvent(u32);
/// # failable_self_event!(MyEvent, io::Error);
/// #[derive(Events)]
/// struct MyEventHandler;
///
/// #[events_impl]
/// impl MyEventHandler {
///     #[event_handler]
///     fn handle_event(ev: &mut MyEvent) -> io::Result<()> {
///         if ev.0 > 50 { Err(io::Error::new(io::ErrorKind::Other, "too large!"))? }
///         ev.0 *= ev.0;
///         Ok(())
///     }
/// }
///
/// let handler = Handler::new(MyEventHandler);
/// assert_eq!(handler.dispatch(MyEvent(12)).ok(), Some(MyEvent(144)));
/// assert!(handler.dispatch(MyEvent(100)).is_err());
/// ```
#[macro_export]
macro_rules! failable_self_event {
    ([$($bounds:tt)*] $ev:ty, $error:ty $(,)?) => {
        impl <$($bounds)*> $crate::events::Event for $ev {
            type State = ((), $crate::private::Option<$error>);
            type StateArg = ();
            type MethodRetVal = $crate::private::FailableReturn<$error>;
            type RetVal = $crate::private::Result<$ev, $error>;
            fn starting_state(&self) -> ((), $crate::private::Option<$error>) {
                ((), $crate::private::None)
            }
            fn borrow_state<'__state>(
                &self, state: &'__state mut ((), $crate::private::Option<$error>),
            ) -> &'__state mut () {
                &mut state.0
            }
            fn to_event_result(
                &self, state: &mut ((), $crate::private::Option<$error>),
                result: $crate::private::FailableReturn<$error>,
            ) -> $crate::events::EventResult {
                match result.0 {
                    Ok(result) => result,
                    Err(err) => {
                        state.1 = $crate::private::Some(err);
                        $crate::events::EventResult::EvCancel
                    }
                }
            }
            fn to_return_value(
                self, state: ((), $crate::private::Option<$error>),
            ) -> $crate::private::Result<$ev, $error> {
                match state.1 {
                    $crate::private::Some(v) => Err(v),
                    $crate::private::None => Ok(self),
                }
            }
        }
    };
    ($ev:ty, $error:ty $(,)?) => {
        failable_self_event!([] $ev, $error);
    };
}
