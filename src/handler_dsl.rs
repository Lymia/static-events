use interface::*;

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

        /// An [`RawEventDispatch`] that defines its response to events using individual
        /// [`EventHandler`] impls for every event it responds to.
        pub trait EventRoot: Sized { }

        impl <T: EventRoot> RawEventDispatch for T {
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

/// A helper macro to declare stateless [`RawEventDispatch`]s with simpler syntax.
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