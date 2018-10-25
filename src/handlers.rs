use crate::interface::*;

macro_rules! handlers {
    (
        regular: ($($ev:ident)*)
        no_main: ($($no_main_ev:ident)*)
    ) => {
        /// The main trait used to define [`RootEventDispatch`]s.
        ///
        /// Each explicit implementation of this trait defines how one reacts to a particular
        /// type of event.
        ///
        /// Only one of [`EventHandler`] or [`IpcEventHandler`] may be implemented,
        pub trait EventHandler<E: Event> : RootEventDispatch {$(
            fn $ev(
                &self, _: &impl EventDispatch, event: &mut E, _: &mut E::StateArg,
            ) -> E::MethodRetVal {
                event.default_return()
            }
        )*}

        pub trait IpcEventMarker : Event { }
        impl <
            S, E: Event<State = Option<S>, StateArg = Option<S>, RetVal = S>,
        > IpcEventMarker for E { }

        /// A trait meant to be used with [`RootEventDispatch`] for IPC events.
        ///
        /// Only one of [`EventHandler`] or [`IpcEventHandler`] may be implemented,
        pub trait IpcEventHandler<E: Event + IpcEventMarker> : RootEventDispatch {
            $(
                fn $no_main_ev(
                    &self, _: &impl EventDispatch, event: &mut E, _: &mut E::StateArg,
                ) -> E::MethodRetVal {
                    event.default_return()
                }
            )*
            fn on_call(&self, _: &impl EventDispatch, _: &mut E) -> E::RetVal;
        }
        impl <
            S, E: Event<State = Option<S>, StateArg = Option<S>, RetVal = S>,
            T: IpcEventHandler<E>,
        > EventHandler<E> for T {
            $(
                fn $no_main_ev(
                    &self, target: &impl EventDispatch, event: &mut E, state: &mut E::StateArg,
                ) -> E::MethodRetVal {
                    IpcEventHandler::$no_main_ev(self, target, event, state)
                }
            )*
            fn on_event(
                &self, target: &impl EventDispatch, event: &mut E, state: &mut E::StateArg,
            ) -> E::MethodRetVal {
                assert!(state.is_none(), "Duplicate listeners responding to event!");
                let result = IpcEventHandler::on_call(self, target, event);
                *state = Some(result);
                event.default_return()
            }
        }

        /// A trait implements [`EventDispatch`] using [`EventHandler`]s.
        pub trait RootEventDispatch: 'static { }

        trait UniversalEventHandler<E: Event> {$(
            fn $ev(
                &self, _: &impl EventDispatch, event: &mut E, _: &mut E::StateArg,
            ) -> E::MethodRetVal;
        )*}
        impl <E: Event, T> UniversalEventHandler<E> for T {$(
            default fn $ev(
                &self, _: &impl EventDispatch, event: &mut E, _: &mut E::StateArg,
            ) -> E::MethodRetVal {
                event.default_return()
            }
        )*}
        impl <E: Event, T: EventHandler<E>> UniversalEventHandler<E> for T {$(
            default fn $ev(
                &self, target: &impl EventDispatch, event: &mut E, state: &mut E::StateArg,
            ) -> E::MethodRetVal {
                EventHandler::$ev(self, target, event, state)
            }
        )*}

        impl <T: RootEventDispatch> RawEventDispatch for T {$(
            default fn $ev<E: Event>(
                &self, target: &impl EventDispatch, ev: &mut E, state: &mut E::State,
            ) -> EventResult {
                let result = {
                    let state_arg = ev.borrow_state(state);
                    UniversalEventHandler::$ev(self, target, ev, state_arg)
                };
                ev.to_event_result(state, result)
            }
        )*}
    };
}
handlers!(
    regular: (init check before_event on_event after_event)
    no_main: (init check before_event after_event)
);
