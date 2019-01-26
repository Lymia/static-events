//! This module is used by static-events_derive, and is not stable API.

use crate::events::*;
use crate::handlers::*;

pub trait EventHandler<E: Event, P: EventPhase> {
    fn on_phase(
        &self, _: &impl EventDispatch, event: &mut E, _: &mut E::StateArg,
    ) -> E::MethodRetVal;
}

pub trait UniversalEventHandler<E: Event, P: EventPhase> {
    fn on_phase(
        &self, _: &impl EventDispatch, event: &mut E, _: &mut E::StateArg,
    ) -> E::MethodRetVal;
}
impl <E: Event, P: EventPhase, T> UniversalEventHandler<E, P> for T {
    default fn on_phase(
        &self, _: &impl EventDispatch, event: &mut E, _: &mut E::StateArg,
    ) -> E::MethodRetVal {
        event.default_return()
    }
}
impl <E: Event, P: EventPhase, T: EventHandler<E, P>> UniversalEventHandler<E, P> for T {
    default fn on_phase(
        &self, target: &impl EventDispatch, event: &mut E, state: &mut E::StateArg,
    ) -> E::MethodRetVal {
        EventHandler::on_phase(self, target, event, state)
    }
}

#[doc(hidden)] pub use core::result::Result;

pub struct FailableReturn<E>(pub Result<EventResult, E>);
impl <E> Default for FailableReturn<E> {
    fn default() -> Self {
        FailableReturn(Ok(EventResult::EvOk))
    }
}
impl <E> From<Result<(), E>> for FailableReturn<E> {
    fn from(res: Result<(), E>) -> Self {
        FailableReturn(res.map(|_| EventResult::EvOk))
    }
}
impl <E> From<Result<EventResult, E>> for FailableReturn<E> {
    fn from(res: Result<EventResult, E>) -> Self {
        FailableReturn(res)
    }
}
