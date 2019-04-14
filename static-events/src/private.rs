//! This module is used by static-events_derive, and is not stable API.

use crate::events::*;
use crate::handlers::*;
use std::future::Future;
use std::pin::Pin;
use std::task::{Poll, Context};

#[doc(hidden)] pub use futures::executor::block_on;
#[doc(hidden)] pub use std::result::Result;

enum NullFuture { }
impl Future for NullFuture {
    type Output = EventResult;
    fn poll(self: Pin<&mut Self>, _: &mut Context) -> Poll<Self::Output> {
        unreachable!()
    }
}
trait UniversalEventHandler<
    'a, E: Events, Ev: Event + 'a, P: EventPhase, D = DefaultHandler,
>: Events {
    const IS_IMPLEMENTED: bool;
    fn on_phase(
        &'a self, target: &'a Handler<E>, ev: &'a mut Ev, state: &'a mut Ev::State,
    ) -> EventResult;

    type FutureType: Future<Output = EventResult>;
    fn on_phase_async(
        &'a self, target: &'a Handler<E>, ev: &'a mut Ev, state: &'a mut Ev::State,
    ) -> Self::FutureType;
}
impl <
    'a, E: Events, Ev: Event + 'a, P: EventPhase, D, T: Events,
> UniversalEventHandler<'a, E, Ev, P, D> for T {
    default const IS_IMPLEMENTED: bool = false;

    default fn on_phase(
        &'a self, _: &'a Handler<E>, _: &'a mut Ev, _: &'a mut Ev::State,
    ) -> EventResult {
        panic!("Called missing event handler! (check IS_IMPLEMENTED)")
    }

    default type FutureType = NullFuture;
    default fn on_phase_async(
        &'a self, _: &'a Handler<E>, _: &'a mut Ev, _: &'a mut Ev::State,
    ) -> Self::FutureType {
        panic!("Called missing event handler! (check IS_IMPLEMENTED)")
    }
}
impl <
    'a, E: Events, Ev: Event + 'a, P: EventPhase, D, T: Events + EventHandler<'a, E, Ev, P, D>,
> UniversalEventHandler<'a, E, Ev, P, D> for T {
    const IS_IMPLEMENTED: bool = <Self as EventHandler<'a, E, Ev, P, D>>::IS_IMPLEMENTED;

    #[inline(always)]
    fn on_phase(
        &'a self, target: &'a Handler<E>, ev: &'a mut Ev, state: &'a mut Ev::State,
    ) -> EventResult {
        self.on_phase(target, ev, state)
    }

    type FutureType = <Self as EventHandler<'a, E, Ev, P, D>>::FutureType;

    #[inline(always)]
    fn on_phase_async(
        &'a self, target: &'a Handler<E>, ev: &'a mut Ev, state: &'a mut Ev::State,
    ) -> Self::FutureType {
        <Self as EventHandler<'a, E, Ev, P, D>>::on_phase_async(self, target, ev, state)
    }
}

#[inline(always)]
pub const fn is_implemented<'a, T: Events, E: Events, Ev: Event + 'a, P: EventPhase, D>() -> bool {
    <T as UniversalEventHandler<'a, E, Ev, P, D>>::IS_IMPLEMENTED
}

#[inline(always)]
pub fn on_phase<'a, T: Events, E: Events, Ev: Event + 'a, P: EventPhase, D>(
    this: &'a T, target: &'a Handler<E>, ev: &'a mut Ev, state: &'a mut Ev::State,
) -> EventResult {
    if is_implemented::<'a, T, E, Ev, P, D>() {
        UniversalEventHandler::<'a, E, Ev, P, D>::on_phase(this, target, ev, state)
    } else {
        EventResult::EvOk
    }
}

#[inline(always)]
pub async fn on_phase_async<'a, T: Events, E: Events, Ev: Event + 'a, P: EventPhase, D>(
    this: &'a T, target: &'a Handler<E>, ev: &'a mut Ev, state: &'a mut Ev::State,
) -> EventResult {
    if is_implemented::<T, E, Ev, P, D>() {
        let future =
            <T as UniversalEventHandler<'a, E, Ev, P, D>>::on_phase_async(this, target, ev, state);
        await!(future)
    } else {
        EventResult::EvOk
    }
}

pub enum HandlerImplBlock { }

pub trait CheckDowncast<A> {
    fn downcast_ref(&self) -> Option<&A>;
}
impl <A> CheckDowncast<A> for A {
    default fn downcast_ref(&self) -> Option<&A> {
        Some(unsafe { &*(self as *const Self as *const A) })
    }
}
impl <A, B> CheckDowncast<B> for A {
    default fn downcast_ref(&self) -> Option<&B> {
        None
    }
}

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
