//! This module is used by static-events_derive, and is not stable API.

use crate::events::*;
use crate::handlers::*;
use std::future::Future;
use std::pin::Pin;
use std::task::{Poll, Waker};

#[doc(hidden)] pub use futures::executor::block_on;
#[doc(hidden)] pub use std::result::Result;

enum NullFuture { }
impl Future for NullFuture {
    type Output = EventResult;
    fn poll(self: Pin<&mut Self>, _: &Waker) -> Poll<Self::Output> {
        unreachable!()
    }
}
trait UniversalEventHandler<E: Events, Ev: Event, P: EventPhase, D = DefaultHandler>: Events {
    const IS_IMPLEMENTED: bool;

    fn on_phase(
        &self, target: &Handler<E>, ev: &mut Ev, state: &mut Ev::State,
    ) -> EventResult;

    type FutureType: Future<Output = EventResult>;
    unsafe fn on_phase_async(ctx: AsyncDispatchContext<Self, E, Ev>) -> Self::FutureType;
}
impl <E: Events, Ev: Event, P: EventPhase, D, T: Events> UniversalEventHandler<E, Ev, P, D> for T {
    default const IS_IMPLEMENTED: bool = false;

    default fn on_phase(
        &self, _: &Handler<E>, _: &mut Ev, _: &mut Ev::State,
    ) -> EventResult {
        unreachable!()
    }

    default type FutureType = NullFuture;
    default unsafe fn on_phase_async(_: AsyncDispatchContext<Self, E, Ev>) -> Self::FutureType {
        unreachable!()
    }
}
impl <
    E: Events, Ev: Event, P: EventPhase, D, T: Events + EventHandler<E, Ev, P, D>,
> UniversalEventHandler<E, Ev, P, D> for T {
    const IS_IMPLEMENTED: bool = true;

    fn on_phase(
        &self, target: &Handler<E>, ev: &mut Ev, state: &mut Ev::State,
    ) -> EventResult {
        self.on_phase(target, ev, state)
    }

    type FutureType = <Self as EventHandler<E, Ev, P, D>>::FutureType;
    unsafe fn on_phase_async(ctx: AsyncDispatchContext<Self, E, Ev>) -> Self::FutureType {
        <Self as EventHandler<E, Ev, P, D>>::on_phase_async(ctx)
    }
}

#[inline(always)]
pub const fn is_implemented<T: Events, E: Events, Ev: Event, P: EventPhase, D>() -> bool {
    <T as UniversalEventHandler<E, Ev, P, D>>::IS_IMPLEMENTED
}

#[inline(always)]
pub fn on_phase<T: Events, E: Events, Ev: Event, P: EventPhase, D>(
    this: &T, target: &Handler<E>, ev: &mut Ev, state: &mut Ev::State,
) -> EventResult {
    if is_implemented::<T, E, Ev, P, D>() {
        UniversalEventHandler::<E, Ev, P, D>::on_phase(this, target, ev, state)
    } else {
        EventResult::EvOk
    }
}

#[inline(always)]
pub async fn on_phase_async<'a, T: Events, E: Events, Ev: Event, P: EventPhase, D>(
    this: &'a T, target: &'a Handler<E>, ev: &'a mut Ev, state: &'a mut Ev::State,
) -> EventResult {
    if is_implemented::<T, E, Ev, P, D>() {
        let async_data = AsyncDispatchContext { this, target, ev, state };
        let future = unsafe {
            <T as UniversalEventHandler<E, Ev, P, D>>::on_phase_async(async_data)
        };
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
