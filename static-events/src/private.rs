//! This module is used by static-events_derive, and is not stable API.

use crate::events::*;
use crate::handlers::*;
use std::future::Future;
use std::marker::PhantomData;
use std::pin::Pin;
use std::task::{Poll, Context};

#[doc(hidden)] pub use futures::executor::block_on;
#[doc(hidden)] pub use std::result::Result;

// TODO: Make NullFuture uninhabited when rust #59972 is solved.

#[inline(never)]
pub fn event_error() -> ! {
    panic!("internal static-events error, this is likely a bug")
}

#[inline(never)]
pub fn async_panicked_error() -> ! {
    panic!("poll on panicked future")
}

#[inline(never)]
pub fn async_already_done_error() -> ! {
    panic!("poll on completed future")
}

pub struct FutureSyncnessWrapper<F: Future<Output = EventResult>, T>(F, PhantomData<(T, *mut ())>);
impl <F: Future<Output = EventResult>, T> FutureSyncnessWrapper<F, T> {
    #[inline(always)]
    pub fn new(f: F) -> FutureSyncnessWrapper<F, T> {
        FutureSyncnessWrapper(f, PhantomData)
    }
}
impl <F: Future<Output = EventResult>, T> Future for FutureSyncnessWrapper<F, T> {
    type Output = EventResult;
    #[inline(always)]
    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let internal = unsafe { self.map_unchecked_mut(|x| &mut x.0) };
        internal.poll(cx)
    }
}
unsafe impl <F: Future<Output = EventResult>, T> Send for FutureSyncnessWrapper<F, T>
    where T: Send { }

pub struct NullFuture(());
impl Future for NullFuture {
    type Output = EventResult;

    #[inline(always)]
    fn poll(self: Pin<&mut Self>, _: &mut Context) -> Poll<Self::Output> {
        event_error()
    }
}

pub trait UniversalEventHandler<
    'a, E: Events, Ev: Event + 'a, P: EventPhase, D = DefaultHandler,
>: Events {
    const IS_IMPLEMENTED: bool;
    const IS_ASYNC: bool;
    fn on_phase(
        &'a self, target: &'a Handler<E>, ev: &'a mut Ev, state: &'a mut Ev::State,
    ) -> EventResult;

    type FutureType: Future<Output = EventResult>;
    unsafe fn on_phase_async(
        &'a self, target: &'a Handler<E>, ev: &'a mut Ev, state: &'a mut Ev::State,
    ) -> Self::FutureType;
}
impl <
    'a, E: Events, Ev: Event + 'a, P: EventPhase, D, T: Events,
> UniversalEventHandler<'a, E, Ev, P, D> for T {
    default const IS_IMPLEMENTED: bool = false;
    default const IS_ASYNC: bool = false;

    #[inline(always)]
    default fn on_phase(
        &'a self, _: &'a Handler<E>, _: &'a mut Ev, _: &'a mut Ev::State,
    ) -> EventResult {
        event_error()
    }

    default type FutureType = NullFuture;

    #[inline(always)]
    default unsafe fn on_phase_async(
        &'a self, _: &'a Handler<E>, _: &'a mut Ev, _: &'a mut Ev::State,
    ) -> Self::FutureType {
        event_error()
    }
}
impl <
    'a, E: Events, Ev: Event + 'a, P: EventPhase, D, T: Events + EventHandler<'a, E, Ev, P, D>,
> UniversalEventHandler<'a, E, Ev, P, D> for T {
    const IS_IMPLEMENTED: bool = <Self as EventHandler<'a, E, Ev, P, D>>::IS_IMPLEMENTED;
    const IS_ASYNC: bool = <Self as EventHandler<'a, E, Ev, P, D>>::IS_ASYNC;
    #[inline(always)]
    fn on_phase(
        &'a self, target: &'a Handler<E>, ev: &'a mut Ev, state: &'a mut Ev::State,
    ) -> EventResult {
        self.on_phase(target, ev, state)
    }
    type FutureType = <Self as EventHandler<'a, E, Ev, P, D>>::FutureType;
    #[inline(always)]
    unsafe fn on_phase_async(
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
pub const fn is_async<'a, T: Events, E: Events, Ev: Event + 'a, P: EventPhase, D>() -> bool {
    <T as UniversalEventHandler<'a, E, Ev, P, D>>::IS_ASYNC
}

#[inline(always)]
pub fn on_phase<
    'a, 'b: 'a, 'c: 'a, 'd: 'a, 'e: 'a, T: Events, E: Events, Ev: Event + 'a, P: EventPhase, D,
>(
    this: &'b T, target: &'c Handler<E>, ev: &'d mut Ev, state: &'e mut Ev::State,
) -> EventResult {
    if is_implemented::<'a, T, E, Ev, P, D>() {
        UniversalEventHandler::<'a, E, Ev, P, D>::on_phase(this, target, ev, state)
    } else {
        EventResult::EvOk
    }
}

#[inline(always)]
pub unsafe fn on_phase_async<'a, T: Events, E: Events, Ev: Event + 'a, P: EventPhase + 'a, D: 'a>(
    this: &'a T, target: &'a Handler<E>, ev: &'a mut Ev, state: &'a mut Ev::State,
) -> impl Future<Output = EventResult> + 'a {
    if is_implemented::<'a, T, E, Ev, P, D>() && is_async::<'a, T, E, Ev, P, D>() {
        <T as UniversalEventHandler<'a, E, Ev, P, D>>::on_phase_async(this, target, ev, state)
    } else {
        event_error()
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
