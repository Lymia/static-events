//! This module is used by static-events_derive, and is not stable API.

use crate::events::*;
use crate::handlers::*;
use std::future::Future;
use std::pin::Pin;
use std::task::{Poll, Context};

#[doc(hidden)] pub use std::result::Result;

/// A hack to allow the `type_alias_impl_trait` feature flag to be contained to this crate.
#[allow_internal_unstable(type_alias_impl_trait)]
pub macro allow_existentials(($($head:tt)*) ($($tail:tt)*)) {
    type $($head)* = impl $($tail)*;
}

#[inline(never)]
#[cold]
pub fn event_error() -> ! {
    panic!("internal static-events error, this is likely a bug")
}

#[inline(never)]
#[cold]
pub fn async_in_sync() -> ! {
    panic!("events containing asynchronous handlers cannot be called in \
            synchronous dispatches or contexts.")
}

/// Casts a `Handler<impl Events>` to a `Handler<impl AsyncEvents>`
pub fn handler_as_sync_handler<E: Events>(handler: &Handler<E>) -> &Handler<impl AsyncEvents> {
    DowncastEvents::as_async_events(handler)
}
#[inline(never)]
#[cold]
fn async_handler_in_sync() -> ! {
    panic!("this event handler requires an asynchronous event dispatch")
}
trait DowncastEvents: Events {
    type AsAsyncEvents: AsyncEvents;
    fn as_async_events(handler: &Handler<Self>) -> &Handler<Self::AsAsyncEvents>;
}
impl <E: Events> DowncastEvents for E {
    default type AsAsyncEvents = NullAsyncEvents;
    default fn as_async_events(_: &Handler<Self>) -> &Handler<Self::AsAsyncEvents> {
        async_handler_in_sync()
    }
}
impl <E: AsyncEvents> DowncastEvents for E {
    type AsAsyncEvents = E;
    fn as_async_events(handler: &Handler<Self>) -> &Handler<Self::AsAsyncEvents> {
        handler
    }
}

/// An `AsyncEvents` of which no instances should exist.
pub enum NullAsyncEvents { }
impl Events for NullAsyncEvents {
    fn get_service<S>(&self) -> Option<&S> {
        event_error()
    }
}
impl AsyncEvents for NullAsyncEvents { }

/// A future of which no instances should exist. Used as a the future type when no async future
/// is defined.
pub enum NullFuture { }
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
}

pub trait UniversalAsyncEventHandler<
    'a, E: AsyncEvents, Ev: AsyncEvent + 'a, P: EventPhase, D = DefaultHandler,
>: AsyncEvents {
    type FutureType: Future<Output = EventResult> + Send;
    fn on_phase_async(
        &'a self, target: &'a Handler<E>, ev: &'a mut Ev, state: &'a mut Ev::State,
    ) -> Self::FutureType;
}
impl <
    'a, E: AsyncEvents, Ev: AsyncEvent + 'a, P: EventPhase, D, T: AsyncEvents,
> UniversalAsyncEventHandler<'a, E, Ev, P, D> for T {
    default type FutureType = NullFuture;
    default fn on_phase_async(
        &'a self, _: &'a Handler<E>, _: &'a mut Ev, _: &'a mut <Ev as Event>::State,
    ) -> Self::FutureType {
        event_error()
    }
}
impl <
    'a, E: AsyncEvents, Ev: AsyncEvent + 'a, P: EventPhase, D,
    T: AsyncEvents + AsyncEventHandler<'a, E, Ev, P, D>,
> UniversalAsyncEventHandler<'a, E, Ev, P, D> for T {
    type FutureType = <Self as AsyncEventHandler<'a, E, Ev, P, D>>::FutureType;
    #[inline(always)]
    fn on_phase_async(
        &'a self, target: &'a Handler<E>, ev: &'a mut Ev, state: &'a mut Ev::State,
    ) -> Self::FutureType {
        <Self as AsyncEventHandler<'a, E, Ev, P, D>>::on_phase_async(self, target, ev, state)
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
pub const fn is_any_async<'a, E: Events, Ev: Event + 'a>() -> bool {
    is_async::<'a, E, E, Ev, EvInit, DefaultHandler>() ||
        is_async::<'a, E, E, Ev, EvCheck, DefaultHandler>() ||
        is_async::<'a, E, E, Ev, EvBeforeEvent, DefaultHandler>() ||
        is_async::<'a, E, E, Ev, EvOnEvent, DefaultHandler>() ||
        is_async::<'a, E, E, Ev, EvAfterEvent, DefaultHandler>()
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
pub fn on_phase_async<
    'a, T: AsyncEvents, E: AsyncEvents, Ev: AsyncEvent + 'a, P: EventPhase + 'a, D: 'a,
>(
    this: &'a T, target: &'a Handler<E>, ev: &'a mut Ev, state: &'a mut Ev::State,
) -> <T as UniversalAsyncEventHandler<'a, E, Ev, P, D>>::FutureType {
    if is_implemented::<'a, T, E, Ev, P, D>() && is_async::<'a, T, E, Ev, P, D>() {
        <T as UniversalAsyncEventHandler<'a, E, Ev, P, D>>::on_phase_async(this, target, ev, state)
    } else {
        event_error()
    }
}

/// Distinguisher for the handler generated by `#[events_impl]`
pub enum HandlerImplBlock { }

/// Helper trait for statically checking if types are equal. Similar to `Any`, kinda.
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

/// A helper type for `failable_event!`.
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

