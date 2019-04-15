//! The underlying traits used to define event handlers.

use crate::events::*;
use std::cell::UnsafeCell;
use std::future::Future;
use std::pin::Pin;
use std::task::{Context, Poll};
use crate::events::EventResult::EvOk;

pub(crate) mod private {
    pub trait Sealed { }
}

/// A sealed trait that marks the phases of an event.
pub trait EventPhase: private::Sealed { }
macro_rules! phases {
    ($(($name:ident, $doc:literal))*) => {$(
        #[doc = $doc]
        pub enum $name { }
        impl private::Sealed for $name { }
        impl EventPhase for $name { }
    )*}
}
phases! {
    (EvInit       , "The first phase of event execution. Intended to be used to set up an event.")
    (EvCheck      , "The second phase of event execution. Intended to be used to check \
                     conditions required for the execution of the event.")
    (EvBeforeEvent, "The third phase of event execution. Intended to be used for hooks that \
                     execute before the main actions of an event.")
    (EvOnEvent    , "The fourth phase of event execution. Intended to be used for the main \
                     actions of an event.")
    (EvAfterEvent , "The fifth phase of event execution. Intended to be used for hooks that \
                     execute after the main actions of an event.")
}

/// Distinguisher for the actual default handler invoked by [`Handler`]
pub enum DefaultHandler { }

/// The base trait used to mark event dispatchers.
pub trait Events: 'static + Sized {
    /// Gets a service from this event dispatch.
    fn get_service<S>(&self) -> Option<&S>;
}

/// A trait that defines a phase of handling a particular event.
///
/// This is not designed to be manually implemented, and is unsafe due to various internal
/// optimizations done.
///
/// # Type parameters
/// * `'a`: The lifetime the async portion of this event handler is linked to.
/// * `E`: The type of event handler this event is being dispatched into.
/// * `Ev`: The event this handler is for.
/// * `P`: The event phase this handler is for.
/// * `D`: A distinguisher used internally by the `#[event_dispatch]` to allow overlapping event
///        handler implementations. This defaults to [`DefaultHandler`], which is what [`Handler`]
///        actually invokes events with.
pub trait EventHandler<'a, E: Events, Ev: Event + 'a, P: EventPhase, D = DefaultHandler>: Events {
    /// `true` if this `EventHandler` actually does anything. Used for optimizations.
    const IS_IMPLEMENTED: bool;

    /// `true` if this `EventHandler` is asynchronous. Used for optimizations.
    const IS_ASYNC: bool;

    /// Runs a phase of this event.
    fn on_phase(
        &'a self, target: &'a Handler<E>, ev: &'a mut Ev, state: &'a mut Ev::State,
    ) -> EventResult;

    /// The type of the future used by this event handler.
    type FutureType: Future<Output = EventResult> + 'a;

    /// Runs a phase of this event asynchronously.
    ///
    /// This function may execute undefined behavior if it is called if `IS_IMPLEMENTED` or
    /// `IS_ASYNC` are false.
    unsafe fn on_phase_async(
        &'a self, target: &'a Handler<E>, ev: &'a mut Ev, state: &'a mut Ev::State,
    ) -> Self::FutureType;
}

macro_rules! make_existentials {
    ($($id:ident)*) => {$(
        existential type $id<'a, E: Events, Ev: Event>: Future<Output = EventResult> + 'a;
    )*}
}
macro_rules! make_existential_fns {
    ($(($name:ident $id:ident $phase:ident $do_phase:ident $do_resume:ident $next:ident))*) => {$(
        unsafe fn $name(&self) -> $id<'a, E, Ev> {
            crate::private::on_phase_async::<'a, E, E, Ev, $phase, DefaultHandler>(
                &self.this.0, &self.this,
                &mut *self.ev.get(), (&mut *self.state.get()).as_mut().unwrap(),
            )
        }

        fn $do_phase(&mut self, cx: &mut Context<'_>) -> Poll<Ev::RetVal> {
            self.fut_state = AsyncDispatchState::Errored;
            if !crate::private::is_implemented::<'a, E, E, Ev, $phase, DefaultHandler>() {
                self.$next(cx)
            } else if !crate::private::is_async::<'a, E, E, Ev, $phase, DefaultHandler>() {
                let result = unsafe {
                    crate::private::on_phase::<E, E, Ev, $phase, DefaultHandler>(
                        &self.this.0, &self.this,
                        &mut *self.ev.get(), (&mut *self.state.get()).as_mut().unwrap(),
                    )
                };
                match result {
                    EvOk => self.$next(cx),
                    _ => self.done(cx),
                }
            } else {
                let handler = unsafe { self.$name() };
                self.fut_state = AsyncDispatchState::$phase(handler);
                self.$do_resume(cx)
            }
        }

        fn $do_resume(&mut self, cx: &mut Context<'_>) -> Poll<Ev::RetVal> {
            if let AsyncDispatchState::$phase(future) = &mut self.fut_state {
                self.is_poisoned = true;
                let res = unsafe { Pin::new_unchecked(future) }.poll(cx);
                self.is_poisoned = false;
                match res {
                    Poll::Ready(EvOk) => {
                        self.fut_state = AsyncDispatchState::Errored;
                        self.$next(cx)
                    },
                    Poll::Ready(_) => self.done(cx),
                    Poll::Pending => Poll::Pending,
                }
            } else {
                crate::private::event_error()
            }
        }
    )*}
}
make_existentials! {
    EvInitExistential EvCheckExistential
    EvBeforeEventExistential EvOnEventExistential EvAfterEventExistential
}
enum AsyncDispatchState<'a, E: Events, Ev: Event + 'a> {
    NeverRun, Done, Errored,
    EvInit       (EvInitExistential       <'a, E, Ev>),
    EvCheck      (EvCheckExistential      <'a, E, Ev>),
    EvBeforeEvent(EvBeforeEventExistential<'a, E, Ev>),
    EvOnEvent    (EvOnEventExistential    <'a, E, Ev>),
    EvAfterEvent (EvAfterEventExistential <'a, E, Ev>),
}
struct AsyncDispatchFuture<'a, E: Events, Ev: Event + 'a> {
    this: &'a Handler<E>,
    ev: UnsafeCell<Ev>,
    state: UnsafeCell<Option<Ev::State>>,
    is_poisoned: bool,
    fut_state: AsyncDispatchState<'a, E, Ev>,
}
impl <'a, E: Events, Ev: Event> AsyncDispatchFuture<'a, E, Ev> {
    make_existential_fns! {
        (on_init   EvInitExistential        EvInit        do_init   resume_init   do_check )
        (on_check  EvCheckExistential       EvCheck       do_check  resume_check  do_before)
        (on_before EvBeforeEventExistential EvBeforeEvent do_before resume_before do_event )
        (on_event  EvOnEventExistential     EvOnEvent     do_event  resume_event  do_after )
        (on_after  EvAfterEventExistential  EvAfterEvent  do_after  resume_after  done     )
    }

    fn done(&mut self, _: &mut Context<'_>) -> Poll<Ev::RetVal> {
        self.fut_state = AsyncDispatchState::Done;
        let ev = unsafe { &mut *self.ev.get() };
        let state = unsafe { &mut *self.state.get() };
        Poll::Ready(ev.to_return_value(self.this, state.take().unwrap()))
    }
}
impl <'a, E: Events, Ev: Event> Future for AsyncDispatchFuture<'a, E, Ev> {
    type Output = Ev::RetVal;
    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let fut = unsafe { self.get_unchecked_mut() };
        if fut.is_poisoned {
            fut.fut_state = AsyncDispatchState::Errored;
            fut.is_poisoned = false;
        }

        match fut.fut_state {
            AsyncDispatchState::NeverRun         => fut.do_init(cx),
            AsyncDispatchState::Done             => crate::private::async_already_done_error(),
            AsyncDispatchState::Errored          => crate::private::async_panicked_error(),
            AsyncDispatchState::EvInit       (_) => fut.resume_init(cx),
            AsyncDispatchState::EvCheck      (_) => fut.resume_check(cx),
            AsyncDispatchState::EvBeforeEvent(_) => fut.resume_before(cx),
            AsyncDispatchState::EvOnEvent    (_) => fut.resume_event(cx),
            AsyncDispatchState::EvAfterEvent (_) => fut.resume_after(cx),
        }
    }
}

#[repr(transparent)]
/// A wrapper for [`Events`] that allows dispatching events into them.
pub struct Handler<E: Events>(E);
impl <E: Events> Handler<E> {
    pub fn new(e: E) -> Self {
        Handler(e)
    }

    pub fn get_service<S>(&self) -> Option<&S> {
        self.0.get_service()
    }

    pub fn downcast_ref<E2: Events>(&self) -> Option<&Handler<E2>> {
        crate::private::CheckDowncast::<Handler<E2>>::downcast_ref(self)
    }

    #[inline(never)]
    pub fn dispatch<Ev: Event>(&self, mut ev: Ev) -> Ev::RetVal {
        let mut state = ev.starting_state(self);
        'outer: loop {
            macro_rules! do_phase {
                ($phase:ident) => {
                    if crate::private::is_implemented::<E, E, Ev, $phase, DefaultHandler>() {
                        match crate::private::on_phase::<E, E, Ev, $phase, DefaultHandler>(
                            &self.0, self, &mut ev, &mut state
                        ) {
                            EventResult::EvOk | EventResult::EvCancelStage => { }
                            EventResult::EvCancel => break 'outer,
                        }
                    }
                }
            }
            do_phase!(EvInit);
            do_phase!(EvCheck);
            do_phase!(EvBeforeEvent);
            do_phase!(EvOnEvent);
            do_phase!(EvAfterEvent);
            break 'outer
        }
        ev.to_return_value(self, state)
    }

    #[inline(never)]
    pub fn dispatch_async<'a, Ev: Event + 'a>(
        &'a self, ev: Ev,
    ) -> impl Future<Output = Ev::RetVal> + 'a {
        let state = ev.starting_state(self);
        AsyncDispatchFuture {
            this: self, ev: UnsafeCell::new(ev), state: UnsafeCell::new(Some(state)),
            is_poisoned: false, fut_state: AsyncDispatchState::NeverRun,
        }
    }
}

/*
/// A [`RawEventDispatch`] that can be shared between threads.
pub trait SyncRawEventDispatch: RawEventDispatch + Sync + Send { }
impl <T: RawEventDispatch + Sync + Send> SyncEventDispatch for T { }

/// A [`EventDispatch`] that can be shared between threads.
pub trait SyncEventDispatch: EventDispatch + Sync + Send { }
impl <T: EventDispatch + Sync + Send> SyncEventDispatch for T { }
*/