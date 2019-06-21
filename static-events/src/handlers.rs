//! The underlying traits used to define event handlers.

use crate::events::*;
use std::cell::UnsafeCell;
use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;
use std::task::{Context, Poll};
use std::marker::PhantomData;

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
/// # Type parameters
/// * `'a`: The lifetime this event handler is for.
/// * `E`: The type of event handler this event is being dispatched into.
/// * `Ev`: The event this handler is for.
/// * `P`: The event phase this handler is for.
/// * `D`: A phantom type used internally by the `#[events_impl]` macro to allow overlapping event
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

    /// The type of the future returned by `on_phase_async`.
    type FutureType: Future<Output = EventResult> + 'a;

    /// Runs a phase of this event asynchronously.
    ///
    /// This function should only be called if `IS_IMPLEMENTED` and `IS_ASYNC` are `true`.
    fn on_phase_async(
        &'a self, target: &'a Handler<E>, ev: &'a mut Ev, state: &'a mut Ev::State,
    ) -> Self::FutureType;
}

macro_rules! make_existentials {
    ($($id:ident)*) => {$(
        #[cfg(rustdoc)]
        type $id<'a, E, Ev> = ::std::marker::PhantomData<(&'a E, &'a Ev)>;

        #[cfg(not(rustdoc))]
        existential type $id<'a, E: Events, Ev: Event>: Future<Output = EventResult> + 'a;
    )*}
}
macro_rules! make_existential_fns {
    ($((
        $sync:ident $async:ident $existential:ident $phase:ident
        $do_phase:ident $do_resume:ident $next:ident
    ))*) => {$(
        unsafe fn $sync(&self) -> EventResult {
            let this = &*self.this;
            crate::private::on_phase::<E, E, Ev, $phase, DefaultHandler>(
                &this.0, &this, &mut *self.ev.get(), (&mut *self.state.get()).as_mut().unwrap(),
            )
        }
        /// This is extracted into a function to set the existential type properly.
        unsafe fn $async(&self) -> $existential<'a, E, Ev> {
            let this = &*self.this;
            crate::private::on_phase_async::<'a, E, E, Ev, $phase, DefaultHandler>(
                &this.0, &this, &mut *self.ev.get(), (&mut *self.state.get()).as_mut().unwrap(),
            )
        }
        crate::private::fragment_future_impl_methods!(
            <'a, E, E, Ev, $phase, DefaultHandler, Ev::RetVal>
            AsyncDispatchState $phase $do_phase $do_resume $next done
            $sync $async Errored Done
            (EventResult::EvCancelStage)
        );
    )*}
}
make_existentials! {
    EvInitFuture EvCheckFuture EvBeforeFuture EvEventFuture EvAfterFuture
}
enum AsyncDispatchState<'a, E: Events, Ev: Event + 'a> {
    NeverRun, Done, Errored,
    EvInit       (EvInitFuture  <'a, E, Ev>),
    EvCheck      (EvCheckFuture <'a, E, Ev>),
    EvBeforeEvent(EvBeforeFuture<'a, E, Ev>),
    EvOnEvent    (EvEventFuture <'a, E, Ev>),
    EvAfterEvent (EvAfterFuture <'a, E, Ev>),
}
struct AsyncDispatchFuture<'a, E: Events, Ev: Event + 'a> {
    this: *const Handler<E>, ev: UnsafeCell<Ev>, state: UnsafeCell<Option<Ev::State>>,
    pub is_poisoned: bool,
    pub fut_state: AsyncDispatchState<'a, E, Ev>,
}
impl <'a, E: Events, Ev: Event + 'a> AsyncDispatchFuture<'a, E, Ev> {
    fn new(this: *const Handler<E>, ev: Ev, state: Ev::State) -> Self {
        AsyncDispatchFuture {
            this, ev: UnsafeCell::new(ev), state: UnsafeCell::new(Some(state)),
            is_poisoned: false, fut_state: AsyncDispatchState::NeverRun,
        }
    }
    make_existential_fns! {
        (sync_init   async_init   EvInitFuture   EvInit        do_init   resume_init   do_check )
        (sync_check  async_check  EvCheckFuture  EvCheck       do_check  resume_check  do_before)
        (sync_before async_before EvBeforeFuture EvBeforeEvent do_before resume_before do_event )
        (sync_event  async_event  EvEventFuture  EvOnEvent     do_event  resume_event  do_after )
        (sync_after  async_after  EvAfterFuture  EvAfterEvent  do_after  resume_after  done     )
    }
    fn done(&mut self, _: &mut Context<'_>) -> Poll<Ev::RetVal> {
        self.fut_state = AsyncDispatchState::Done;
        let this = unsafe { &*self.this };
        let ev = unsafe { &mut *self.ev.get() };
        let state = unsafe { &mut *self.state.get() };
        Poll::Ready(ev.to_return_value(this, state.take().unwrap()))
    }
    fn do_poll(&mut self, cx: &mut Context<'_>, handler: *const Handler<E>) -> Poll<Ev::RetVal> {
        if self.is_poisoned {
            self.fut_state = AsyncDispatchState::Errored;
            self.is_poisoned = false;
        }
        match self.fut_state {
            AsyncDispatchState::NeverRun         => {
                self.this = handler;
                self.do_init(cx)
            },
            AsyncDispatchState::Done             => crate::private::async_already_done_error(),
            AsyncDispatchState::Errored          => crate::private::async_panicked_error(),
            AsyncDispatchState::EvInit       (_) => self.resume_init(cx),
            AsyncDispatchState::EvCheck      (_) => self.resume_check(cx),
            AsyncDispatchState::EvBeforeEvent(_) => self.resume_before(cx),
            AsyncDispatchState::EvOnEvent    (_) => self.resume_event(cx),
            AsyncDispatchState::EvAfterEvent (_) => self.resume_after(cx),
        }
    }
}
unsafe impl <'a, E: Events, Ev: Event + 'a> Send for AsyncDispatchFuture<'a, E, Ev>
    where E: Sync, Ev: Send, Ev::State: Send, AsyncDispatchState<'a, E, Ev>: Send { }

struct AsyncDispatchFutureStatic<E: Events, Ev: Event + 'static> {
    this: Handler<E>, fut: AsyncDispatchFuture<'static, E, Ev>,
}
impl <E: Events, Ev: Event + 'static> Future for AsyncDispatchFutureStatic<E, Ev> {
    type Output = Ev::RetVal;
    fn poll(self: Pin<&mut Self>, cx: &mut Context) -> Poll<Self::Output> {
        let this = unsafe { self.get_unchecked_mut() };
        this.fut.do_poll(cx, &this.this)
    }
}

struct AsyncDispatchFutureBounded<'a, E: Events, Ev: Event + 'a> {
    fut: AsyncDispatchFuture<'a, E, Ev>,
    phantom: PhantomData<&'a Handler<E>>,
}
impl <'a, E: Events, Ev: Event + 'a> Future for AsyncDispatchFutureBounded<'a, E, Ev> {
    type Output = Ev::RetVal;
    fn poll(self: Pin<&mut Self>, cx: &mut Context) -> Poll<Self::Output> {
        let this = unsafe { self.get_unchecked_mut() };
        this.fut.do_poll(cx, this.fut.this)
    }
}

#[derive(Default, Debug)]
/// A wrapper for [`Events`] that allows dispatching events into them.
pub struct Handler<E: Events>(Arc<E>);
impl <E: Events> Handler<E> {
    /// Wraps an [`Events`] to allow dispatching events into it.
    pub fn new(e: E) -> Self {
        Handler(Arc::new(e))
    }

    /// Retrieves a service from an [`Events`].
    ///
    /// # Example
    /// ```
    /// # #![feature(existential_type, async_await)]
    /// # use static_events::*;
    /// #[derive(Eq, PartialEq, Debug)]
    /// struct TestService;
    ///
    /// #[derive(Events)]
    /// struct EventHandler(#[service] TestService);
    ///
    /// let handler = Handler::new(EventHandler(TestService));
    /// assert_eq!(handler.get_service::<TestService>(), Some(&TestService));
    /// ```
    pub fn get_service<S>(&self) -> Option<&S> {
        self.0.get_service()
    }

    /// Downcasts this handler into a particular concrete handler type.
    pub fn downcast_ref<E2: Events>(&self) -> Option<&Handler<E2>> {
        crate::private::CheckDowncast::<Handler<E2>>::downcast_ref(self)
    }

    /// Dispatches an event synchronously.
    ///
    /// Any asynchronous event handlers are polled until completion.
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

    /// Dispatches an event asynchronously. This future is bounded by the lifetime of `&self`
    /// and the event.
    ///
    /// Any synchronous events are run immediately as part of the [`Future::poll`] execution.
    pub fn dispatch_async<'a, Ev: Event + 'a>(
        &'a self, ev: Ev,
    ) -> impl Future<Output = Ev::RetVal> + 'a {
        let state = ev.starting_state(self);
        AsyncDispatchFutureBounded {
            fut: AsyncDispatchFuture::new(self, ev, state),
            phantom: PhantomData,
        }
    }

    /// Dispatches an event asynchronously.
    ///
    /// Any synchronous events are run immediately as part of the [`Future::poll`] execution.
    pub fn dispatch_async_static<Ev: Event + 'static>(
        &self, ev: Ev,
    ) -> impl Future<Output = Ev::RetVal> + 'static {
        let state = ev.starting_state(self);
        AsyncDispatchFutureStatic {
            this: self.clone(),
            fut: AsyncDispatchFuture::new(::std::ptr::null(), ev, state)
        }
    }

    /// Returns the number of active references to this `Handler`.
    pub fn refcount(&self) -> usize {
        Arc::strong_count(&self.0)
    }
}
impl <E: Events> Clone for Handler<E> {
    fn clone(&self) -> Self {
        Handler(self.0.clone())
    }
}

/// An [`Events`] that can be shared between threads.
pub trait SyncEvents: Events + Sync + Send { }
impl <T: Events + Sync + Send> SyncEvents for T { }
