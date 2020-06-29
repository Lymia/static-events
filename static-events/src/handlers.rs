//! The underlying traits used to define event handlers.

use crate::events::*;
use futures::FutureExt;
use futures::future::LocalBoxFuture;
use std::fmt;
use std::future::Future;
use std::sync::Arc;
use std::fmt::{Debug, Formatter};

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
pub trait Events: Sized + 'static {
    /// Gets a service from this event dispatch.
    fn get_service<S>(&self) -> Option<&S>;
}

/// The base trait used to mark asynchronous event dispatchers.
pub trait AsyncEvents: Events + Sync + Send { }

pub use static_events_derive::*;

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
    /// `true` if this `EventHandler` actually does anything. This is used for optimizations.
    const IS_IMPLEMENTED: bool = true;

    /// `true` if this `EventHandler` contains asynchronous handlers. This is used for
    /// optimizations and early panicking.
    const IS_ASYNC: bool = false;

    /// Runs a phase of this event.
    fn on_phase(
        &'a self, target: &'a Handler<E>, ev: &'a mut Ev, state: &'a mut Ev::State,
    ) -> EventResult;
}

/// A trait that defines a phase of handling a particular event asynchronously.
pub trait AsyncEventHandler<
    'a, E: AsyncEvents, Ev: Event + 'a, P: EventPhase, D = DefaultHandler,
> : AsyncEvents + EventHandler<'a, E, Ev, P, D> {
    /// The type of the future returned by `on_phase_async`.
    type FutureType: Future<Output = EventResult> + Send + 'a;

    /// Runs a phase of this event asynchronously.
    ///
    /// This function should only be called if `IS_IMPLEMENTED` and `IS_ASYNC` are `true`.
    fn on_phase_async(
        &'a self, target: &'a Handler<E>, ev: &'a mut Ev, state: &'a mut Ev::State,
    ) -> Self::FutureType;
}

/// A trait to help execute asynchronous event handlers in sync contents.
pub trait SyncFutureExecutor: Send + Sync + 'static {
    fn block_on(&self, fut: LocalBoxFuture<'_, ()>);
}

/// A builder for a [`Handler`].
#[derive(Default)]
pub struct HandlerBuilder<E: Events>(HandlerData<E>);
impl <E: Events> HandlerBuilder<E> {
    /// Creates a new [`HandlerBuilder`] with a given [`Events`].
    pub fn new(e: E) -> Self {
        HandlerBuilder(HandlerData {
            events: e,
            async_ctx: None,
        })
    }

    /// Creates a [`Handler`] from this builder.
    pub fn build(self) -> Handler<E> {
        Handler(Arc::new(self.0))
    }
}
impl <E: AsyncEvents> HandlerBuilder<E> {
    /// Sets the executor this builder uses to run async events in synchronous dispatches.
    pub fn future_executor(mut self, executor: impl SyncFutureExecutor) -> HandlerBuilder<E> {
        self.0.async_ctx = Some(Box::new(executor));
        self
    }
}

/// A wrapper for [`Events`] that allows dispatching events into them.
#[derive(Default)]
pub struct Handler<E: Events>(Arc<HandlerData<E>>);
#[derive(Default)]
struct HandlerData<E: Events> {
    events: E,
    async_ctx: Option<Box<dyn SyncFutureExecutor>>,
}
impl <E: Events> Handler<E> {
    /// Wraps an [`Events`] to allow dispatching events into it.
    pub fn new(e: E) -> Self {
        Self::builder(e).build()
    }

    /// Creates a new [`HandlerBuilder`] with a given [`Events`].
    pub fn builder(e: E) -> HandlerBuilder<E> {
        HandlerBuilder::new(e)
    }

    /// Retrieves a service from an [`Events`].
    ///
    /// # Panics
    ///
    /// This function will panic if the service does not exist.
    ///
    /// # Example
    /// ```rust
    /// use static_events::prelude_sync::*;
    ///
    /// #[derive(Eq, PartialEq, Debug)]
    /// struct TestService;
    ///
    /// #[derive(Events)]
    /// struct EventHandler(#[service] TestService);
    ///
    /// let handler = Handler::new(EventHandler(TestService));
    /// assert_eq!(handler.get_service::<TestService>(), &TestService);
    /// ```
    pub fn get_service<S>(&self) -> &S {
        match self.0.events.get_service() {
            Some(v) => v,
            None => missing_service::<S>(),
        }
    }

    /// Retrieves a service that may or may not exist from an [`Events`].
    ///
    /// # Example
    /// ```rust
    /// use static_events::prelude_sync::*;
    ///
    /// #[derive(Eq, PartialEq, Debug)]
    /// struct TestService;
    ///
    /// #[derive(Events)]
    /// struct EventHandler(#[service] TestService);
    ///
    /// let handler = Handler::new(EventHandler(TestService));
    /// assert_eq!(handler.get_service_opt::<TestService>(), Some(&TestService));
    /// ```
    pub fn get_service_opt<S>(&self) -> Option<&S> {
        self.0.events.get_service()
    }

    /// Downcasts this handler into a particular concrete handler type.
    pub fn downcast_ref<E2: Events>(&self) -> Option<&Handler<E2>> {
        crate::private::CheckDowncast::<Handler<E2>>::downcast_ref(self)
    }

    /// Dispatches an event synchronously.
    ///
    /// Any asynchronous event handlers are polled until completion.
    ///
    /// If this `Handler` does not contain a [`SyncFutureExecutor`], this function will panic if
    /// the event dispatches to any async event handlers.
    pub fn dispatch_sync<Ev: Event>(&self, mut ev: Ev) -> Ev::RetVal {
        let mut state = ev.starting_state();
        'outer: loop {
            macro_rules! do_phase {
                ($phase:ident) => {
                    if crate::private::is_implemented::<E, E, Ev, $phase, DefaultHandler>() {
                        match crate::private::on_phase::<E, E, Ev, $phase, DefaultHandler>(
                            &self.0.events, self, &mut ev, &mut state
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
        ev.to_return_value(state)
    }

    /// Returns the number of active references to this `Handler`.
    pub fn refcount(&self) -> usize {
        Arc::strong_count(&self.0)
    }
}
impl <E: AsyncEvents> Handler<E> {
    /// Blocks on a future in the context of this handler.
    ///
    /// This uses the [`SyncFutureExecutor`] contained in this handler, and panics if one is not
    /// available.
    pub fn block_on_future<T>(&self, fut: impl Future<Output = T>) -> T {
        let mut result = None;
        match self.0.async_ctx.as_ref() {
            Some(x) => x.block_on(async { result = Some(fut.await); }.boxed_local()),
            None => no_async_executor(),
        }
        match result {
            Some(x) => x,
            None => internal_async_executor_err(),
        }
    }

    /// Dispatches an event asynchronously. This future is bounded by the lifetime of `&self`
    /// and the event.
    ///
    /// Any synchronous events are run immediately as part of the [`Future::poll`] execution.
    ///
    /// This method requires that the [`Events`] type parameter is [`Sync`].
    pub fn dispatch_async<'a, Ev: AsyncEvent + 'a>(
        &'a self, mut ev: Ev,
    ) -> impl Future<Output = Ev::RetVal> + 'a {
        async move {
            let mut state = ev.starting_state();
            'outer: loop {
                macro_rules! do_phase {
                    ($phase:ident) => {
                        if crate::private::is_implemented::<E, E, Ev, $phase, DefaultHandler>() {
                            let result =
                                if crate::private::is_async::<E, E, Ev, $phase, DefaultHandler>() {
                                    crate::private::on_phase_async::<
                                        E, E, Ev, $phase, DefaultHandler,
                                    >(
                                        &self.0.events, self, &mut ev, &mut state
                                    ).await
                                } else {
                                    crate::private::on_phase::<E, E, Ev, $phase, DefaultHandler>(
                                        &self.0.events, self, &mut ev, &mut state
                                    )
                                };
                            match result {
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
            ev.to_return_value(state)
        }
    }

    /// Dispatches an event asynchronously.
    ///
    /// Any synchronous events are run immediately as part of the [`Future::poll`] execution.
    ///
    /// This method requires that the [`Events`] type parameter is [`Sync`].
    pub fn dispatch_async_static<Ev: AsyncEvent + 'static>(
        &self, ev: Ev,
    ) -> impl Future<Output = Ev::RetVal> + 'static {
        let this = self.clone();
        async move {
            this.dispatch_async(ev).await
        }
    }
}
impl <E: Events> Clone for Handler<E> {
    fn clone(&self) -> Self {
        Handler(self.0.clone())
    }
}
impl <E: Events + Debug> Debug for Handler<E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Handler")
            .field("events", &self.0.events)
            .finish()
    }
}

#[inline(never)]
#[cold]
fn missing_service<S>() -> ! {
    panic!("Missing service: {}", std::any::type_name::<S>())
}

#[inline(never)]
#[cold]
fn no_async_executor() -> ! {
    panic!("An async handler was found during a synchronous events dispatch, and no \
            `SyncFutureExecutor` available in the given context.")
}

#[inline(never)]
#[cold]
fn internal_async_executor_err() -> ! {
    panic!("Given `SyncFutureExecutor` instance did not run the future?")
}