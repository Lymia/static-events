//! The underlying traits used to define event handlers.

use crate::events::*;
use std::future::Future;
use std::sync::Arc;

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
    /// # #![feature(type_alias_impl_trait, async_await)]
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
    pub async fn dispatch_async<'a, Ev: Event + 'a>(
        &'a self, mut ev: Ev,
    ) -> Ev::RetVal {
        let mut state = ev.starting_state(self);
        'outer: loop {
            macro_rules! do_phase {
                ($phase:ident) => {
                    if crate::private::is_implemented::<E, E, Ev, $phase, DefaultHandler>() {
                        let result =
                            if crate::private::is_async::<E, E, Ev, $phase, DefaultHandler>() {
                                crate::private::on_phase_async::<E, E, Ev, $phase, DefaultHandler>(
                                    &self.0, self, &mut ev, &mut state
                                ).await
                            } else {
                                crate::private::on_phase::<E, E, Ev, $phase, DefaultHandler>(
                                    &self.0, self, &mut ev, &mut state
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
        ev.to_return_value(self, state)
    }

    /// Dispatches an event asynchronously.
    ///
    /// Any synchronous events are run immediately as part of the [`Future::poll`] execution.
    pub fn dispatch_async_static<Ev: Event + 'static>(
        &self, ev: Ev,
    ) -> impl Future<Output = Ev::RetVal> + 'static {
        let this = self.clone();
        async move {
            this.dispatch_async(ev).await
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
