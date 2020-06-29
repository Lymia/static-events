#![feature(const_fn, specialization, type_alias_impl_trait, allow_internal_unstable, decl_macro)]

// TODO: Implement filtering of some kind on events, and state between phases.
// TODO: Document how to set the phase of an event handler.
// TODO: Refactor out a synchronous-only API that does not bound on Sync/etc.
//       (design: split out async EventHandler, async requires a Send bound on event.)
//       (design: require a Send+Sync bound on Events for async.)
//       (design: split out async/sync preludes.)
// TODO: Provide a way to provide a `Handle` with a means to control how it runs async code in
//       synchronous contexts.

//! A generic zero-cost asynchronous event handler system built on compile-time magic.
//!
//! Synchronous event dispatches should get compiled down to a plain function that executes all
//! handlers involved with no dynamic dispatches. Asynchrnous event dispatches should be
//! similarily compile down to a relatively optimized future
//!
//! This crate relies on many unstable features, and can only be used on nightly versions of Rust.
//!
//! # Basic model
//!
//! Events handlers can be any type that implements [`Event`], and the type of the event itself
//! is used to distinguish different kinds of events. For detailed information on defining events,
//! see the [`events`] module. These handlers are wrapped in a [`Handler`] to allow events to be
//! dispatched into them.
//!
//! [`Events`] is implemented using `#[derive(Events)]`. This can be paired with an `impl` block
//! marked with an `#[events_impl]` annotation, which is used to actually defined the handlers
//! used by the event handler.
//!
//! # Event dispatch
//!
//! Events dispatches behave like functions built up from individual event handlers called in
//! sequence. They take an event, and return a value (which may simply be `()`).
//!
//! Event dispatch proceeds in multiple phases, each of which can have its own each event handlers
//! attached to them:
//! [`EvInit`], [`EvCheck`], [`EvBeforeEvent`], [`EvOnEvent`], and [`EvAfterEvent`]
//!
//! The exact signature of the functions event handlers are built up of are defined by the
//! particular [`Event`] trait implementation for the event in question.
//!
//! # Defining event handlers
//!
//! Event handlers are defined by adding an `#[derive(Events)]` annotation. The event handlers's
//! response to various events is defined inside an impl block marked with `#[events_impl]`.
//! Any function in such an impl block marked with `#[event_handler]` will be called when an
//! event of an approprate type is passed into the corresponding [`Events`].
//!
//! These methods have parameters in the following order: a `&self` parameter, an
//! `&Handler<impl Events>` parameter, a mutable borrow of the event value, and a mutable
//! borrow of the event dispatch state.
//!
//! (e.g. `&self, target: &impl EventDispatch, ev: &mut MyEvent, state: &mut MyEventState`)
//!
//! Each parameter except the event value itself are optional, and the method may accept `ev`
//! or `state` by immutable borrows instead.
//!
//! Example:
//! ```
//! use static_events::prelude_sync::*;
//!
//! struct MyEvent(u32);
//! simple_event!(MyEvent, u32, 0);
//!
//! #[derive(Events, Default)]
//! struct MyEventHandler;
//!
//! #[events_impl]
//! impl MyEventHandler {
//!     #[event_handler]
//!     fn handle_event(ev: &MyEvent, i: &mut u32) {
//!         *i += ev.0;
//!     }
//! }
//!
//! let handler = Handler::new(MyEventHandler);
//! assert_eq!(handler.dispatch_sync(MyEvent(42)), 42);
//! ```
//!
//! Fields inside the [`Events`] can be marked with `#[subhandler]` to cause any events to be
//! passed on to another event handler:
//! ```
//! use static_events::prelude_sync::*;
//!
//! # struct MyEvent(u32); simple_event!(MyEvent, u32, 0);
//! # #[derive(Default)] struct MyEventHandler;
//! # #[derive(Events)] #[events(impl_on_external = "MyEventHandler")] struct MyEventHandler2;
//! # #[events_impl] impl MyEventHandler {
//! #     #[event_handler] fn handle_event(ev: &MyEvent, i: &mut u32) {
//! #         *i += ev.0;
//! #     }
//! # }
//! // events are defined as in the previous example
//!
//! #[derive(Events, Default)]
//! struct MyOtherEventHandler;
//!
//! #[events_impl]
//! impl MyOtherEventHandler {
//!     #[event_handler]
//!     fn handle_event(ev: &MyEvent, i: &mut u32) {
//!         *i *= ev.0;
//!     }
//! }
//!
//! #[derive(Events, Default)]
//! struct SquaringEventHandler {
//!     #[subhandler] evh_a: MyEventHandler,
//!     #[subhandler] evh_b: MyOtherEventHandler,
//! }
//!
//! let handler = Handler::new(SquaringEventHandler::default());
//! assert_eq!(handler.dispatch_sync(MyEvent(9)), 81);
//! ```
//!
//! # Limitations
//!
//! A fundamental limitation to this approach is that event handlers cannot be dynamically added
//! or removed at runtime, and sets of handlers can only be defined at compile-time.
//!
//! As event handlers are passed around using immutable pointers, locking or cells must be used to
//! store state.

mod events_types;
pub use crate::events_types::*;

pub mod events;
pub mod handle;
pub mod handlers;

// Fixes for documentation.
#[allow(unused_imports)] use std::fmt::Debug;
#[allow(unused_imports)] use events::*;

#[doc(hidden)]
/// This module is used by static-events_derive, and is not stable API.
pub mod private;

mod prelude_common {
    pub use crate::events::{Event, EventResult};
    pub use crate::events::EventResult::*;
    pub use crate::handlers::{Handler, event_handler};
    pub use crate::handlers::{EvInit, EvCheck, EvBeforeEvent, EvOnEvent, EvAfterEvent};
    pub use crate::{simple_event, self_event, failable_event};
}
mod prelude_private {
    use crate::handlers::*;
    pub trait Sealed { }
    impl <E: Events> Sealed for Handler<E> { }
}

/// A module containing useful imports for synchronous applications.
pub mod prelude_sync {
    pub use crate::prelude_common::*;
    pub use crate::handlers::{Events, events_impl};

    /// A helper extension trait for allowing `Handler::dispatch` as an alias to `dispatch_sync`.
    pub trait HandlerDispatchExt<E: Events>: crate::prelude_private::Sealed {
        fn dispatch<Ev: Event>(&self, ev: Ev) -> Ev::RetVal;
    }
    impl <E: Events> HandlerDispatchExt<E> for Handler<E> {
        fn dispatch<Ev: Event>(&self, ev: Ev) -> Ev::RetVal {
            self.dispatch_sync(ev)
        }
    }
}

/// A module containing useful imports for asynchronous applications.
#[allow(non_camel_case_types)]
pub mod prelude_async {
    pub use crate::prelude_common::*;
    pub use crate::events::SyncEvent;
    pub use crate::handle::EventsHandle;
    pub use crate::handlers::{SyncEvents as Events, sync_events_impl as events_impl};
}
