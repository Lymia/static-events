#![feature(const_fn, nll, specialization, futures_api, async_await, await_macro)]
#![feature(existential_type, decl_macro, doc_cfg)]

// TODO: Implement filtering of some kind on events, and state between phases.
// TODO: Reevaluate all our unsafety and optimization hacks as Rust's async support improves.
// TODO: Document how to set the phase of an event handler.

//! A generic zero-cost event handler system built on compile-time magic.
//!
//! Synchronous event dispatches should get compiled down to a plain function that executes all
//! handlers involved with no dynamic dispatches. Asynchrnous event dispatches should be
//! similarily compile down to a relatively optimized future
//!
//! This crate relies on many unstable features, and can only be used on nightly versions of Rust.
//! It currently requires the following features:
//!
//! ```
//! #![feature(existential_type, futures_api, async_await, await_macro)]
//! ```
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
//! # #![feature(existential_type, futures_api, async_await, await_macro)]
//! # use static_events::*;
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
//! assert_eq!(handler.dispatch(MyEvent(42)), 42);
//! ```
//!
//! Fields inside the [`Events`] can be marked with `#[subhandler]` to cause any events to be
//! passed on to another event handler:
//! ```
//! # #![feature(existential_type, futures_api, async_await, await_macro)]
//! # use static_events::*;
//! # struct MyEvent(u32); simple_event!(MyEvent, u32, 0);
//! # #[derive(Events, Default)] struct MyEventHandler;
//! # #[events_impl] impl MyEventHandler {
//! #     #[event_handler] fn handle_event(ev: &MyEvent, i: &mut u32) {
//! #         *i += ev.0;
//! #     }
//! # }
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
//! assert_eq!(handler.dispatch(MyEvent(9)), 81);
//! ```
//!
//! # Limitations
//!
//! A fundamental limitation to this approach is that event handlers cannot be dynamically added
//! or removed at runtime, and sets of handlers can only be defined at compile-time.
//!
//! As event handlers are passed around using immutable pointers, locking or cells must be used to
//! store state.

pub use static_events_derive::{Events, events_impl, event_handler};

mod events_types;
pub use crate::events_types::*;

pub mod events;
pub mod handle;
pub mod handlers;

pub use crate::events::{Event, EventResult};
pub use crate::events::EventResult::*;
pub use crate::handlers::{Events, SyncEvents, Handler};
pub use crate::handlers::{EvInit, EvCheck, EvBeforeEvent, EvOnEvent, EvAfterEvent};

// Fixes for documentation.
#[allow(unused_imports)] use std::fmt::Debug;
#[allow(unused_imports)] use events::*;

#[doc(hidden)]
/// This module is used by static-events_derive, and is not stable API.
pub mod private;
