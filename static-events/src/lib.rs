#![no_std]
#![feature(nll, specialization)]

// TODO: Add a parallel set of traits for Futures
// TODO: Implement filtering of some kind on events, and state between phases.
// TODO: Document #[ipc_proxy]

//! A generic zero-cost event handler system. Event dispatches should get compiled down to a
//! plain function that executes all handlers involved with no dynamic dispatches.
//!
//! This crate relies on specialization, and can only be used on nightly versions of Rust.
//!
//! # Basic model
//!
//! Events can be any type that implements [`Event`], and the type itself is used to distinguish
//! different kinds of events. For detailed information on defining events, see the [`events`]
//! module.
//!
//! [`EventDispatch`]s receive events, and eventually dispatch them to event handler functions.
//! This trait is primarily implemented via the `#[event_dispatch]` attribute on an `impl` block.
//! This implementation will call methods in the `impl` block marked with `#[event_dispatch]` in
//! response to a matching event type.
//!
//! [`EventDispatch`]s can also be merged via `#[derive(EventDispatch)]`, Events will be
//! dispatched in order to each field in the `struct` or `enum` the attribute is added to.
//!
//! # Event dispatch
//!
//! Events dispatches behave like functions built up from individual event handlers called in
//! sequence. They take an event, and return a value (which may simply be `()`).
//!
//! Event dispatch proceeds in multiple phases, each of which can have its own each event handlers
//! attached to them:
//! [`EvInit`] -> [`EvCheck`], [`EvBeforeEvent`], [`EvOnEvent`], and [`EvAfterEvent`]
//!
//! Event handlers take both themselves and the current state as mutable borrows, and return a
//! status value controlling the rest of the event dispatch:
//! * [`EvOk`] continues the event dispatch as normal.
//! * [`EvCancelStage`] prevents the execution of the currently executing phase any further
//!   event handlers.
//! * [`EvCancel`] immediately stops the event dispatch, proceeding to the calculation of the
//!   return value.
//!
//! Note that this behavior is heavily customizable by particular [`Event`] implementations, and
//! can be significantly different from event to event.
//!
//! # Defining event handlers
//!
//! Event handlers are defined using the `#[event_dispatch]` annotation on an impl block. This
//! automatically creates an implementation of [`EventDispatch`] based on methods annotated
//! with `#[event_handler]` inside it.
//!
//! These methods have parameters in the following order: a `&self` parameter, an
//! `&impl EventDispatch` parameter, a mutable borrow of the event value, and a mutable
//! borrow of the event dispatch state.
//!
//! (e.g. `&self, target: &impl EventDispatch, ev: &mut MyEvent, state: &mut MyEventState`)
//!
//! Each parameter except the event value itself are optional, and the method may accept `ev`
//! or `state` by immutable borrows instead.
//!
//! Example:
//! ```
//! # use static_events::*;
//! struct MyEvent(u32);
//! simple_event!(MyEvent, u32, 0);
//!
//! #[derive(Default)]
//! struct MyEventHandler;
//!
//! #[event_dispatch]
//! impl MyEventHandler {
//!     #[event_handler]
//!     fn handle_event(ev: &MyEvent, i: &mut u32) {
//!         *i += ev.0;
//!     }
//! }
//!
//! assert_eq!(MyEventHandler.dispatch(MyEvent(42)), 42);
//! ```
//!
//! Multiple event handlers may be merged using `#[derive(RawEventDispatch)]`:
//! ```
//! # use static_events::*;
//! # struct MyEvent(u32);
//! # simple_event!(MyEvent, u32, 0);
//! #
//! # // From previous example
//! # #[derive(Default)]
//! # struct MyEventHandler;
//! # #[event_dispatch]
//! # impl MyEventHandler {
//! #     #[event_handler]
//! #     fn handle_event(ev: &MyEvent, i: &mut u32) {
//! #         *i += ev.0;
//! #     }
//! # }
//! #[derive(Default)]
//! struct MyOtherEventHandler;
//!
//! #[event_dispatch]
//! impl MyOtherEventHandler {
//!     #[event_handler]
//!     fn handle_event(ev: &MyEvent, i: &mut u32) {
//!         *i *= ev.0;
//!     }
//! }
//!
//! #[derive(Default, EventDispatch)]
//! struct SquaringEventHandler {
//!     evh_a: MyEventHandler, evh_b: MyOtherEventHandler,
//! }
//!
//! assert_eq!(SquaringEventHandler::default().dispatch(MyEvent(9)), 81);
//! ```
//!
//! # Limitations
//!
//! A fundamental limitation to this approach is that event handlers cannot be dynamically added
//! or removed at runtime, and sets of handlers can only be defined at compile-time.
//!
//! As all event handlers are passed around using immutable pointers, locking or cells must be
//! used to store state in handlers.

#[allow(unused_imports)] use core::fmt::Debug;
#[cfg(feature = "std")] extern crate std;

pub use static_events_derive::*;

mod events_types;
pub use crate::events_types::*;

pub mod events;
pub mod handlers;

pub use crate::events::{Event, EventResult};
pub use crate::events::EventResult::*;
pub use crate::handlers::{EventDispatch, SyncEventDispatch};
pub use crate::handlers::{EvInit, EvCheck, EvBeforeEvent, EvOnEvent, EvAfterEvent};

#[doc(hidden)]
/// This module is used by static-events_derive, and is not stable API.
pub mod private;

#[cfg(feature = "std")] pub mod handle;
