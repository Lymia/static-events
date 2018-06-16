#![no_std]
#![feature(specialization, overlapping_marker_traits,
           macro_vis_matcher, macro_at_most_once_rep, allow_internal_unstable, use_extern_macros)]

// TODO: Use custom derive rather than the merged_event_handler! macro.
// TODO: Add a parallel set of traits for Futures

//! A generic zero-cost event handler system. Event dispatches should get compiled down to a
//! plain function that executes all handlers involved with no dynamic dispatches.
//!
//! As this crate depends on specialization, you must enable `#![feature(specialization)]` in
//! all crates that use this library:
//!
//! ```
//! #![feature(specialization)]
//! #[macro_use] extern crate static_events;
//! ```
//!
//! # Basic model
//!
//! Events can be any type that implements [`Event`], and the type itself is used as the key
//! that event handlers to distinguish different events.
//!
//! Event handlers are primarily defined by types that implement [`EventRoot`], which provides
//! a default noop [`EventHandler`] implementation for all [`Event`]s, which can be further
//! overwritten with explicit [`EventHandler`] implementations.
//!
//! [`RawEventDispatch`] is a further abstraction over [`EventRoot`], that has methods that are
//! individually generic over all [`Event`]s, rather than having an impl of [`EventHandler`]
//! for all [`Event`]s like [`EventRoot`].
//!
//! Finally, [`EventDispatch`] is the highest level event handling trait, with a function that
//! takes a [`Event`], and returns its `RetVal`.
//!
//! # Event dispatch
//!
//! Events dispatches fundamentally behave as function calls built up extensibly from individual
//! event handlers. They take an event, and return a value (which may simply be `()`).
//!
//! At the beginning of an event dispatch, [`Event::starting_state`] is called once to create
//! a temporary state value. This will be passed to all event handlers, and is used to store
//! transient state and as an accumulator for the final return value.
//!
//! The lower level event handlers, [`EventHandler`] and [`RawEventDispatch`] each contain 5
//! methods that are executed in the following order:<br>
//! `init` -> `check` -> `before_event` -> `on_event` -> `after_event`
//!
//! They take both the event itself and the current state as mutable borrows, and return a
//! status value controlling the rest of the event dispatch:
//! * [`EvOk`] continues the event dispatch as normal.
//! * [`EvCancelStage`] prevents the execution of the currently executing method any further
//!   event handlers.
//! * [`EvCancel`] immediately stops the event dispatch, proceeding to the calculation of the
//!   return value.
//!
//! Finally, at the end of event dispatch, [`Event::to_return_value`] is called on the state
//! to compute the final return value. In many cases, this will simply be a noop.
//!
//! # Defining events
//!
//! Any module can define an event. Events are normal types that implement the [`Event`] trait,
//! which can be declared with various macros;
//! * [`simple_event!`] for events that directly return their state to the caller, or do not
//!   use state at all.
//! * [`failable_event!`] for events that can fail and return [`Result`]s.
//! * [`ipc_event!`] for events that should only have one listener processing it.
//!
//! ```
//! # #[macro_use] extern crate static_events;
//! # use static_events::*;
//! pub struct MyEvent(u32);
//! simple_event!(MyEvent, u32, 0);
//! ```
//!
//! While [`Event`] is stable API, and can be manually implemented, this should only be done in
//! special cases.
//!
//! # Defining event handlers
//!
//! Individual event handlers are defined using a combination of [`EventRoot`] (a marker trait),
//! and any number of [`EventHandler`] implementations:
//!
//! ```
//! # #![feature(specialization)]
//! # #[macro_use] extern crate static_events;
//! # use static_events::*;
//! # struct MyEvent(u32);
//! # simple_event!(MyEvent, u32, 0);
//! struct MyEventHandler;
//! impl EventRoot for MyEventHandler { }
//! impl EventHandler<MyEvent> for MyEventHandler {
//!     fn on_event(&self, _: &impl EventDispatch, ev: &mut MyEvent, i: &mut u32) -> EventResult {
//!         *i += ev.0;
//!         EvOk
//!     }
//! }
//!
//! assert_eq!(MyEventHandler.dispatch(MyEvent(42)), 42);
//! ```
//!
//! [`simple_event_handler!`] may also be used instead for handlers with no state or parameters.
//! A [`event_handler!`] macro also exists which adds [`EventHandler`] impls to an existing
//! struct, rather than creating a new one:
//! ```
//! # #![feature(specialization)]
//! # #[macro_use] extern crate static_events;
//! # use static_events::*;
//! # struct MyEvent(u32);
//! # simple_event!(MyEvent, u32, 0);
//! simple_event_handler!(MyEventHandler, MyEvent: {
//!     on_event: |_, ev, i| { *i += ev.0 }
//! });
//! # assert_eq!(MyEventHandler.dispatch(MyEvent(42)), 42);
//! ```
//!
//! Finally, multiple event handlers may be merged using the [`merged_event_handler!`] macro:
//! ```
//! # #![feature(specialization)]
//! # #[macro_use] extern crate static_events;
//! # use static_events::*;
//! # struct MyEvent(u32);
//! # simple_event!(MyEvent, u32, 0);
//! simple_event_handler!(MyEventHandler, MyEvent: {
//!     on_event: |_, ev, i| { *i += ev.0 }
//! });
//! simple_event_handler!(MyOtherEventHandler, MyEvent: {
//!     on_event: |_, ev, i| { *i *= ev.0 }
//! });
//!
//! merged_event_handler! {
//!     #[derive(Default)]
//!     struct SquaringEventHandler<T: RawEventDispatch> {
//!         evh_a: MyEventHandler, evh_b: T,
//!     }
//! }
//!
//! assert_eq!(SquaringEventHandler::<MyOtherEventHandler>::default().dispatch(MyEvent(9)), 81);
//! ```
//!
//! # Limitations
//!
//! A fundamental limitation to this approach is that event handlers cannot be dynamically added
//! or removed at runtime, and sets of handlers can only be defined at compile-time.
//!
//! As all event handlers are passed around using immutable pointers, locking or cells must be
//! used to store state in handlers.


#[allow(unused_imports)] extern crate unhygienic;
#[doc(hidden)] pub use unhygienic::unhygienic_item;
#[doc(hidden)] pub use unhygienic::unhygienic_item_impl;
#[allow(unused_imports)] use core::fmt::Debug;

mod events_types;

mod interface;
pub use interface::*;

mod handler_dsl;
pub use handler_dsl::*;