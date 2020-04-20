#![recursion_limit="256"]

extern crate proc_macro;

#[macro_use] pub mod utils;

mod common;
mod derive;
mod errors;
mod handlers;

pub use crate::errors::*;
pub use crate::derive::DeriveStaticEvents;
pub use crate::handlers::EventsImplAttr;