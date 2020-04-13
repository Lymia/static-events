#![recursion_limit="256"]

extern crate proc_macro;

use proc_macro::TokenStream;

#[macro_use]
mod utils;

mod common;
mod derive;
mod handlers;

#[proc_macro_derive(Events, attributes(subhandler, service, events))]
pub fn derive_events(input: TokenStream) -> TokenStream {
    derive::derive_events(input)
}

#[proc_macro_attribute]
pub fn events_impl(attr: TokenStream, item: TokenStream) -> TokenStream {
    handlers::events_impl(attr, item)
}

derived_attr!(event_handler, events_impl);
