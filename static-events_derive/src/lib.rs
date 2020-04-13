#![recursion_limit="256"]

extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::{TokenStream as SynTokenStream, TokenTree};
use quote::*;

#[macro_use]
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

const ATTR_OK_STR: &str = concat!(
    "__event_handler_ok_2e72dd274be94c5e85063900550c326d_",
    env!("CARGO_PKG_VERSION_MAJOR"),
    "_",
    env!("CARGO_PKG_VERSION_MINOR"),
    "_",
    env!("CARGO_PKG_VERSION_PATCH"),
);

fn smart_err_attr(attr: SynTokenStream, item: SynTokenStream, error: &str) -> SynTokenStream {
    syn::Error::new(
        common::stream_span(if attr.is_empty() { item } else { attr }), error,
    ).to_compile_error()
}
fn is_handler_valid(attr: SynTokenStream) -> bool {
    if attr.clone().into_iter().count() != 1 { return false }
    if let Some(TokenTree::Ident(ident)) = attr.clone().into_iter().next() {
        ident.to_string() == ATTR_OK_STR
    } else {
        false
    }
}
fn warn_helper_attribute(
    name: &str, attr: SynTokenStream, item: SynTokenStream,
) -> SynTokenStream {
    if !is_handler_valid(attr.clone()) {
        smart_err_attr(attr, item,
                       &format!("{} can only be used inside #[events_impl] blocks.", name))
    } else {
        SynTokenStream::new()
    }
}

macro_rules! derived_attr {
    ($event_name:ident) => {
        #[proc_macro_attribute]
        pub fn $event_name(attr: TokenStream, item: TokenStream) -> TokenStream {
            let item: SynTokenStream = item.into();
            let attr_str = concat!("#[", stringify!($event_name),"]");
            let warn = warn_helper_attribute(attr_str, attr.into(), item.clone());
            (quote! {
                #warn
                #item
            }).into()
        }
    }
}
derived_attr!(event_handler);
