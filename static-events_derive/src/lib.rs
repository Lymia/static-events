#![recursion_limit="256"]

extern crate proc_macro;

use lazy_static::*;
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

lazy_static! {
    static ref RAND_IDENT: String = format!("event_handler_ok_{}", rand::random::<u64>());
}

fn smart_err_attr(attr: SynTokenStream, item: SynTokenStream, error: &str) -> SynTokenStream {
    syn::Error::new(
        common::stream_span(if attr.is_empty() { item } else { attr }), error,
    ).to_compile_error()
}
fn is_handler_valid(attr: SynTokenStream) -> bool {
    if attr.clone().into_iter().count() != 1 { return false }
    if let Some(TokenTree::Ident(ident)) = attr.clone().into_iter().next() {
        ident.to_string() == *RAND_IDENT
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

#[proc_macro_attribute]
pub fn event_handler(attr: TokenStream, item: TokenStream) -> TokenStream {
    let item: SynTokenStream = item.into();
    let warn = warn_helper_attribute("#[event_handler]", attr.into(), item.clone());
    (quote! {
        #warn
        #item
    }).into()
}
