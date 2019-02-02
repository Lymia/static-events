#![feature(proc_macro_diagnostic, proc_macro_span, drain_filter, bind_by_move_pattern_guards)]
#![recursion_limit="128"]

extern crate proc_macro;

use lazy_static::*;
use proc_macro::{TokenStream, TokenTree, Span};

mod derive;
mod handlers;

#[proc_macro_derive(EventDispatch)]
pub fn derive_event_dispatch(input: TokenStream) -> TokenStream {
    derive::derive_event_dispatch(input)
}

#[proc_macro_attribute]
pub fn event_dispatch(attr: TokenStream, item: TokenStream) -> TokenStream {
    handlers::event_dispatch(attr, item)
}

lazy_static! {
    static ref RAND_IDENT: String = format!("event_handler_ok_{}", rand::random::<u64>());
}

fn stream_span(attr: TokenStream) -> Span {
    let head_span = attr.clone().into_iter().next().unwrap().span();
    let tail_span = attr.into_iter().last().unwrap().span();
    head_span.join(tail_span).unwrap()
}
fn smart_err_attr(attr: TokenStream, item: TokenStream, error: &str) {
    stream_span(if attr.is_empty() { item } else { attr }).error(error).emit()
}
fn is_handler_valid(attr: TokenStream) -> bool {
    if attr.clone().into_iter().count() != 1 { return false }
    if let Some(TokenTree::Ident(ident)) = attr.clone().into_iter().next() {
        ident.to_string() == *RAND_IDENT
    } else {
        false
    }
}
fn warn_helper_attribute(name: &str, attr: TokenStream, item: TokenStream) {
    if !is_handler_valid(attr.clone()) {
        smart_err_attr(attr, item,
                       &format!("{} can only be used inside #[event_dispatch] blocks.", name))

    }
}

#[proc_macro_attribute]
pub fn event_handler(attr: TokenStream, item: TokenStream) -> TokenStream {
    warn_helper_attribute("#[event_handler]", attr, item.clone());
    item
}
