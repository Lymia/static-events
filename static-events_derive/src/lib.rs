#![feature(proc_macro_diagnostic, proc_macro_span, drain_filter)]
#![recursion_limit="128"]

extern crate proc_macro;
use proc_macro::{TokenStream, TokenTree};

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

fn attr_err(stream: TokenStream, warning: String) {
    assert!(!stream.is_empty(), warning);
    let mut iter = stream.into_iter();
    let first = iter.next().unwrap();
    let last = iter.last().unwrap_or_else(|| first.clone());
    first.span().join(last.span()).expect("invalid span").error(warning).emit();
}
fn smart_err_attr(attr: TokenStream, item: TokenStream, warning: String) {
    if attr.is_empty() {
        attr_err(item, warning)
    } else {
        attr_err(attr, warning)
    }
}
fn warn_helper_attribute(name: &str, attr: TokenStream, item: TokenStream) {
    let len = attr.clone().into_iter().count();
    if len != 1 {
        smart_err_attr(attr, item,
                       format!("{} can only be used inside #[event_dispatch] blocks.", name))
    } else if let Some(TokenTree::Ident(ident)) = attr.clone().into_iter().next() {
        if ident.to_string() != "event_handler_ok" {
            smart_err_attr(attr, item,
                           format!("{} can only be used inside #[event_dispatch] blocks.", name))
        }
    }
}

#[proc_macro_attribute]
pub fn event_handler(attr: TokenStream, item: TokenStream) -> TokenStream {
    warn_helper_attribute("#[event_handler]", attr, item.clone());
    item
}

#[proc_macro_attribute]
pub fn ipc_handler(attr: TokenStream, item: TokenStream) -> TokenStream {
    warn_helper_attribute("#[ipc_handler]", attr, item.clone());
    item
}