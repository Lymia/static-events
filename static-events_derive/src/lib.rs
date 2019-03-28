#![feature(proc_macro_diagnostic, proc_macro_span, drain_filter, bind_by_move_pattern_guards)]
#![recursion_limit="256"]

extern crate proc_macro;

use lazy_static::*;
use proc_macro::{TokenStream, TokenTree, Span};

mod derive;
mod handlers;

#[proc_macro_derive(Events)]
pub fn derive_events(input: TokenStream) -> TokenStream {
    derive::derive_events(input)
}

#[proc_macro_attribute]
pub fn event_dispatch(attr: TokenStream, item: TokenStream) -> TokenStream {
    handlers::event_dispatch(attr, item)
}

lazy_static! {
    static ref RAND_IDENT: String = format!("event_handler_ok_{}", rand::random::<u64>());
    static ref IMPL_COUNT_OFFSET: usize = rand::random::<usize>();
}

fn impl_id() -> usize {
    use std::sync::atomic::{AtomicUsize, Ordering};
    static IMPL_COUNT: AtomicUsize = AtomicUsize::new(0);
    IMPL_COUNT.fetch_add(1, Ordering::Relaxed).wrapping_add(*IMPL_COUNT_OFFSET)
}

fn merge_generics(a: &syn::Generics, b: &syn::Generics) -> syn::Generics {
    use quote::quote;
    use proc_macro2::{TokenStream as SynTokenStream};

    let mut toks = SynTokenStream::new();
    for lifetime in a.lifetimes() {
        toks.extend(quote! { #lifetime, })
    }
    for lifetime in b.lifetimes() {
        toks.extend(quote! { #lifetime, })
    }
    for bound in a.type_params() {
        toks.extend(quote! { #bound, })
    }
    for bound in b.type_params() {
        toks.extend(quote! { #bound, })
    }
    for const_bound in a.const_params() {
        toks.extend(quote! { #const_bound, })
    }
    for const_bound in a.const_params() {
        toks.extend(quote! { #const_bound, })
    }

    let mut generics = syn::parse2::<syn::Generics>(quote! { < #toks > }).unwrap();
    let mut toks = SynTokenStream::new();
    toks.extend(quote! { where });
    for where_element in &a.clone().make_where_clause().predicates {
        toks.extend(quote! { #where_element, })
    }
    for where_element in &b.clone().make_where_clause().predicates {
        toks.extend(quote! { #where_element, })
    }
    generics.where_clause = Some(syn::parse2(toks).unwrap());
    generics
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
