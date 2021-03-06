//! Various helper methods that may help with static-events based procedural derives.

use crate::errors::{Error, Result};
use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as SynTokenStream};
use std::fmt::Display;
use syn::*;
use quote::*;

/// Creates an identifier with a format-like syntax.
#[macro_export]
macro_rules! ident {
    ($($tts:tt)*) => { Ident::new(&format!($($tts)*), ::proc_macro2::Span::call_site()) }
}

/// Emits a `syn` based compile error.
pub fn error<T>(span: Span, message: impl Display) -> Result<T> {
    Err(Error::new(span, &message.to_string()))
}

/// Returns the actual type name of a path as a string.
pub fn last_path_segment(path: &Path) -> String {
    (&path.segments).into_iter().last().expect("Empty path?").ident.to_string()
}

/// Helpers for parsing interior attributes in the outer block.
const ATTR_OK_STR: &str = concat!(
    "(If you include this string in your crate, you are doing a bad, unstable thing.) ",
    "__",
    env!("CARGO_PKG_NAME"),
    "_attr_ok_2e72dd274be94c5e85063900550c326d_",
    env!("CARGO_PKG_VERSION"),
);

fn smart_err_attr(attr: SynTokenStream, item: SynTokenStream, error: &str) -> SynTokenStream {
    syn::Error::new(
        stream_span(if attr.is_empty() { item } else { attr }), error,
    ).to_compile_error()
}
fn is_handler_valid(attr: SynTokenStream) -> bool {
    if attr.clone().into_iter().count() != 1 { return false }
    parse2::<Lit>(attr).ok()
        .map(|x| match x {
            Lit::Str(s) => s.value() == ATTR_OK_STR,
            _ => false,
        })
        .unwrap_or(false)
}
fn err_helper_attribute(
    error_str: &str, attr: SynTokenStream, item: SynTokenStream,
) -> SynTokenStream {
    if !is_handler_valid(attr.clone()) {
        smart_err_attr(attr, item, error_str)
    } else {
        SynTokenStream::new()
    }
}

/// Checks if an attribute has been processed via `mark_attribute_processed`.
///
/// Not public API, use [`derived_attr!`] instead.
#[doc(hidden)]
pub fn check_attr(error_str: &str, attr: TokenStream, item: TokenStream) -> TokenStream {
    let item: SynTokenStream = item.into();
    let error = err_helper_attribute(error_str, attr.into(), item.clone());
    (quote! {
        #error
        #item
    }).into()
}

/// Creates a macro attribute that exists only to be processed by another macro attribute.
///
/// The macro will result in an error if it's used outside the macro. The macro must be
/// marked with [`mark_attribute_processed`] once processed to suppress this error.
#[macro_export]
macro_rules! derived_attr {
    (@error_str $attr:ident ($($head:tt)*) $inside:ident,) => {
        concat!(
            "#[", stringify!($attr), "] may only be used in a ",
            $($head)* "#[", stringify!($inside), "]",
            " block.",
        )
    };
    (@error_str $attr:ident ($($head:tt)*) $first:ident, $last:ident) => {
        concat!(
            "#[", stringify!($attr), "] may only be used in a ",
            $($head)* "#[", stringify!($first), "], or #[", stringify!($last), "]",
            " block.",
        )
    };
    (@error_str $attr:ident ($($head:tt)*) $inside:ident, $($rest:ident,)*) => {
        derived_attr!(@error_str ("#[", stringify!($inside), "], ",) $($rest,)*)
    };
    ($event_name:ident, $($inside:ident),* $(,)?) => {
        #[proc_macro_attribute]
        pub fn $event_name(attr: TokenStream, item: TokenStream) -> TokenStream {
            const ERROR_STR: &str = derived_attr!(@error_str $event_name () $($inside,)*);
            crate::utils::check_attr(ERROR_STR, attr, item)
        }
    };
}

/// Marks an attribute as having been successfully processed.
///
/// See [`derived_attr!`].
pub fn mark_attribute_processed(attr: &mut Attribute) {
    attr.tokens = quote! { (#ATTR_OK_STR) }.into();
}

/// Creates generics from a token stream.
///
/// # Example
///
/// ```rust
/// # use static_events_internals::utils::generics;
/// # use syn::Generics;
/// # use quote::*;
/// let generics: Generics = generics(quote! {
///     A, B: Copy,
/// });
/// # drop(generics);
/// ```
pub fn generics(a: impl ToTokens) -> Generics {
    parse2::<Generics>(quote! { < #a > }).unwrap()
}

/// Common function for processing generics.
fn process_generics(
    list: &[&Generics], skip_lifetimes: bool, reparent_lifetimes: Option<Lifetime>,
) -> Generics {
    let mut toks = SynTokenStream::new();
    if !skip_lifetimes {
        for g in list {
            for lifetime in g.lifetimes() {
                let mut lifetime = (*lifetime).clone();
                if let Some(lt) = &reparent_lifetimes {
                    lifetime.bounds.push(lt.clone());
                }
                toks.extend(quote! { #lifetime, })
            }
        }
    }
    for g in list {
        for bound in g.type_params() {
            toks.extend(quote! { #bound, })
        }
        for const_bound in g.const_params() {
            toks.extend(quote! { #const_bound, })
        }
    }

    let mut generics = generics(toks);
    if list.iter().any(|x| x.where_clause.is_some()) {
        let mut toks = SynTokenStream::new();
        toks.extend(quote! { where });
        for g in list {
            for where_element in &(*g).clone().make_where_clause().predicates {
                toks.extend(quote! { #where_element, })
            }
        }
        generics.where_clause = Some(parse2(toks).unwrap());
    }
    generics
}

/// Merges two sets of generics into one.
pub fn merge_generics(a: &Generics, b: &Generics) -> Generics {
    process_generics(&[a, b], false, None)
}

/// Strips lifetimes from a set of generics.
pub fn strip_lifetimes(a: &Generics) -> Generics {
    process_generics(&[a], true, None)
}

/// Reparents the lifetimes in a set of generics.
pub fn reparent_lifetimes(a: &Generics, lt: SynTokenStream) -> Generics {
    process_generics(&[a], false, Some(parse2(lt).unwrap()))
}

/// Creates a span for an entire TokenStream.
pub fn stream_span(attr: SynTokenStream) -> Span {
    let head_span = attr.clone().into_iter().next().unwrap().span();
    let tail_span = attr.into_iter().last().unwrap().span();
    head_span.join(tail_span).unwrap()
}
