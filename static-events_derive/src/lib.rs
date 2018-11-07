#![feature(proc_macro_diagnostic)]

extern crate proc_macro;
use self::proc_macro::TokenStream;

use proc_macro2::{TokenStream as SynTokenStream};
use syn::*;
use syn::export::Span;
use syn::spanned::Spanned;
use quote::*;

fn create_ret_callback(method_name: &Ident, field: impl ToTokens) -> SynTokenStream {
    quote! {
        match ::static_events::RawEventDispatch::#method_name(#field, target, ev, state) {
            ::static_events::EvOk => { }
            e => return e,
        }
    }
}
fn create_fn_body(method_name: &Ident, fields: &Fields) -> SynTokenStream {
    let mut stream = SynTokenStream::new();
    match fields {
        Fields::Named(named) => for field in &named.named {
            let name = &field.ident;
            stream.extend(create_ret_callback(method_name, quote!(&self.#name)))
        },
        Fields::Unnamed(unnamed) => for (i, _) in unnamed.unnamed.iter().enumerate() {
            stream.extend(create_ret_callback(method_name, quote!(&self.#i)))
        },
        Fields::Unit => { }
    }
    quote! {
        #stream
        ::static_events::EvOk
    }
}

fn create_fn(method_name: &Ident, body: impl ToTokens) -> SynTokenStream {
    quote! {
        fn #method_name<E: ::static_events::Event>(
            &self, target: &impl ::static_events::EventDispatch, ev: &mut E, state: &mut E::State,
        ) -> ::static_events::EventResult {
            #body
        }
    }
}

fn create_struct_fn(method_name: &str, fields: &Fields) -> SynTokenStream {
    let method_name = Ident::new(method_name, Span::call_site());
    create_fn(&method_name, create_fn_body(&method_name, fields))
}

#[proc_macro_derive(RawEventDispatch)]
pub fn derive_raw_event_dispatch(input: TokenStream) -> TokenStream {
    let input: DeriveInput = parse_macro_input!(input);
    let mut functions = SynTokenStream::new();
    let name = match &input.data {
        Data::Struct(data) => {
            functions.extend(create_struct_fn("init"        , &data.fields));
            functions.extend(create_struct_fn("check"       , &data.fields));
            functions.extend(create_struct_fn("before_event", &data.fields));
            functions.extend(create_struct_fn("on_event"    , &data.fields));
            functions.extend(create_struct_fn("after_event" , &data.fields));
            input.ident
        }
        Data::Enum(_) => unimplemented!(),
        Data::Union(_) => {
            input.span()
                .unstable()
                .error("RawEventDispatch can only be derived from a struct or enum.")
                .emit();
            return TokenStream::new()
        }
    };

    let (impl_bounds, ty_param, where_bounds) = input.generics.split_for_impl();
    TokenStream::from(quote! {
        impl #impl_bounds ::static_events::RawEventDispatch for #name #ty_param #where_bounds {
            #functions
        }
    })
}
