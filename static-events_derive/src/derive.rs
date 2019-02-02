use proc_macro::TokenStream;
use proc_macro2::{TokenStream as SynTokenStream};
use syn::*;
use syn::export::Span;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use quote::*;

fn create_ret_callback(field: impl ToTokens) -> SynTokenStream {
    quote! {
        {
            let dispatch_result =
                ::static_events::handlers::RawEventDispatch::on_phase
                ::<__EventType, __EventPhase, __EventDispatch>(#field, target, ev, state);
            match dispatch_result {
                ::static_events::EvOk => { }
                e => return e,
            }
        }
    }
}
fn create_struct_fn_body(fields: &Fields) -> SynTokenStream {
    let mut stream = SynTokenStream::new();
    match fields {
        Fields::Named(named) => for field in &named.named {
            let name = &field.ident;
            stream.extend(create_ret_callback(quote!(&self.#name)))
        },
        Fields::Unnamed(unnamed) => for (i, _) in unnamed.unnamed.iter().enumerate() {
            stream.extend(create_ret_callback(quote!(&self.#i)))
        },
        Fields::Unit => { }
    }
    stream
}
fn i_ident(i: usize) -> Ident {
    Ident::new(&format!("field_{}", i), Span::call_site())
}
fn create_enum_fn_body(variants: &Punctuated<Variant, Token![,]>) -> SynTokenStream {
    let mut stream = SynTokenStream::new();
    for variant in variants {
        let name = &variant.ident;
        stream.extend(match &variant.fields {
            Fields::Named(named) => {
                let mut matches = SynTokenStream::new();
                let mut body = SynTokenStream::new();
                for (i, field) in named.named.iter().enumerate() {
                    let i = i_ident(i);
                    let field_name = &field.ident;
                    matches.extend(quote!(#field_name: #i,));
                    body.extend(create_ret_callback(i));
                }
                quote!(Self::#name { #matches } => { #body })
            },
            Fields::Unnamed(unnamed) => {
                let mut matches = SynTokenStream::new();
                let mut body = SynTokenStream::new();
                for (i, _) in unnamed.unnamed.iter().enumerate() {
                    let i = i_ident(i);
                    matches.extend(quote!(#i,));
                    body.extend(create_ret_callback(i));
                }
                quote!(Self::#name(#matches) => { #body })
            },
            Fields::Unit => quote!(Self::#name => { }),
        })
    }
    quote! {
        match self {
            #stream
        }
    }
}

pub fn derive_event_dispatch(input: TokenStream) -> TokenStream {
    let input: DeriveInput = parse_macro_input!(input);
    let body = match input.data {
        Data::Struct(data) => create_struct_fn_body(&data.fields),
        Data::Enum(data) => create_enum_fn_body(&data.variants),
        Data::Union(_) => {
            input.span()
                .unstable()
                .error("EventDispatch can only be derived from a struct or enum.")
                .emit();
            return TokenStream::new()
        }
    };

    let (impl_bounds, ty_param, where_bounds) = input.generics.split_for_impl();
    let name = input.ident;
    TokenStream::from(quote! {
        impl #impl_bounds ::static_events::handlers::RawEventDispatch
            for #name #ty_param #where_bounds
        {
            fn on_phase<
                __EventType: ::static_events::Event,
                __EventPhase: ::static_events::handlers::EventPhase,
                __EventDispatch: ::static_events::EventDispatch,
            >(
                &self,
                target: &__EventDispatch, ev: &mut __EventType, state: &mut __EventType::State,
            ) -> ::static_events::EventResult {
                #body
                ::static_events::EvOk
            }
        }
    })
}
