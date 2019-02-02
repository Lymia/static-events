use proc_macro::TokenStream;
use proc_macro2::{TokenStream as SynTokenStream};
use syn::*;
use syn::export::Span;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use quote::*;

fn dispatch_on_phase(field: impl ToTokens) -> SynTokenStream {
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
fn dispatch_get_service(field: impl ToTokens) -> SynTokenStream {
    quote! {
        match ::static_events::handlers::RawEventDispatch::get_service::<__DowncastTarget>(#field) {
            Some(t) => return Some(t),
            None => { }
        }
    }
}

fn create_struct_fn_body(fields: &Fields) -> (SynTokenStream, SynTokenStream) {
    let mut on_phase_body = SynTokenStream::new();
    let mut get_service_body = SynTokenStream::new();
    match fields {
        Fields::Named(named) => for field in &named.named {
            let name = &field.ident;
            on_phase_body.extend(dispatch_on_phase(quote!(&self.#name)));
            get_service_body.extend(dispatch_get_service(quote!(&self.#name)));
        },
        Fields::Unnamed(unnamed) => for (i, _) in unnamed.unnamed.iter().enumerate() {
            on_phase_body.extend(dispatch_on_phase(quote!(&self.#i)));
            get_service_body.extend(dispatch_get_service(quote!(&self.#i)));
        },
        Fields::Unit => { }
    }
    (on_phase_body, get_service_body)
}
fn i_ident(i: usize) -> Ident {
    Ident::new(&format!("field_{}", i), Span::call_site())
}
fn create_enum_fn_body(
    variants: &Punctuated<Variant, Token![,]>,
) -> (SynTokenStream, SynTokenStream) {
    let mut on_phase_body = SynTokenStream::new();
    let mut get_service_body = SynTokenStream::new();
    for variant in variants {
        let name = &variant.ident;
        let (on_phase_arm, get_service_arm) = match &variant.fields {
            Fields::Named(named) => {
                let mut matches = SynTokenStream::new();
                let mut on_phase = SynTokenStream::new();
                let mut get_service = SynTokenStream::new();
                for (i, field) in named.named.iter().enumerate() {
                    let i = i_ident(i);
                    let field_name = &field.ident;
                    matches.extend(quote!(#field_name: #i,));
                    on_phase.extend(dispatch_on_phase(&i));
                    get_service.extend(dispatch_get_service(&i));
                }
                (quote!(Self::#name { #matches } => { #on_phase }),
                 quote!(Self::#name { #matches } => { #get_service }))
            },
            Fields::Unnamed(unnamed) => {
                let mut matches = SynTokenStream::new();
                let mut on_phase = SynTokenStream::new();
                let mut get_service = SynTokenStream::new();
                for (i, _) in unnamed.unnamed.iter().enumerate() {
                    let i = i_ident(i);
                    matches.extend(quote!(#i,));
                    on_phase.extend(dispatch_on_phase(&i));
                    get_service.extend(dispatch_get_service(&i));
                }
                (quote!(Self::#name(#matches) => { #on_phase }),
                 quote!(Self::#name(#matches) => { #get_service }))
            },
            Fields::Unit => (quote!(Self::#name => { }),
                             quote!(Self::#name => { })),
        };
        on_phase_body.extend(on_phase_arm);
        get_service_body.extend(get_service_arm);
    }
    (quote! { match self { #on_phase_body } }, quote! { match self { #get_service_body } })
}

pub fn derive_event_dispatch(input: TokenStream) -> TokenStream {
    let input: DeriveInput = parse_macro_input!(input);
    let (on_phase_body, get_service_body) = match input.data {
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
                #on_phase_body
                ::static_events::EvOk
            }

            fn get_service<__DowncastTarget>(&self) -> Option<&__DowncastTarget> {
                #get_service_body
                None
            }
        }
    })
}
