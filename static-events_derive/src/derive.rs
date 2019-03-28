use proc_macro::TokenStream;
use proc_macro2::{TokenStream as SynTokenStream, Span as SynSpan};
use syn::*;
use syn::export::Span;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use quote::*;

fn dispatch_on_phase(field: impl ToTokens, tp: impl ToTokens, is_async: bool) -> SynTokenStream {
    let call_fn = if is_async { quote! { on_phase_async } } else { quote! { on_phase } };
    let call = quote! {
        ::static_events::private::#call_fn::<
            #tp, __EventDispatch, __EventType, __EventPhase,
            ::static_events::handlers::DefaultHandler,
        >(#field, target, ev, state)
    };
    let call = if is_async { quote! { r#await!(#call) } } else { call };
    quote! {{
        let dispatch_result = #call;
        match dispatch_result {
            ::static_events::EvOk => { }
            e => return e,
        }
    }}
}
fn dispatch_get_service(field: impl ToTokens) -> SynTokenStream {
    quote! {
        match ::static_events::handlers::Events::get_service::<__DowncastTarget>(#field) {
            Some(t) => return Some(t),
            None => { }
        }
    }
}
fn is_implemented(tp: impl ToTokens) -> SynTokenStream {
    quote! {
        || ::static_events::private::is_implemented::<
            #tp, __EventDispatch, __EventType, __EventPhase,
            ::static_events::handlers::DefaultHandler,
        >()
    }
}

type BodyImpls = (SynTokenStream, SynTokenStream, SynTokenStream, SynTokenStream);
fn create_struct_fn_body(fields: &Fields) -> BodyImpls {
    let mut on_phase_body = SynTokenStream::new();
    let mut on_phase_async_body = SynTokenStream::new();
    let mut get_service_body = SynTokenStream::new();
    let mut is_implemented_expr = quote! { false };
    match fields {
        Fields::Named(named) => for field in &named.named {
            let name = &field.ident;
            on_phase_body.extend(dispatch_on_phase(quote!(&self.#name), &field.ty, false));
            on_phase_async_body.extend(dispatch_on_phase(quote!(&self.#name), &field.ty, true));
            get_service_body.extend(dispatch_get_service(quote!(&self.#name)));
            is_implemented_expr.extend(is_implemented(&field.ty));
        },
        Fields::Unnamed(unnamed) => for (i, field) in unnamed.unnamed.iter().enumerate() {
            on_phase_body.extend(dispatch_on_phase(quote!(&self.#i), &field.ty, false));
            on_phase_async_body.extend(dispatch_on_phase(quote!(&self.#i), &field.ty, true));
            is_implemented_expr.extend(is_implemented(&field.ty));
        },
        Fields::Unit => { }
    }
    (on_phase_body, on_phase_async_body, get_service_body, is_implemented_expr)
}
fn i_ident(i: usize) -> Ident {
    Ident::new(&format!("field_{}", i), Span::call_site())
}
fn create_enum_fn_body(variants: &Punctuated<Variant, Token![,]>) -> BodyImpls {
    let mut on_phase_body = SynTokenStream::new();
    let mut on_phase_async_body = SynTokenStream::new();
    let mut get_service_body = SynTokenStream::new();
    let mut is_implemented_expr = quote! { false };
    for variant in variants {
        let name = &variant.ident;
        let (on_phase_arm, on_phase_async_arm, get_service_arm) = match &variant.fields {
            Fields::Named(named) => {
                let mut matches = SynTokenStream::new();
                let mut on_phase = SynTokenStream::new();
                let mut on_phase_async = SynTokenStream::new();
                let mut get_service = SynTokenStream::new();
                for (i, field) in named.named.iter().enumerate() {
                    let i = i_ident(i);
                    let field_name = &field.ident;
                    matches.extend(quote!(#field_name: #i,));
                    on_phase.extend(dispatch_on_phase(&i, &field.ty, false));
                    on_phase_async.extend(dispatch_on_phase(&i, &field.ty, true));
                    get_service.extend(dispatch_get_service(&i));
                    is_implemented_expr.extend(is_implemented(&field.ty));
                }
                (quote!(Self::#name { #matches } => { #on_phase }),
                 quote!(Self::#name { #matches } => { #on_phase_async }),
                 quote!(Self::#name { #matches } => { #get_service }))
            },
            Fields::Unnamed(unnamed) => {
                let mut matches = SynTokenStream::new();
                let mut on_phase = SynTokenStream::new();
                let mut on_phase_async = SynTokenStream::new();
                let mut get_service = SynTokenStream::new();
                for (i, field) in unnamed.unnamed.iter().enumerate() {
                    let i = i_ident(i);
                    matches.extend(quote!(#i,));
                    on_phase.extend(dispatch_on_phase(&i, &field.ty, false));
                    on_phase_async.extend(dispatch_on_phase(&i, &field.ty, true));
                    get_service.extend(dispatch_get_service(&i));
                    is_implemented_expr.extend(is_implemented(&field.ty));
                }
                (quote!(Self::#name(#matches) => { #on_phase }),
                 quote!(Self::#name(#matches) => { #on_phase_async }),
                 quote!(Self::#name(#matches) => { #get_service }))
            },
            Fields::Unit => (quote!(Self::#name => { }),
                             quote!(Self::#name => { }),
                             quote!(Self::#name => { })),
        };
        on_phase_body.extend(on_phase_arm);
        on_phase_async_body.extend(on_phase_async_arm);
        get_service_body.extend(get_service_arm);
    }
    (
        quote! { match self { #on_phase_body } },
        quote! { match self { #on_phase_async_body } },
        quote! { match self { #get_service_body } },
        is_implemented_expr,
    )
}

pub fn derive_events(input: TokenStream) -> TokenStream {
    let input: DeriveInput = parse_macro_input!(input);
    let (on_phase_body, on_phase_async_body, get_service_body, is_implemented_expr) =
        match input.data {
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

    let handler_generics = parse2::<Generics>(quote! { <
        __EventDispatch: ::static_events::Events,
        __EventType: ::static_events::Event,
        __EventPhase: ::static_events::handlers::EventPhase,
    > }).unwrap();
    let handler_generics = crate::merge_generics(&handler_generics, &input.generics);
    let (handler_impl_bounds, _, handler_where_bounds) =
        handler_generics.split_for_impl();

    let (event_impl_bounds, ty_param, event_where_bounds) = input.generics.split_for_impl();

    let name = input.ident;

    let impl_id = crate::impl_id();
    let impl_name = Ident::new(&format!("__ImplEvents_{}", impl_id), SynSpan::call_site());
    let fn_name = Ident::new(&format!("__ImplEvents_future_{}", impl_id), SynSpan::call_site());
    let fn_wrap_name = Ident::new(&format!("__ImplEvents_futurewrap_{}", impl_id), SynSpan::call_site());
    TokenStream::from(quote! {
        const #impl_name: () = {
            impl #event_impl_bounds ::static_events::Events
                for #name #ty_param #event_where_bounds
            {
                fn get_service<__DowncastTarget>(&self) -> Option<&__DowncastTarget> {
                    #get_service_body
                    None
                }
            }

            impl #event_impl_bounds #name #ty_param #event_where_bounds {
                async fn #fn_name <
                    '__EventLifetime,
                    __EventDispatch: ::static_events::Events,
                    __EventType: ::static_events::Event,
                    __EventPhase: ::static_events::handlers::EventPhase,
                > (
                    &'__EventLifetime self,
                    target: &'__EventLifetime ::static_events::Handler<__EventDispatch>,
                    ev: &'__EventLifetime mut __EventType,
                    state: &'__EventLifetime mut __EventType::State,
                ) -> ::static_events::EventResult {
                    #on_phase_async_body
                    ::static_events::EvOk
                }

                unsafe async fn #fn_wrap_name<
                    __EventDispatch: ::static_events::Events,
                    __EventType: ::static_events::Event,
                    __EventPhase: ::static_events::handlers::EventPhase,
                >(
                    ctx: ::static_events::handlers::AsyncDispatchContext<
                        Self, __EventDispatch, __EventType
                    >
                ) -> ::static_events::EventResult {
                    let future = ctx.this().#fn_name::<
                        __EventDispatch, __EventType, __EventPhase,
                    >(ctx.target(), ctx.ev(), ctx.state());
                    r#await!(future)
                }
            }

            existential type __ExistentialEventFuture #handler_generics:
                ::std::future::Future<Output = ::static_events::EventResult>;

            impl #handler_impl_bounds ::static_events::handlers::EventHandler<
                __EventDispatch, __EventType, __EventPhase,
            > for #name #ty_param #handler_where_bounds {
                const IS_IMPLEMENTED: bool = #is_implemented_expr;

                fn on_phase(
                    &self, target: &::static_events::Handler<__EventDispatch>,
                    ev: &mut __EventType, state: &mut __EventType::State,
                ) -> ::static_events::EventResult {
                    #on_phase_body
                    ::static_events::EvOk
                }

                type FutureType = __ExistentialEventFuture<
                    __EventDispatch, __EventType, __EventPhase,
                >;
                unsafe fn on_phase_async(
                    ctx: ::static_events::handlers::AsyncDispatchContext<
                        Self, __EventDispatch, __EventType
                    >,
                ) -> Self::FutureType {
                    Self::#fn_wrap_name::<__EventDispatch, __EventType, __EventPhase>(ctx)
                }
            }

            ()
        };
    })
}
