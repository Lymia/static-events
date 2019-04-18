use crate::common::*;
use proc_macro::TokenStream;
use proc_macro2::{TokenStream as SynTokenStream, Span as SynSpan};
use std::result::Result;
use syn::*;
use syn::spanned::Spanned;
use quote::{quote, ToTokens};

// TODO: Figure out what to do with elided type parameters in function arguments.
// TODO: Properly handle lifetimes in general for method arguments.

#[derive(Debug)]
enum HandlerArg {
    SelfParam,
    TargetParam,
    OtherParam(Type),
}
impl HandlerArg {
    fn generic_argument_contains_impl(arg: &GenericArgument) -> bool {
        match arg {
            GenericArgument::Type(tp) => Self::type_contains_impl(tp),
            _ => false,
        }
    }
    fn path_segment_contains_impl(seg: &PathSegment) -> bool {
        match &seg.arguments {
            PathArguments::None => false,
            PathArguments::AngleBracketed(paren) =>
                paren.args.iter().any(|x| Self::generic_argument_contains_impl(x)),
            PathArguments::Parenthesized(_) => false,
        }
    }
    fn type_contains_impl(tp: &Type) -> bool {
        match tp {
            Type::Paren(paren) => Self::type_contains_impl(&paren.elem),
            Type::Group(group) => Self::type_contains_impl(&group.elem),
            Type::Path(path) =>
                path.path.segments.iter().any(|x| Self::path_segment_contains_impl(x)),
            Type::ImplTrait(_) => true,
            _ => false,
        }
    }

    fn check_reference_type(tp: &Type) -> Result<HandlerArg, ()> {
        match tp {
            Type::Paren(paren) => Self::check_reference_type(&paren.elem),
            Type::Group(group) => Self::check_reference_type(&group.elem),
            Type::Path(_) => Ok(if Self::type_contains_impl(tp) {
                HandlerArg::TargetParam
            } else {
                HandlerArg::OtherParam((*tp).clone())
            }),
            _ => Ok(HandlerArg::OtherParam((*tp).clone())),
        }
    }
    fn check_any_type(tp: &Type) -> Result<HandlerArg, ()> {
        match tp {
            Type::Reference(ref_tp) => Self::check_reference_type(&ref_tp.elem),
            Type::Paren(paren) => Self::check_any_type(&paren.elem),
            Type::Group(group) => Self::check_any_type(&group.elem),
            _ => {
                tp.span().unstable()
                    .error("Event handlers must take parameters by reference.")
                    .emit();
                Err(())
            }
        }
    }

    fn from_param(param: &FnArg) -> Result<HandlerArg, ()> {
        match param {
            FnArg::SelfValue(_) => {
                param.span().unstable()
                    .error("Event handlers may not take `self` by value.")
                    .emit();
                Err(())
            },
            FnArg::SelfRef(self_ref) => if self_ref.mutability.is_some() {
                param.span().unstable()
                    .error("Event handlers may not take `self` by mutable reference.")
                    .emit();
                Err(())
            } else {
                Ok(HandlerArg::SelfParam)
            },
            FnArg::Ignored(ty) | FnArg::Captured(ArgCaptured { ty, .. }) =>
                Self::check_any_type(ty),
            FnArg::Inferred(_) => unreachable!(),
        }
    }
}

enum EventHandlerBody {
    Sync(SynTokenStream), Async(SynTokenStream),
}

struct HandlerSig {
    fn_name: Ident,
    method_generics: Generics,
    self_param: Option<SynSpan>,
    target_param: Option<SynSpan>,
    event_ty: Type,
    state_param: Option<SynSpan>,
    is_unsafe: bool,
    is_async: bool,
}
impl HandlerSig {
    fn find_signature(method: &ImplItemMethod) -> Result<HandlerSig, ()> {
        let sig = &method.sig;
        if sig.decl.variadic.is_some() {
            sig.span().unstable()
                .error("Event handlers cannot be variadic.")
                .emit();
            return Err(())
        }

        let mut parsed_params = Vec::new();
        for arg in &sig.decl.inputs {
            parsed_params.push((HandlerArg::from_param(arg)?, arg.span()));
        }
        let mut params = parsed_params.into_iter().peekable();

        let mut handler_sig = HandlerSig {
            fn_name: sig.ident.clone(),
            method_generics: sig.decl.generics.clone(),
            self_param: None,
            target_param: None,
            event_ty: Type::Verbatim(TypeVerbatim { tts: SynTokenStream::new() }),
            state_param: None,
            is_unsafe: sig.unsafety.is_some(),
            is_async: sig.asyncness.is_some(),
        };

        if let Some((HandlerArg::SelfParam, _)) = params.peek() {
            handler_sig.self_param = Some(params.next().unwrap().1);
        }
        if let Some((HandlerArg::TargetParam, _)) = params.peek() {
            handler_sig.target_param = Some(params.next().unwrap().1);
        }
        if let Some((HandlerArg::OtherParam(ty), _)) = params.next() {
            handler_sig.event_ty = ty;
        } else {
            sig.span().unstable()
                .error("No event parameter found for event handler.")
                .emit();
            return Err(())
        }
        if let Some((HandlerArg::OtherParam(_), _)) = params.peek() {
            handler_sig.state_param = Some(params.next().unwrap().1);
        }
        if let Some((_, span)) = params.next() {
            span.unstable()
                .error("Unexpected parameter in event handler signature.")
                .emit();
            return Err(())
        }

        Ok(handler_sig)
    }

    fn make_body(&self) -> EventHandlerBody {
        let name = &self.fn_name;
        let call = match self.self_param {
            Some(_) => quote! { self.#name },
            None    => quote! { Self::#name },
        };
        let target = match self.target_param {
            Some(_) => quote! { _target, },
            None    => quote! {},
        };
        let state = match self.state_param {
            Some(_) => quote! { _ev.borrow_state(_state), },
            None    => quote! {},
        };
        let call = quote! { #call (#target _ev, #state) };
        let call = if !self.is_unsafe {
            call
        } else {
            quote! { unsafe { #call } }
        };
        if self.is_async {
            EventHandlerBody::Async(quote! { r#await!(#call) })
        } else {
            EventHandlerBody::Sync(call)
        }
    }
}

enum HandlerType {
    EventHandler(SynTokenStream),
}
impl HandlerType {
    fn is_attr(attr: &Attribute) -> bool {
        match last_path_segment(&attr.path).as_str() {
            "event_handler" => true,
            _ => false,
        }
    }
    fn for_attr(attr: &Attribute) -> Result<Option<HandlerType>, ()> {
        match last_path_segment(&attr.path).as_str() {
            "event_handler" => {
                Ok(Some(HandlerType::EventHandler(if !attr.tts.is_empty() {
                    match parse2::<TypeParen>(attr.tts.clone()) {
                        Ok(tp) => tp.elem.into_token_stream(),
                        Err(_) => {
                            attr.span()
                                .unstable()
                                .error(format!("Could not parse #[event_handler] attribute."))
                                .emit();
                            return Err(())
                        },
                    }
                } else {
                    quote! { ::static_events::EvOnEvent }
                })))
            },
            _ => Ok(None),
        }
    }
    fn name(&self) -> &'static str {
        match self {
            HandlerType::EventHandler(_) => "#[event_handler]",
        }
    }
}

enum MethodInfo {
    EventHandler { phase: SynTokenStream, sig: HandlerSig },
}
impl MethodInfo {
    fn for_method(method: &ImplItemMethod) -> Result<Option<MethodInfo>, ()> {
        let mut handler_type: Option<HandlerType> = None;
        for attr in &method.attrs {
            if let Some(tp) = HandlerType::for_attr(attr)? {
                if let Some(e_tp) = &handler_type {
                    if e_tp.name() == tp.name() {
                        attr.span().unstable()
                            .error(format!("{} can only be used once.", tp.name()))
                            .emit();
                    } else {
                        attr.span().unstable()
                            .error(format!("{} cannot be used with {}.",
                                           tp.name(), e_tp.name()))
                            .emit();
                    }
                    return Err(())
                }
                handler_type = Some(tp);
            }
        }
        match handler_type {
            Some(HandlerType::EventHandler(phase)) => {
                let sig = HandlerSig::find_signature(method)?;
                Ok(Some(MethodInfo::EventHandler { phase, sig }))
            }
            None => Ok(None),
        }
    }
}

fn create_normal_handler(
    ctx: &GensymContext, self_ty: &Type, impl_generics: &Generics, phantom: SynTokenStream,
    phase: &SynTokenStream, sig: &HandlerSig,
) -> SynTokenStream {
    let phantom = phantom.into_token_stream();
    let ctx = ctx.derive(&phantom);

    let fn_async = ctx.gensym("async_handler");
    let existential = ctx.gensym("FutureTypeExistential");

    let event_ty = &sig.event_ty;
    let event_ty_event = quote! { <#event_ty as ::static_events::Event> };

    let method_generics = strip_lifetimes(&sig.method_generics);
    let merged_generics = merge_generics(impl_generics, &method_generics);

    let async_generics_raw = quote! {
        '__EventLifetime,
        __EventDispatch: ::static_events::Events,
    };
    let async_generics = generics(&async_generics_raw);
    let async_generics = merge_generics(&method_generics, &async_generics);

    let handler_generics = generics(&async_generics_raw);
    let handler_generics = merge_generics(&merged_generics, &handler_generics);

    let (self_impl_bounds, _, self_where_bounds) =
        impl_generics.split_for_impl();
    let (async_bounds, _, async_where_bounds) =
        async_generics.split_for_impl();
    let (handler_impl_bounds, handler_ty_param, handler_where_bounds) =
        handler_generics.split_for_impl();

    let async_generics_no_lifetimes = strip_lifetimes(&async_generics);
    let async_ty_params = async_generics_no_lifetimes.split_for_impl().1;
    let async_turbofish = async_ty_params.as_turbofish();

    let (is_async, sync_body, future_ty, async_defs, async_body) = match sig.make_body() {
        EventHandlerBody::Sync(sync_body) => (
            false, sync_body,
            quote! { ::static_events::private::NullFuture },
            quote! { },
            quote! { ::static_events::private::event_error() },
        ),
        EventHandlerBody::Async(async_body) => (
            true,
            quote! {
                ::static_events::private::block_on(
                    self.#fn_async #async_turbofish(_target, _ev, _state)
                )
            },
            quote! { #existential #handler_ty_param },
            quote! {
                impl #self_impl_bounds #self_ty #self_where_bounds {
                    async fn #fn_async #async_bounds (
                        &'__EventLifetime self,
                        _target: &'__EventLifetime ::static_events::Handler<__EventDispatch>,
                        _ev: &'__EventLifetime mut #event_ty,
                        _state: &'__EventLifetime mut #event_ty_event::State,
                    ) -> ::static_events::EventResult #async_where_bounds {
                        use ::static_events::Event;
                        let ev_result = (#async_body).into();
                        _ev.to_event_result(_state, ev_result)
                    }
                }

                existential type #existential #handler_generics:
                    ::std::future::Future<Output = ::static_events::EventResult> + '__EventLifetime;
            },
            quote! {
                self.#fn_async #async_turbofish(target, ev, state)
            },
        ),
    };

    quote! {
        #async_defs

        impl #handler_impl_bounds ::static_events::handlers::EventHandler<
            '__EventLifetime, __EventDispatch, #event_ty, #phase, #phantom,
        > for #self_ty #handler_where_bounds {
            const IS_IMPLEMENTED: bool = true;
            const IS_ASYNC: bool = #is_async;

            #[inline]
            fn on_phase(
                &'__EventLifetime self,
                _target: &'__EventLifetime ::static_events::Handler<__EventDispatch>,
                _ev: &'__EventLifetime mut #event_ty,
                _state: &'__EventLifetime mut #event_ty_event::State,
            ) -> ::static_events::EventResult {
                use ::static_events::Event;
                let ev_result = (#sync_body).into();
                _ev.to_event_result(_state, ev_result)
            }

            type FutureType = #future_ty;

            #[inline]
            fn on_phase_async (
                &'__EventLifetime self,
                target: &'__EventLifetime ::static_events::Handler<__EventDispatch>,
                ev: &'__EventLifetime mut #event_ty,
                state: &'__EventLifetime mut #event_ty_event::State,
            ) -> #future_ty {
                use ::static_events::Event;
                #async_body
            }
        }
    }
}
fn create_impls(
    ctx: &GensymContext, self_ty: &Type, impl_generics: &Generics, methods: &[MethodInfo],
) -> SynTokenStream {
    let dist = quote! { ::static_events::private::HandlerImplBlock };

    let mut impls = SynTokenStream::new();
    let mut stages = Vec::new();
    let mut phantoms = Vec::new();

    for (i, info) in methods.iter().enumerate() {
        match info {
            MethodInfo::EventHandler { phase, sig } => {
                let phantom = ctx.gensym_id("PhantomMarker", i);
                impls.extend(create_normal_handler(
                    ctx, self_ty, impl_generics, quote!(#phantom), phase, sig,
                ));
                stages.push(CallStage::new(quote!(this), self_ty, Some(quote!(#phantom))));
                phantoms.push(phantom);
            }
        }
    }
    let group = CallGroup::new(quote!(true), stages);
    if phantoms.len() == 1 {
        let phantom = phantoms.pop().unwrap();
        impls.extend(quote! { type #phantom = #dist; });
    } else if phantoms.len() > 1 {
        for phantom in phantoms {
            impls.extend(quote! { enum #phantom { } });
        }
        impls.extend(
            make_merge_event_handler(ctx, EventHandlerTarget::Type(self_ty), impl_generics,
                                     Some(dist), vec![group], Vec::new())
        );
    }

    let impl_name = ctx.gensym("ImplBlocksWrapper");
    quote! {
        #[allow(non_snake_case)]
        const #impl_name: () = {
            #impls
            ()
        };
    }
}

fn mark_attrs_processed(method: &mut ImplItemMethod) {
    let ident = Ident::new(&crate::RAND_IDENT, SynSpan::call_site());
    for attr in &mut method.attrs {
        if HandlerType::is_attr(attr) {
            attr.tts = quote! { (#ident) };
        }
    }
}

pub fn events_impl(attr: TokenStream, item: TokenStream) -> TokenStream {
    let ctx = GensymContext::new(&format_args!("({})({})", attr, item));
    let mut impl_block = match parse::<ItemImpl>(item.clone()) {
        Ok(block) => block,
        Err(_) => {
            stream_span(item.clone())
                .error("#[event_dispatch] can only be used on impl blocks.")
                .emit();
            return item
        },
    };

    let mut handlers = Vec::new();
    let mut has_errors = false;

    for item in &mut impl_block.items {
        match item {
            ImplItem::Method(method) => {
                match MethodInfo::for_method(method) {
                    Ok(Some(handler)) => handlers.push(handler),
                    Ok(None) => { }
                    Err(()) => has_errors = true,
                }
                mark_attrs_processed(method);
            },
            _ => { }
        }
    }
    if has_errors {
        return TokenStream::from(quote! { #impl_block })
    }

    let const_block = create_impls(&ctx, &impl_block.self_ty, &impl_block.generics, &handlers);
    TokenStream::from(quote! {
        #impl_block
        #const_block
    })
}