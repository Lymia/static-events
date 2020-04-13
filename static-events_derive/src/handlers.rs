use crate::common::*;
use proc_macro::TokenStream;
use proc_macro2::{TokenStream as SynTokenStream, Span as SynSpan};
use syn::{*, Result};
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

    fn check_reference_type(tp: &Type) -> Result<HandlerArg> {
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
    fn check_any_type(tp: &Type) -> Result<HandlerArg> {
        match tp {
            Type::Reference(ref_tp) => Self::check_reference_type(&ref_tp.elem),
            Type::Paren(paren) => Self::check_any_type(&paren.elem),
            Type::Group(group) => Self::check_any_type(&group.elem),
            _ => error(tp.span(), "Event handlers must take parameters by reference."),
        }
    }

    fn from_param(param: &FnArg) -> Result<HandlerArg> {
        match param {
            FnArg::Receiver(receiver) => if receiver.reference.is_none() {
                error(param.span(), "Event handlers may not take `self` by value.")
            } else if receiver.mutability.is_some() {
                error(param.span(), "Event handlers may not take `self` by mutable reference.")
            } else {
                Ok(HandlerArg::SelfParam)
            },
            FnArg::Typed(ty) => Self::check_any_type(&ty.ty),
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
    fn find_signature(method: &ImplItemMethod) -> Result<HandlerSig> {
        let sig = &method.sig;
        if sig.variadic.is_some() {
            error(sig.span(), "Event handlers cannot be variadic.")?;
        }

        let mut parsed_params = Vec::new();
        for arg in &sig.inputs {
            parsed_params.push((HandlerArg::from_param(arg)?, arg.span()));
        }
        let mut params = parsed_params.into_iter().peekable();

        let mut handler_sig = HandlerSig {
            fn_name: sig.ident.clone(),
            method_generics: sig.generics.clone(),
            self_param: None,
            target_param: None,
            event_ty: Type::Verbatim(SynTokenStream::new()),
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
            error(sig.span(), "No event parameter found for event handler.")?;
        }
        if let Some((HandlerArg::OtherParam(_), _)) = params.peek() {
            handler_sig.state_param = Some(params.next().unwrap().1);
        }
        if let Some((_, span)) = params.next() {
            error(span, "Unexpected parameter in event handler signature.")?;
        }

        Ok(handler_sig)
    }

    fn make_body(&self, self_ty: &Type) -> EventHandlerBody {
        let name = &self.fn_name;
        let call = match self.self_param {
            Some(_) => quote! { _self.#name },
            None    => quote! { <#self_ty>::#name },
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
            EventHandlerBody::Async(quote! { (#call).await })
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
    fn for_attr(attr: &Attribute) -> Result<Option<HandlerType>> {
        match last_path_segment(&attr.path).as_str() {
            "event_handler" => {
                Ok(Some(HandlerType::EventHandler(if !attr.tokens.is_empty() {
                    match parse2::<TypeParen>(attr.tokens.clone()) {
                        Ok(tp) => tp.elem.into_token_stream(),
                        Err(_) => error(attr.span(), "Error parsing #[event_handler] attribute.")?,
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
    fn for_method(method: &ImplItemMethod) -> Result<Option<MethodInfo>> {
        let mut handler_type: Option<HandlerType> = None;
        for attr in &method.attrs {
            if let Some(tp) = HandlerType::for_attr(attr)? {
                if let Some(e_tp) = &handler_type {
                    error(
                        attr.span(),
                        if e_tp.name() == tp.name() {
                            format!("{} can only be used once.", tp.name())
                        } else {
                            format!("{} cannot be used with {}.", tp.name(), e_tp.name())
                        }
                    )?;
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
    id: usize, count: usize,
    self_ty: &Type, impl_generics: &Generics, phase: &SynTokenStream, sig: &HandlerSig,
) -> (Ident, SynTokenStream) {
    let phantom = ident!("__DistinguisherPhantom_{}", id);
    let async_fn = ident!("__events_async_handler_wrapper_{}", id);
    let existential = ident!("__RootEventFut_{}", id);

    let event_ty = &sig.event_ty;
    let event_ty_event = quote! { <#event_ty as ::static_events::Event> };

    let method_generics = strip_lifetimes(&sig.method_generics);
    let merged_generics = merge_generics(impl_generics, &method_generics);

    let async_generics = generics(&quote! {
        '__EventLifetime,
        __EventDispatch: ::static_events::Events,
    });
    let async_generics = merge_generics(&merged_generics, &async_generics);
    let (async_bounds, async_ty, async_where_bounds) =
        async_generics.split_for_impl();

    let (is_async, sync_body, future_ty, async_defs, async_body) = match sig.make_body(self_ty) {
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
                    #async_fn(self, _target, _ev, _state)
                )
            },
            quote! { #existential #async_ty },
            quote! {
                async fn #async_fn #async_bounds (
                    _self: &'__EventLifetime #self_ty,
                    _target: &'__EventLifetime ::static_events::Handler<__EventDispatch>,
                    _ev: &'__EventLifetime mut #event_ty,
                    _state: &'__EventLifetime mut #event_ty_event::State,
                ) -> ::static_events::EventResult #async_where_bounds {
                    use ::static_events::Event;
                    let ev_result = (#async_body).into();
                    _ev.to_event_result(_state, ev_result)
                }

                type #existential #async_generics =
                    impl ::std::future::Future<Output = ::static_events::EventResult> +
                    '__EventLifetime;
            },
            quote! {
                #async_fn(self, target, ev, state)
            },
        ),
    };

    let phantom_impl = if count == 1 {
        quote! { pub type #phantom = ::static_events::private::HandlerImplBlock; }
    } else {
        quote! { pub enum #phantom { } }
    };

    (phantom.clone(), quote! {
        #phantom_impl
        #async_defs
        impl #async_bounds ::static_events::handlers::EventHandler<
            '__EventLifetime, __EventDispatch, #event_ty, #phase, #phantom,
        > for #self_ty #async_where_bounds {
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
    })
}
fn create_impls(
    self_ty: &Type, impl_generics: &Generics, methods: &[MethodInfo],
) -> SynTokenStream {
    let mut impls = SynTokenStream::new();
    let mut stages = Vec::new();

    for (i, info) in methods.iter().enumerate() {
        match info {
            MethodInfo::EventHandler { phase, sig } => {
                let (phantom, tts) = create_normal_handler(
                    i, methods.len(), self_ty, impl_generics, phase, sig,
                );
                impls.extend(tts);
                stages.push(CallStage::new(quote!(_this), self_ty, Some(quote!(#phantom))));
            }
        }
    }
    let group = CallGroup::new(quote!(_), stages);
    if methods.len() > 1 {
        impls.extend(
            make_merge_event_handler(
                EventHandlerTarget::Type(self_ty), impl_generics,
                Some(quote! { ::static_events::private::HandlerImplBlock }),
                vec![group], Vec::new(),
            )
        );
    }

    quote! {
        #[allow(non_snake_case)]
        const _: () = {
            #impls
        };
    }
}

fn mark_attrs_processed(method: &mut ImplItemMethod) {
    let ident = Ident::new(&crate::RAND_IDENT, SynSpan::call_site());
    for attr in &mut method.attrs {
        if HandlerType::is_attr(attr) {
            attr.tokens = quote! { (#ident) };
        }
    }
}

pub fn events_impl(_: TokenStream, item: TokenStream) -> TokenStream {
    let mut impl_block = match parse::<ItemImpl>(item.clone()) {
        Ok(block) => block,
        Err(_) => {
            let item: SynTokenStream = item.into();
            let e = Error::new(item.span(), "#[events_impl] can only be used on impl blocks.");
            return e.to_compile_error().into();
        },
    };

    let mut handlers = Vec::new();
    let mut errors = Vec::new();

    for item in &mut impl_block.items {
        match item {
            ImplItem::Method(method) => {
                match MethodInfo::for_method(method) {
                    Ok(Some(handler)) => handlers.push(handler),
                    Ok(None) => { }
                    Err(e) => errors.push(e),
                }
                mark_attrs_processed(method);
            },
            _ => { }
        }
    }
    if !errors.is_empty() {
        let errors: Vec<_> = errors.into_iter().map(|x| x.to_compile_error()).collect();
        return TokenStream::from(quote! {
            #(#errors)*
            #impl_block
        })
    }

    let const_block = create_impls(&impl_block.self_ty, &impl_block.generics, &handlers);
    TokenStream::from(quote! {
        #impl_block
        #const_block
    })
}
