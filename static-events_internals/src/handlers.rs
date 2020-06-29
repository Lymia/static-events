use crate::common::*;
use crate::errors::{Error, Result};
use crate::utils::*;
use proc_macro2::{TokenStream as SynTokenStream, Span as SynSpan};
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
    fn find_signature(method: &ImplItemMethod, is_handler_async: bool) -> Result<HandlerSig> {
        let sig = &method.sig;
        if sig.variadic.is_some() {
            error(sig.span(), "Event handlers cannot be variadic.")?;
        }
        if sig.asyncness.is_some() && !is_handler_async {
            error(
                sig.asyncness.span(),
                "`async` handlers cannot be included in synchronous event handlers.",
            )?;
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

    fn make_body(
        &self, crate_name: &SynTokenStream, self_ty: &Type, is_async_handler: bool,
    ) -> EventHandlerBody {
        let name = &self.fn_name;
        let call = match self.self_param {
            Some(_) => quote! { _self.#name },
            None    => quote! { <#self_ty>::#name },
        };
        let target = match self.target_param {
            Some(_) => if is_async_handler {
                // this is a hack to allow the (slightly sloppy) `AsyncEvents` bound in sync
                // handlers defined in async events.
                quote! { #crate_name::private::handler_as_sync_handler(_target), }
            } else {
                quote! { _target, }
            },
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
    fn for_attr(crate_name: &SynTokenStream, attr: &Attribute) -> Result<Option<HandlerType>> {
        match last_path_segment(&attr.path).as_str() {
            "event_handler" => {
                Ok(Some(HandlerType::EventHandler(if !attr.tokens.is_empty() {
                    match parse2::<TypeParen>(attr.tokens.clone()) {
                        Ok(tp) => tp.elem.into_token_stream(),
                        Err(_) => error(attr.span(), "Error parsing #[event_handler] attribute.")?,
                    }
                } else {
                    quote! { #crate_name::handlers::EvOnEvent }
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
    fn for_method(
        crate_name: &SynTokenStream, method: &ImplItemMethod, is_async_handler: bool,
    ) -> Result<Option<MethodInfo>> {
        let mut handler_type: Option<HandlerType> = None;
        for attr in &method.attrs {
            if let Some(tp) = HandlerType::for_attr(crate_name, attr)? {
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
                let sig = HandlerSig::find_signature(method, is_async_handler)?;
                Ok(Some(MethodInfo::EventHandler { phase, sig }))
            }
            None => Ok(None),
        }
    }
}

fn create_normal_handler(
    crate_name: &SynTokenStream, discriminator: &SynTokenStream, is_async_handler: bool,
    id: usize, count: usize,
    self_ty: &Type, impl_generics: &Generics, phase: &SynTokenStream, sig: &HandlerSig,
) -> (Ident, SynTokenStream) {
    let phantom = ident!("__DistinguisherPhantom_{}", id);
    let async_fn = ident!("__events_async_handler_wrapper_{}", id);
    let existential = ident!("__RootEventFut_{}", id);

    let event_ty = &sig.event_ty;
    let event_ty_event = quote! { <#event_ty as #crate_name::events::Event> };

    let method_generics = strip_lifetimes(&sig.method_generics);
    let merged_generics = merge_generics(impl_generics, &method_generics);

    let sync_generics = generics(&quote! {
        '__EventLifetime,
        __EventDispatch: #crate_name::handlers::Events,
    });
    let sync_generics = merge_generics(&merged_generics, &sync_generics);
    let (sync_bounds, _, sync_where_bounds) = sync_generics.split_for_impl();

    let async_generics = generics(&quote! {
        '__EventLifetime,
        __EventDispatch: #crate_name::handlers::AsyncEvents,
    });
    let async_generics = merge_generics(&merged_generics, &async_generics);
    let (async_bounds, async_ty, async_where_bounds) = async_generics.split_for_impl();

    let body = sig.make_body(crate_name, self_ty, is_async_handler);
    let (is_async, sync_body, future_ty, async_defs, async_body) = match body {
        EventHandlerBody::Sync(sync_body) => (
            false,
            quote! {
                use #crate_name::events::Event;
                let _self = self;
                let ev_result = (#sync_body).into();
                _ev.to_event_result(_state, ev_result)
            },
            quote! { #crate_name::private::NullFuture },
            quote! { },
            quote! { #crate_name::private::event_error() },
        ),
        EventHandlerBody::Async(async_body) => (
            true,
            quote! { #crate_name::private::async_in_sync() },
            quote! { #existential #async_ty },
            quote! {
                async fn #async_fn #async_bounds (
                    _self: &'__EventLifetime #self_ty,
                    _target: &'__EventLifetime #crate_name::handlers::Handler<__EventDispatch>,
                    _ev: &'__EventLifetime mut #event_ty,
                    _state: &'__EventLifetime mut #event_ty_event::State,
                ) -> #crate_name::events::EventResult #async_where_bounds {
                    use #crate_name::events::Event;
                    let ev_result = (#async_body).into();
                    _ev.to_event_result(_state, ev_result)
                }

                #crate_name::private::allow_existentials! {
                    (#existential #async_generics)
                    (::std::future::Future<Output = #crate_name::events::EventResult> +
                        '__EventLifetime)
                }
            },
            quote! {
                #async_fn(self, target, ev, state)
            },
        ),
    };

    let phantom_impl = if count == 1 {
        quote! { pub type #phantom = #discriminator; }
    } else {
        quote! { pub enum #phantom { } }
    };
    let async_impl = if is_async_handler {
        quote! {
            impl #async_bounds #crate_name::handlers::AsyncEventHandler<
                '__EventLifetime, __EventDispatch, #event_ty, #phase, #phantom,
            > for #self_ty #async_where_bounds {
                type FutureType = #future_ty;

                #[inline]
                fn on_phase_async (
                    &'__EventLifetime self,
                    target: &'__EventLifetime #crate_name::handlers::Handler<__EventDispatch>,
                    ev: &'__EventLifetime mut #event_ty,
                    state: &'__EventLifetime mut #event_ty_event::State,
                ) -> #future_ty {
                    use #crate_name::events::Event;
                    #async_body
                }
            }
        }
    } else {
        SynTokenStream::new()
    };

    (phantom.clone(), quote! {
        #phantom_impl
        #async_defs
        impl #sync_bounds #crate_name::handlers::EventHandler<
            '__EventLifetime, __EventDispatch, #event_ty, #phase, #phantom,
        > for #self_ty #sync_where_bounds {
            const IS_IMPLEMENTED: bool = true;
            const IS_ASYNC: bool = #is_async;

            #[inline]
            fn on_phase(
                &'__EventLifetime self,
                _target: &'__EventLifetime #crate_name::handlers::Handler<__EventDispatch>,
                _ev: &'__EventLifetime mut #event_ty,
                _state: &'__EventLifetime mut #event_ty_event::State,
            ) -> #crate_name::events::EventResult {
                #sync_body
            }
        }
        #async_impl
    })
}
fn create_impls(
    crate_name: &SynTokenStream, discriminator: &SynTokenStream, is_async_handler: bool,
    self_ty: &Type, impl_generics: &Generics, methods: &[MethodInfo],
    synthetic_methods: &[ImplItemMethod], extra_items: &[SynTokenStream],
) -> SynTokenStream {
    let mut impls = SynTokenStream::new();
    let mut stages = Vec::new();

    for (i, info) in methods.iter().enumerate() {
        match info {
            MethodInfo::EventHandler { phase, sig } => {
                let (phantom, tts) = create_normal_handler(
                    crate_name, discriminator, is_async_handler,
                    i, methods.len(), self_ty, impl_generics, phase, sig,
                );
                impls.extend(tts);
                stages.push(CallStage::new(
                    crate_name, quote!(_this), self_ty, Some(quote!(#phantom)),
                ));
            }
        }
    }

    let (impl_bounds, _, impl_where_bounds) = impl_generics.split_for_impl();
    let synthetic_methods = if synthetic_methods.is_empty() {
        quote! { }
    } else {
        quote! {
            impl #impl_bounds #self_ty #impl_where_bounds {
                #(#synthetic_methods)*
            }
        }
    };

    let group = CallGroup::new(quote!(_), stages);
    if methods.len() > 1 {
        impls.extend(
            make_merge_event_handler(
                crate_name, is_async_handler,
                EventHandlerTarget::Type(self_ty), impl_generics,
                Some(discriminator.clone()),
                vec![group], Vec::new(),
            )
        );
    }

    quote! {
        #[allow(non_snake_case)]
        const _: () = {
            #impls
            #(#extra_items)*
            #synthetic_methods
        };
    }
}

fn mark_attrs_processed(method: &mut ImplItemMethod) {
    for attr in &mut method.attrs {
        if HandlerType::is_attr(attr) {
            mark_attribute_processed(attr);
        }
    }
}

/// Contains the implementation for `#[events_impl]`.
pub struct EventsImplAttr {
    self_ty: Type,
    impl_generics: Generics,
    methods: Vec<MethodInfo>,
    synthetic_methods: Vec<ImplItemMethod>,
    extra_items: Vec<SynTokenStream>,
    emit_input: bool,
    impl_input: ItemImpl,
    crate_name: SynTokenStream,
    discriminator: SynTokenStream,
    is_async_handler: bool,
}
impl EventsImplAttr {
    /// Parses an impl block.
    ///
    /// It takes a mutable reference to the impl block, as it may mark some attributes processed.
    pub fn new(
        impl_block: &mut ItemImpl, crate_name: Option<SynTokenStream>, is_async_handler: bool,
    ) -> Result<Self> {
        let crate_name = crate_name.unwrap_or(quote! { ::static_events });

        let mut handlers = Vec::new();
        let mut errors = Error::empty();

        for item in &mut impl_block.items {
            match item {
                ImplItem::Method(method) => {
                    match MethodInfo::for_method(&crate_name, method, is_async_handler) {
                        Ok(Some(handler)) => handlers.push(handler),
                        Ok(None) => { }
                        Err(e) => errors = errors.combine(e),
                    }
                    mark_attrs_processed(method);
                },
                _ => { }
            }
        }
        if !errors.is_empty() {
            Err(errors)
        } else {
            Ok(EventsImplAttr {
                self_ty: (*impl_block.self_ty).clone(),
                impl_generics: impl_block.generics.clone(),
                methods: handlers,
                synthetic_methods: Vec::new(),
                extra_items: Vec::new(),
                emit_input: false,
                impl_input: impl_block.clone(),
                crate_name: crate_name.clone(),
                discriminator: quote!(#crate_name::private::HandlerImplBlock),
                is_async_handler,
            })
        }
    }

    fn mark_emit_input(mut self) -> Self {
        self.emit_input = true;
        self
    }

    /// Parses a token stream.
    pub fn from_tokens(toks: impl ToTokens, is_async_handler: bool) -> Result<Self> {
        Ok(Self::new(
            &mut parse2(toks.into_token_stream())?, None, is_async_handler,
        )?.mark_emit_input())
    }

    /// Parses a token stream.
    pub fn from_tokens_raw(toks: proc_macro::TokenStream, is_async_handler: bool) -> Result<Self> {
        Ok(Self::new(
            &mut parse(toks)?, None, is_async_handler,
        )?.mark_emit_input())
    }

    /// Sets the discriminator of the generated impl block.
    pub fn set_discriminator(&mut self, toks: impl ToTokens) {
        self.discriminator = toks.into_token_stream();
    }

    /// Processes an synthetic method, emitting it alongside the method's other impls.
    pub fn process_synthetic_method(&mut self, method: impl ToTokens) -> Result<()> {
        let mut method: ImplItemMethod = parse2(method.into_token_stream())?;
        match MethodInfo::for_method(&self.crate_name, &mut method, self.is_async_handler) {
            Ok(Some(handler)) => self.methods.push(handler),
            Ok(None) => { }
            Err(e) => return Err(e),
        }
        mark_attrs_processed(&mut method);
        self.synthetic_methods.push(method);
        Ok(())
    }

    /// Adds an extra item into the const block.
    pub fn add_extra_item(&mut self, item: impl ToTokens) {
        self.extra_items.push(item.into_token_stream());
    }

    /// Generates an impl block for the event handler implementation.
    pub fn generate(self) -> SynTokenStream {
        let impls = create_impls(
            &self.crate_name, &self.discriminator, self.is_async_handler,
            &self.self_ty, &self.impl_generics, &self.methods,
            &self.synthetic_methods, &self.extra_items,
        );
        if self.emit_input {
            let input = self.impl_input;
            quote! {
                #input
                #impls
            }
        } else {
            impls
        }
    }
}