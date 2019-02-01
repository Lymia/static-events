use core::result::Result;
use darling::*;
use proc_macro::TokenStream;
use proc_macro2::{TokenStream as SynTokenStream, Span as SynSpan};
use std::sync::atomic::{AtomicUsize, Ordering};
use syn::*;
use syn::spanned::Spanned;
use quote::{quote, ToTokens};

// TODO: Figure out a way to handle elided lifetimes
// TODO: Propagate docs to generated IPC proxy traits.

#[derive(Debug)]
enum HandlerArg {
    SelfParam,
    ImplParam,
    Borrow(Type),
}
impl HandlerArg {
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
            FnArg::Ignored(ty) | FnArg::Captured(ArgCaptured { ty, .. }) => match ty {
                Type::Reference(TypeReference { elem, .. }) => match &**elem {
                    Type::ImplTrait(_) => Ok(HandlerArg::ImplParam),
                    elem => Ok(HandlerArg::Borrow(elem.clone())),
                },
                _ => {
                    ty.span().unstable()
                        .error("Event handlers must take parameters by reference.")
                        .emit();
                    Err(())
                }
            },
            FnArg::Inferred(_) => unreachable!(),
        }
    }
}

struct HandlerSig {
    fn_name: Ident,
    method_generics: Generics,
    self_param: Option<SynSpan>,
    target_param: Option<SynSpan>,
    event_ty: Type,
    state_param: Option<SynSpan>,
    is_unsafe: bool,
}
impl HandlerSig {
    fn find_signature(method: &ImplItemMethod) -> Result<HandlerSig, ()> {
        let sig = &method.sig;
        if sig.asyncness.is_some() || sig.decl.variadic.is_some() {
            sig.span().unstable()
                .error("Event handlers cannot be async, or variadic.")
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
        };

        if let Some((HandlerArg::SelfParam, _)) = params.peek() {
            handler_sig.self_param = Some(params.next().unwrap().1);
        }
        if let Some((HandlerArg::ImplParam, _)) = params.peek() {
            handler_sig.target_param = Some(params.next().unwrap().1);
        }
        if let Some((HandlerArg::Borrow(ty), _)) = params.next() {
            handler_sig.event_ty = ty;
        } else {
            sig.span().unstable()
                .error("No event parameter found for event handler.")
                .emit();
            return Err(())
        }
        if let Some((HandlerArg::Borrow(_), _)) = params.peek() {
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

    fn make_call(&self) -> SynTokenStream {
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
            Some(_) => quote! { _state, },
            None    => quote! {},
        };
        let call = quote! { #call (#target ev, #state) };
        if !self.is_unsafe {
            call
        } else {
            quote! { unsafe { #call } }
        }
    }
}

fn check_type(tp: &Type, allow_nonstatic: bool) -> bool {
    match tp {
        Type::Reference(ref_ty) => {
            if let Some(lifetime) = &ref_ty.lifetime {
                let is_valid = if allow_nonstatic {
                    lifetime.ident.to_string().as_str() != "_"
                } else {
                    lifetime.ident.to_string().as_str() == "static"
                };
                is_valid && check_type(&ref_ty.elem, allow_nonstatic)
            } else {
                false
            }
        }
        Type::ImplTrait(_) => false,
        Type::Slice(data) => check_type(&data.elem, allow_nonstatic),
        Type::Array(data) => check_type(&data.elem, allow_nonstatic),
        Type::Ptr(data) => check_type(&data.elem, allow_nonstatic),
        Type::Paren(data) => check_type(&data.elem, allow_nonstatic),
        Type::Group(data) => check_type(&data.elem, allow_nonstatic),
        Type::Tuple(data) => {
            for tp in &data.elems {
                if !check_type(tp, allow_nonstatic) {
                    return false
                }
            }
            true
        }
        _ => true,
    }
}

struct ProxySig {
    fn_name: Ident,
    method_generics: Generics,
    self_param: Option<SynSpan>,
    target_param: Option<SynSpan>,
    types: Vec<Type>,
    fn_return: Type,
    unsafety: Option<Token![unsafe]>,
}
impl ProxySig {
    fn find_signature(method: &ImplItemMethod) -> Result<ProxySig, ()> {
        let sig = &method.sig;
        if sig.asyncness.is_some() || sig.decl.variadic.is_some() {
            sig.span().unstable()
                .error("Event handlers cannot be async, or variadic.")
                .emit();
            return Err(())
        }

        let mut self_param = None;
        let mut target_param = None;
        let mut types = Vec::new();

        let mut params = method.sig.decl.inputs.clone().into_iter().peekable();
        match params.peek() {
            Some(param @ FnArg::SelfValue(_)) => {
                param.span().unstable()
                    .error("Event handlers may not take `self` by value.")
                    .emit();
                return Err(())
            },
            Some(FnArg::SelfRef(self_ref)) => if self_ref.mutability.is_some() {
                self_ref.span().unstable()
                    .error("Event handlers may not take `self` by mutable reference.")
                    .emit();
                return Err(())
            } else {
                self_param = Some(params.next().unwrap().span())
            },
            _ => { }
        }
        if let Some(Ok(HandlerArg::ImplParam)) = params.peek().map(HandlerArg::from_param) {
            target_param = Some(params.next().unwrap().span())
        }
        while let Some(param) = params.next() {
            match param {
                FnArg::Ignored(ty) | FnArg::Captured(ArgCaptured { ty, .. }) => {
                    if !check_type(&ty, true) {
                        ty.span().unstable()
                            .error("IPC calls cannot currently use elided type parameters or \
                                    impl trait.")
                            .emit();
                        return Err(())
                    }
                    types.push(ty);
                }
                FnArg::SelfValue(_) | FnArg::SelfRef(_) | FnArg::Inferred(_) => unimplemented!(),
            }
        }

        let fn_return = match &sig.decl.output {
            ReturnType::Default => parse_str::<Type>("()").unwrap(),
            ReturnType::Type(_, ty) => *ty.clone(),
        };
        if !check_type(&fn_return, false) {
            fn_return.span().unstable()
                .error("IPC calls currently cannot return non-static types or impl trait.")
                .emit();
            return Err(())
        }

        Ok(ProxySig {
            fn_name: method.sig.ident.clone(),
            method_generics: method.sig.decl.generics.clone(),
            self_param,
            target_param,
            types,
            fn_return,
            unsafety: method.sig.unsafety.clone(),
        })
    }
}

fn last_path_segment(path: &Path) -> String {
    (&path.segments).into_iter().last().expect("Empty path?").ident.to_string()
}

enum HandlerType {
    Normal(SynTokenStream),
    Ipc,
    IpcProxy,
}
impl HandlerType {
    fn is_attr(attr: &Attribute) -> bool {
        match last_path_segment(&attr.path).as_str() {
            "event_handler" | "ipc_handler" | "ipc_proxy" => true,
            _ => false,
        }
    }

    fn for_attr(attr: &Attribute) -> Result<Option<HandlerType>, ()> {
        let attr_name = last_path_segment(&attr.path);
        match attr_name.as_str() {
            "event_handler" => {
                Ok(Some(HandlerType::Normal(if !attr.tts.is_empty() {
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
            "ipc_handler" | "ipc_proxy" => if !attr.tts.is_empty() {
                attr.span()
                    .unstable()
                    .error(format!("#[{}] may not be used with parameters.", attr_name))
                    .emit();
                Err(())
            } else {
                Ok(Some(match attr_name.as_str() {
                    "ipc_handler" => HandlerType::Ipc,
                    "ipc_proxy" => HandlerType::IpcProxy,
                    _ => unreachable!(),
                }))
            },
            _ => Ok(None),
        }
    }

    fn name(&self) -> &'static str {
        match self {
            HandlerType::Normal(_) => "#[event_handler]",
            HandlerType::Ipc => "#[ipc_handler]",
            HandlerType::IpcProxy => "#[ipc_proxy]",
        }
    }
}

fn merge_generics(a: &Generics, b: &Generics) -> Generics {
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

    let mut generics = parse2::<Generics>(quote! { < #toks > }).unwrap();
    let mut toks = SynTokenStream::new();
    toks.extend(quote! { where });
    for where_element in &a.clone().make_where_clause().predicates {
        toks.extend(quote! { #where_element, })
    }
    for where_element in &b.clone().make_where_clause().predicates {
        toks.extend(quote! { #where_element, })
    }
    generics.where_clause = Some(parse2(toks).unwrap());
    generics
}

struct IpcProxyEntry(MethodSig, Block);
impl IpcProxyEntry {
    fn new(sig: MethodSig, toks: SynTokenStream) -> Self {
        IpcProxyEntry(sig, parse2(quote! { { #toks } })
            .expect("Internal error: Generated invalid block."))
    }
}

enum MethodInfo {
    Normal { phase: SynTokenStream, sig: HandlerSig },
    Ipc { sig: HandlerSig },
    IpcProxy { sig: ProxySig },
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
            Some(HandlerType::Normal(phase)) => {
                let sig = HandlerSig::find_signature(method)?;
                Ok(Some(MethodInfo::Normal { phase, sig }))
            }
            Some(HandlerType::Ipc) => {
                let sig = HandlerSig::find_signature(method)?;
                if let Some(span) = &sig.state_param {
                    span.unstable()
                        .error("IPC event handlers cannot have a state parameter.")
                        .emit();
                }
                Ok(Some(MethodInfo::Ipc { sig }))
            }
            Some(HandlerType::IpcProxy) => {
                let sig = ProxySig::find_signature(method)?;
                Ok(Some(MethodInfo::IpcProxy { sig }))
            }
            None => Ok(None),
        }
    }

    fn create_handler_impl(
        self, self_ty: &Type, impl_generics: &Generics, phantom: &Ident,
    ) -> (SynTokenStream, Vec<IpcProxyEntry>) {
        let (event_ty, phase, method_generics, fn_body, gen_items, proxy_entries) = match self {
            MethodInfo::Normal { phase, sig } => {
                let call = sig.make_call();
                (sig.event_ty, phase, sig.method_generics,
                 quote! { #call.into() }, SynTokenStream::new(), Vec::new())
            }
            MethodInfo::Ipc { sig } => {
                let call = sig.make_call();
                (sig.event_ty, quote! { ::static_events::EvOnEvent }, sig.method_generics, quote! {
                    let state: &mut Option<_> = _state;
                    assert!(state.is_none(), "Duplicate listeners responding to IPC event!");
                    let result = #call;
                    *state = Some(result);
                    ::static_events::EvOk
                }, SynTokenStream::new(), Vec::new())
            }
            MethodInfo::IpcProxy { sig } => {
                let event_ident =
                    Ident::new(&format!("{}_IpcProxyEvent", phantom), SynSpan::call_site());

                let (impl_bounds, event_tys, where_bounds) = sig.method_generics.split_for_impl();
                let mut struct_ty_decl = SynTokenStream::new();
                let mut fn_param_decl = SynTokenStream::new();
                let mut param_captures = SynTokenStream::new();
                for (i, param) in sig.types.iter().enumerate() {
                    let param_ident = Ident::new(&format!("param_{}", i), SynSpan::call_site());
                    struct_ty_decl.extend(quote! { #param, });
                    fn_param_decl.extend(quote! { #param_ident: #param, });
                    param_captures.extend(quote! { #param_ident, });
                }

                let fn_name = sig.fn_name;
                let fn_return = sig.fn_return;
                let unsafety = sig.unsafety;
                let event_def = quote! {
                    struct #event_ident #impl_bounds (Option<(#struct_ty_decl)>) #where_bounds;
                    impl #impl_bounds static_events::events::SimpleInterfaceEvent
                    for #event_ident #event_tys #where_bounds {
                        type State = Option<#fn_return>;
                        type RetVal = #fn_return;
                        fn starting_state(
                            &self, _: &impl ::static_events::EventDispatch,
                        ) -> Option<#fn_return> {
                            None
                        }
                        fn to_return_value(
                            &self,
                            _: &impl ::static_events::EventDispatch, state: Option<#fn_return>,
                        ) -> #fn_return {
                            state.expect(concat!("No listeners responded to ",
                                                 stringify!(#fn_name), "!"))
                        }
                    }
                };
                let proxy_sig = parse2::<TraitItemMethod>(quote! {
                    #unsafety fn #fn_name #impl_bounds (
                        &self, #fn_param_decl
                    ) -> #fn_return #where_bounds;
                }).unwrap();
                let proxy_entry = IpcProxyEntry::new(proxy_sig.sig, quote! {
                    self.dispatch(#event_ident(Some((#param_captures))))
                });

                let call = match sig.self_param {
                    Some(_) => quote! { self.#fn_name },
                    None    => quote! { Self::#fn_name },
                };
                let target = match sig.target_param {
                    Some(_) => quote! { _target, },
                    None    => quote! { },
                };
                let fn_body = quote! {
                    if let Some((#param_captures)) = ev.0.take() {
                        *_state = Some(#call(#target #param_captures));
                        ::static_events::EvOk
                    } else {
                        panic!("Duplicate listeners responding to IPC event!")
                    }
                };
                let event_ty = parse2::<Type>(quote! { #event_ident #event_tys }).unwrap();
                (event_ty, quote! { ::static_events::EvOnEvent }, sig.method_generics,
                 fn_body, event_def, vec![proxy_entry])
            }
        };

        let merged = merge_generics(&method_generics, impl_generics);
        let (impl_bounds, _, where_bounds) = merged.split_for_impl();
        let impls = quote! {
            #gen_items

            impl #impl_bounds
                ::static_events::private::EventHandler<#event_ty, #phase, #phantom> for #self_ty
                #where_bounds
            {
                fn on_phase(
                    &self, _target: &impl ::static_events::EventDispatch, ev: &mut #event_ty,
                    _state: &mut <#event_ty as ::static_events::Event>::StateArg,
                ) -> <#event_ty as ::static_events::Event>::MethodRetVal {
                    #fn_body
                }
            }
        };
        (impls, proxy_entries)
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

struct VisibilityAttr(Visibility);
impl Default for VisibilityAttr {
    fn default() -> Self {
        VisibilityAttr(Visibility::Public(VisPublic { pub_token: Default::default() }))
    }
}
impl FromMeta for VisibilityAttr {
    fn from_string(value: &str) -> darling::Result<Self> {
        parse_str(value).map(VisibilityAttr).map_err(|_| darling::Error::unknown_value(value))
    }
}

#[derive(Default, FromDeriveInput)]
#[darling(default, attributes(event_dispatch))]
struct EventDispatchAttr {
    ipc_proxy_vis: VisibilityAttr,
    ipc_proxy_name: Option<Ident>,
}

static IMPL_COUNT: AtomicUsize = AtomicUsize::new(0);
pub fn event_dispatch(attr: TokenStream, item: TokenStream) -> TokenStream {
    let attrs = match EventDispatchAttr::from_derive_input({
        let attr = SynTokenStream::from(attr.clone());
        &parse2::<DeriveInput>(quote!{ #[event_dispatch(#attr)] struct FakeStruct { } }).unwrap()
    }) {
        Ok(attrs) => attrs,
        Err(e) if !attr.is_empty() => {
            crate::stream_span(attr)
                .error("Could not parse #[event_dispatch] attribute.")
                .note(format!("{}", e))
                .emit();
            return item
        }
        _ => EventDispatchAttr::default(),
    };
    let mut impl_block = match parse::<ItemImpl>(item.clone()) {
        Ok(block) => block,
        Err(_) => {
            crate::stream_span(item.clone())
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

    let impl_id = IMPL_COUNT.fetch_add(1, Ordering::Relaxed);
    let mut impls = SynTokenStream::new();
    let mut ipc_proxies = Vec::new();
    let mut on_phase_impls = SynTokenStream::new();
    for (i, handler) in handlers.into_iter().enumerate() {
        let phantom_name =
            Ident::new(&format!("__ImplEventDispatch_Phantom_{}_{}", impl_id, i),
                       SynSpan::call_site());
        let universal_handler = quote! { ::static_events::private::UniversalEventHandler };
        let universal_params = quote! { <__EventType, __EventPhase, #phantom_name> };

        let (handler_impl, ipc_proxy) =
            handler.create_handler_impl(&impl_block.self_ty, &impl_block.generics, &phantom_name);
        impls.extend(quote! {
            enum #phantom_name { }
            #handler_impl
        });
        ipc_proxies.extend(ipc_proxy);
        on_phase_impls.extend(quote! {
            if <Self as #universal_handler #universal_params>::IS_IMPLEMENTED {
                let result = {
                    let state_arg = ev.borrow_state(state);
                    #universal_handler::#universal_params::on_phase(self, target, ev, state_arg)
                };
                match ev.to_event_result(state, result) {
                    ::static_events::EvOk => { }
                    e => return e,
                }
            }
        });
    }
    let main_impl = {
        let (impl_bounds, _, where_bounds) = impl_block.generics.split_for_impl();
        let ty = &impl_block.self_ty;
        quote! {
            impl #impl_bounds ::static_events::handlers::RawEventDispatch for #ty #where_bounds {
                fn on_phase<
                    __EventType: ::static_events::Event,
                    __EventPhase: ::static_events::handlers::EventPhase,
                    __EventDispatch: ::static_events::EventDispatch,
                >(
                    &self, target: &__EventDispatch,
                    ev: &mut __EventType, state: &mut __EventType::State,
                ) -> ::static_events::EventResult {
                    #on_phase_impls
                    ::static_events::EvOk
                }
            }
        }
    };

    let (trait_decl, trait_impl) = match attrs.ipc_proxy_name {
        Some(ident) => {
            let mut trait_decl_body = SynTokenStream::new();
            let mut trait_impl_body = SynTokenStream::new();
            for IpcProxyEntry(sig, body) in ipc_proxies {
                let trait_item_method = TraitItemMethod {
                    attrs: Vec::new(),
                    sig: sig.clone(),
                    default: None,
                    semi_token: Some(Default::default()),
                };
                trait_decl_body.extend(quote! { #trait_item_method });

                let impl_item_method = ImplItemMethod {
                    attrs: Vec::new(),
                    vis: Visibility::Inherited,
                    defaultness: None,
                    sig,
                    block: body,
                };
                trait_impl_body.extend(quote! { #impl_item_method });
            }
            let vis = attrs.ipc_proxy_vis.0;
            (quote! {
                #vis trait #ident { #trait_decl_body }
            }, quote! {
                impl <__EventHandlerType: ::static_events::EventDispatch> #ident
                for __EventHandlerType {
                    #trait_impl_body
                }
            })
        }
        None => {
            if !ipc_proxies.is_empty() {
                crate::smart_err_attr(
                    attr, item,
                    "#[ipc_proxy] requires #[event_handler(ipc_proxy_name = \"ProxyName\")].");
            }
            (SynTokenStream::new(), SynTokenStream::new())
        },
    };

    let impl_name = Ident::new(&format!("__ImplEventDispatch_{}", impl_id), SynSpan::call_site());
    TokenStream::from(quote! {
        #impl_block
        #trait_decl
        const #impl_name: () = {
            #impls
            #main_impl
            #trait_impl
            ()
        };
    })
}