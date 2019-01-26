use core::result::Result;
use proc_macro::{TokenStream, Span};
use proc_macro2::{TokenStream as SynTokenStream, Span as SynSpan};
use syn::*;
use syn::spanned::Spanned;
use quote::{quote, ToTokens};

fn stream_span(attr: TokenStream) -> Span {
    let head_span = attr.clone().into_iter().next().unwrap().span();
    let tail_span = attr.into_iter().last().unwrap().span();
    head_span.join(tail_span).unwrap()
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum HandlerType {
    None, Normal, Ipc, Invalid,
}
impl HandlerType {
    fn name(&self) -> &'static str {
        match self {
            HandlerType::None => "<none>",
            HandlerType::Normal => "#[event_handler]",
            HandlerType::Ipc => "#[ipc_handler]",
            HandlerType::Invalid => "<invalid>",
        }
    }

    #[must_use]
    fn set(&mut self, tp: HandlerType, span: SynSpan) -> Result<(), ()> {
        if *self != HandlerType::None {
            span.unstable().error(if *self == tp {
                format!("{} can only be specified once.", self.name())
            } else {
                format!("{} cannot be used with {}.", self.name(), tp.name())
            }).emit();
            Err(())
        } else {
            *self = tp;
            Ok(())
        }
    }
}

fn last_path_segment(path: &Path) -> String {
    (&path.segments).into_iter().last().expect("Empty path?").ident.to_string()
}
fn merge_generics(a: &Generics, b: &Generics) -> Generics {
    let mut toks = SynTokenStream::new();
    toks.extend(quote! { < });
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
    toks.extend(quote! { > });

    let mut generics = parse2::<Generics>(toks).unwrap();
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
fn create_impls_for_method(
    self_ty: &Type, impl_generics: &Generics, method: &mut ImplItemMethod,
) -> Result<SynTokenStream, ()> {
    let mut handler_type = HandlerType::None;
    let mut bad_attr = false;
    let mut phase = quote! { ::static_events::EvOnEvent };
    for attr in &mut method.attrs {
        let (tp, is_handler) = match last_path_segment(&attr.path).as_str() {
            "event_handler" => (HandlerType::Normal, true),
            "ipc_handler"   => (HandlerType::Ipc   , true),
            _ => (HandlerType::Invalid, false),
        };
        if is_handler {
            if handler_type.set(tp, attr.span()).is_err() {
                bad_attr = true;
            }
            if !attr.tts.is_empty() {
                match parse2::<TypeParen>(attr.tts.clone()) {
                    Ok(tp) => phase = tp.elem.into_token_stream(),
                    Err(_) => {
                        attr.tts.span()
                            .unstable()
                            .error(format!("Could not parse {} attribute.", tp.name()))
                            .emit();
                        bad_attr = true;
                    },
                }
            }
            attr.tts = quote! { (event_handler_ok) };
        }
    }
    if bad_attr {
        Ok(SynTokenStream::new())
    } else if handler_type != HandlerType::None {
        let sig = &method.sig;
        if sig.unsafety.is_some() || sig.asyncness.is_some() || sig.decl.variadic.is_some() {
            sig.span()
                .unstable()
                .error("Event handlers cannot be unsafe, async, or variadic.")
                .emit();
            return Err(())
        }

        let mut params = sig.decl.inputs.iter().peekable();
        let (mut has_self_param, mut has_target_param, mut has_state_param) = (false, false, false);
        fn check_peek(arg: Option<&&FnArg>) -> Result<Option<FnArg>, ()> {
            let param = match arg {
                Some(arg @ FnArg::SelfValue(_)) => {
                    arg.span()
                        .unstable()
                        .error("Event handlers may not take self by value.")
                        .emit();
                    return Err(())
                },
                Some(FnArg::Ignored(ty)) => Some(FnArg::Captured(ArgCaptured {
                    pat: Pat::Ident(PatIdent {
                        by_ref: None,
                        mutability: None,
                        ident: Ident::new("", SynSpan::call_site()),
                        subpat: None
                    }),
                    colon_token: syn::token::Colon(SynSpan::call_site()),
                    ty: ty.clone(),
                })),
                Some(FnArg::Inferred(_)) => unreachable!(),
                x => x.cloned().cloned(),
            };
            if let Some(arg @ FnArg::Captured(ArgCaptured {
                ty: Type::ImplTrait(_), ..
            })) = &param {
                arg.span()
                    .unstable()
                    .error("Source event dispatch must be taken by reference")
                    .emit();
                Err(())
            } else {
                Ok(param)
            }
        }

        if let Some(FnArg::SelfRef(self_info)) = check_peek(params.peek())? {
            if self_info.mutability.is_some() {
                self_info.span()
                    .unstable()
                    .error("Event handlers may not take self by mutable reference.")
                    .emit();
                return Err(())
            }
            has_self_param = true;
            params.next();
        }
        if let Some(FnArg::Captured(ArgCaptured {
            ty: Type::Reference(reference), ..
        })) = check_peek(params.peek())? {
            if let Type::ImplTrait(_) = &*reference.elem {
                has_target_param = true;
                params.next();
            }
        }
        let event_ty = if let Some(FnArg::Captured(arg_info)) = params.next() {
            match &arg_info.ty {
                Type::Reference(reference_info) => {
                    let ty = *reference_info.elem.clone();
                    if let Type::ImplTrait(_) = &ty {
                        arg_info.span()
                            .unstable()
                            .error("Event type cannot be an impl trait.")
                            .emit();
                        return Err(())
                    }
                    ty
                },
                _ => {
                    arg_info.span()
                        .unstable()
                        .error("Event handlers must take the event type by reference.")
                        .emit();
                    return Err(())
                }
            }
        } else {
            method.sig.span()
                .unstable()
                .error("No event type parameter found.")
                .emit();
            return Err(())
        };
        if handler_type != HandlerType::Ipc {
            if let Some(FnArg::Captured(ArgCaptured {
                ty: Type::Reference(_), ..
            })) = check_peek(params.peek())? {
                has_state_param = true;
                params.next();
            }
        }
        if let Some(extra) = params.next() {
            extra.span()
                .unstable()
                .error("Unexpected parameter for an event handler.")
                .emit();
            return Err(())
        }

        let name = &method.sig.ident;
        let call = if has_self_param {
            quote! { self.#name }
        } else {
            quote! { Self::#name }
        };
        let state = if has_state_param {
            quote! { _state, }
        } else {
            quote! {}
        };
        let params = if has_target_param {
            quote! { (_target, ev, #state) }
        } else {
            quote! { (ev, #state) }
        };

        let fn_body = match handler_type {
            HandlerType::Normal => quote! { #call #params .into() },
            HandlerType::Ipc => quote! {
                let state: &mut Option<_> = _state;
                assert!(state.is_none(), "Duplicate listeners responding to event!");
                let result = #call #params;
                *state = Some(result);
                ::static_events::Event::default_return(ev)
            },
            _ => unreachable!(),
        };

        let merged = merge_generics(&method.sig.decl.generics, impl_generics);
        let (impl_bounds, _, where_bounds) = merged.split_for_impl();
        Ok(quote! {
            impl #impl_bounds
                ::static_events::EventHandler<#event_ty, #phase> for #self_ty
                #where_bounds
            {
                fn on_phase(
                    &self, _target: &impl ::static_events::EventDispatch, ev: &mut #event_ty,
                    _state: &mut <#event_ty as ::static_events::Event>::StateArg,
                ) -> <#event_ty as ::static_events::Event>::MethodRetVal {
                    #fn_body
                }
            }
        })
    } else {
        Ok(SynTokenStream::new())
    }
}

pub fn event_dispatch(attr: TokenStream, item: TokenStream) -> TokenStream {
    if !attr.is_empty() {
        stream_span(attr)
            .error("#[event_dispatch] cannot be used with parameters.")
            .emit();
        return item
    }
    let mut impl_block = match parse::<ItemImpl>(item.clone()) {
        Ok(block) => block,
        Err(_) => {
            stream_span(item.clone())
                .error("#[event_dispatch] can only be used on impl blocks.")
                .emit();
            return item
        },
    };

    let mut impls = SynTokenStream::new();
    for item in &mut impl_block.items {
        match item {
            ImplItem::Method(method) =>
                impls.extend(create_impls_for_method(&impl_block.self_ty,
                                                     &impl_block.generics, method)),
            _ => { }
        }
    }
    if !impls.is_empty() {
        let (impl_bounds, _, where_bounds) = impl_block.generics.split_for_impl();
        let ty = &impl_block.self_ty;
        impls.extend(quote! {
            impl #impl_bounds ::static_events::RootEventDispatch for #ty #where_bounds { }
        });
    }
    TokenStream::from(quote! {
        #impl_block
        #impls
    })
}