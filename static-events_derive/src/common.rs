use proc_macro::{TokenStream, Span};
use proc_macro2::{Ident, Span as SynSpan, TokenStream as SynTokenStream};
use sha2::*;
use std::fmt::Display;
use syn::*;
use quote::*;

pub struct GensymContext(String, String);
impl GensymContext {
    pub fn new(target: impl Display) -> GensymContext {
        let full_hash = Sha256::digest(target.to_string().as_bytes());
        let full_hash = format!("{:x}", &full_hash);
        let hash = (&full_hash[0..16]).to_string();
        GensymContext(full_hash, hash)
    }
    pub fn derive(&self, target: impl Display) -> GensymContext {
        Self::new(format_args!("{}_{}", target, self.0))
    }

    pub fn gensym(&self, purpose: &str) -> Ident {
        Ident::new(&format!("__ProcMacroImplEvents_{}_{}", purpose, self.1),
                   SynSpan::call_site())
    }
    pub fn gensym_id(&self, purpose: &str, id: impl Display) -> Ident {
        Ident::new(&format!("__ProcMacroImplEvents_{}_{}_{}", purpose, id, self.1),
                   SynSpan::call_site())
    }
}

pub fn last_path_segment(path: &Path) -> String {
    (&path.segments).into_iter().last().expect("Empty path?").ident.to_string()
}

pub fn generics(a: impl ToTokens) -> Generics {
    parse2::<Generics>(quote! { < #a > }).unwrap()
}
fn process_generics(list: &[&Generics], skip_lifetimes: bool) -> Generics {
    let mut toks = SynTokenStream::new();
    if !skip_lifetimes {
        for g in list {
            for lifetime in g.lifetimes() {
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
pub fn merge_generics(a: &Generics, b: &Generics) -> Generics {
    process_generics(&[a, b], false)
}
pub fn strip_lifetimes(a: &Generics) -> Generics {
    process_generics(&[a], true)
}

pub fn stream_span(attr: TokenStream) -> Span {
    let head_span = attr.clone().into_iter().next().unwrap().span();
    let tail_span = attr.into_iter().last().unwrap().span();
    head_span.join(tail_span).unwrap()
}

fn unwrap_distinguisher(distinguisher: Option<SynTokenStream>) -> SynTokenStream {
    distinguisher.unwrap_or(quote! {::static_events::handlers::DefaultHandler})
}
fn make_call(
    field: impl ToTokens, tp: impl ToTokens, is_async: bool, distinguisher: impl ToTokens,
) -> SynTokenStream {
    let call_fn = if is_async { quote! { on_phase_async } } else { quote! { on_phase } };
    let call = quote! {
        ::static_events::private::#call_fn::<
            #tp, __EventDispatch, __EventType, __EventPhase, #distinguisher,
        >(#field, _target, _ev, _state)
    };
    if is_async { quote! { r#await!(unsafe { #call }) } } else { call }
}
pub fn dispatch_on_phase(
    field: impl ToTokens, tp: impl ToTokens, is_async: bool, distinguisher: Option<SynTokenStream>,
) -> SynTokenStream {
    let distinguisher = unwrap_distinguisher(distinguisher);
    let check_args = quote! {
        ::<'__EventLifetime, #tp, __EventDispatch, __EventType, __EventPhase, #distinguisher>
    };
    let match_result = quote! {
        match dispatch_result {
            ::static_events::EvOk => { }
            e => return e,
        }
    };
    if is_async {
        let call_async = make_call(&field, &tp, true, &distinguisher);
        let call_sync = make_call(&field, &tp, false, &distinguisher);
        quote! {{
            if ::static_events::private::is_implemented #check_args() {
                let dispatch_result = if ::static_events::private::is_async #check_args() {
                    #call_async
                } else {
                    #call_sync
                };
                #match_result
            }
        }}
    } else {
        let call = make_call(&field, &tp, false, &distinguisher);
        quote! {{
            if ::static_events::private::is_implemented #check_args() {
                let dispatch_result = #call;
                #match_result
            }
        }}
    }
}
fn bool_val(
    tp: impl ToTokens, distinguisher: Option<SynTokenStream>, call: impl ToTokens,
) -> SynTokenStream {
    let distinguisher = unwrap_distinguisher(distinguisher);
    quote! {
        || ::static_events::private::#call::<
            '__EventLifetime, #tp, __EventDispatch, __EventType, __EventPhase, #distinguisher,
        >()
    }
}
pub fn is_implemented(tp: impl ToTokens, distinguisher: Option<SynTokenStream>) -> SynTokenStream {
    bool_val(tp, distinguisher, quote! { is_implemented })
}
pub fn is_async(tp: impl ToTokens, distinguisher: Option<SynTokenStream>) -> SynTokenStream {
    bool_val(tp, distinguisher, quote! { is_async })
}

pub enum EventHandlerTarget<'a> {
    Ident(&'a Ident), Type(&'a Type),
}
pub fn make_universal_event_handler(
    ctx: &GensymContext, name: EventHandlerTarget, item_generics: &Generics,
    distinguisher: Option<SynTokenStream>,
    is_implemented: SynTokenStream, is_async: SynTokenStream,
    on_phase_body: SynTokenStream, on_phase_async_body: SynTokenStream,
) -> SynTokenStream {
    let distinguisher = unwrap_distinguisher(distinguisher);
    let ctx = ctx.derive(&distinguisher);

    let method_generics_raw = quote! {
        '__EventLifetime,
        __EventDispatch: ::static_events::Events,
        __EventType: ::static_events::Event + '__EventLifetime,
        __EventPhase: ::static_events::handlers::EventPhase + '__EventLifetime,
    };
    let async_generics = generics(&method_generics_raw);

    let handler_generics = merge_generics(&async_generics, item_generics);
    let (handler_impl_bounds, handler_ty_param, handler_where_bounds) =
        handler_generics.split_for_impl();
    let (self_impl_bounds, self_ty_param, self_where_bounds) =
        item_generics.split_for_impl();

    let name = match name {
        EventHandlerTarget::Ident(id) => quote! { #id #self_ty_param },
        EventHandlerTarget::Type(tp) => quote! { #tp },
    };

    let fn_async = ctx.gensym("async_handler");
    let existential = ctx.gensym("FutureTypeExistential");

    quote! {
        impl #self_impl_bounds #name #self_where_bounds {
            #[inline(always)]
            async fn #fn_async #async_generics (
                &'__EventLifetime self,
                _target: &'__EventLifetime ::static_events::Handler<__EventDispatch>,
                _ev: &'__EventLifetime mut __EventType,
                _state: &'__EventLifetime mut __EventType::State,
            ) -> ::static_events::EventResult {
                #on_phase_async_body
            }
        }

        existential type #existential #handler_generics:
            ::std::future::Future<Output = ::static_events::EventResult> + '__EventLifetime;

        impl #handler_impl_bounds ::static_events::handlers::EventHandler<
            '__EventLifetime, __EventDispatch, __EventType, __EventPhase, #distinguisher,
        > for #name #handler_where_bounds {
            const IS_IMPLEMENTED: bool = #is_implemented;
            const IS_ASYNC: bool = #is_async;

            #[inline(always)]
            fn on_phase(
                &'__EventLifetime self,
                _target: &'__EventLifetime ::static_events::Handler<__EventDispatch>,
                _ev: &'__EventLifetime mut __EventType,
                _state: &'__EventLifetime mut __EventType::State,
            ) -> ::static_events::EventResult {
                #on_phase_body
            }

            type FutureType = #existential #handler_ty_param;

            #[inline(always)]
            unsafe fn on_phase_async (
                &'__EventLifetime self,
                _target: &'__EventLifetime ::static_events::Handler<__EventDispatch>,
                _ev: &'__EventLifetime mut __EventType,
                _state: &'__EventLifetime mut __EventType::State,
            ) -> #existential #handler_ty_param {
                self.#fn_async::<__EventDispatch, __EventType, __EventPhase>(
                    _target, _ev, _state
                )
            }
        }
    }
}
