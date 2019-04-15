use proc_macro::{TokenStream, Span};
use proc_macro2::{Ident, Span as SynSpan, TokenStream as SynTokenStream};
use sha2::*;
use std::fmt::Display;
use syn::*;
use quote::*;

macro_rules! ident {
    ($($tts:tt)*) => { Ident::new(&format!($($tts)*), SynSpan::call_site()) }
}

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
        ident!("__ProcMacroImplEvents_{}_{}", purpose, self.1)
    }
    pub fn gensym_id(&self, purpose: &str, id: impl Display) -> Ident {
        ident!("__ProcMacroImplEvents_{}_{}_{}", purpose, id, self.1)
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

fn unwrap_distinguisher(distinguisher: Option<impl ToTokens>) -> SynTokenStream {
    match distinguisher {
        Some(x) => x.into_token_stream(),
        None => quote! { ::static_events::handlers::DefaultHandler },
    }
}
fn bool_val(
    tp: impl ToTokens, distinguisher: impl ToTokens, call: impl ToTokens,
) -> SynTokenStream {
    quote! {
        || ::static_events::private::#call::<
            '__EventLifetime, #tp, __EventDispatch, __EventType, __EventPhase, #distinguisher,
        >()
    }
}
pub fn is_implemented(tp: impl ToTokens, distinguisher: impl ToTokens) -> SynTokenStream {
    bool_val(tp, distinguisher, quote! { is_implemented })
}
pub fn is_async(tp: impl ToTokens, distinguisher: impl ToTokens) -> SynTokenStream {
    bool_val(tp, distinguisher, quote! { is_async })
}

pub struct CallStage {
    extract_field: SynTokenStream, field_tp: SynTokenStream, distinguisher: SynTokenStream,
}
impl CallStage {
    pub fn new(
        extract_field: impl ToTokens, field_tp: impl ToTokens,
        distinguisher: Option<SynTokenStream>,
    ) -> CallStage {
        CallStage {
            extract_field: extract_field.into_token_stream(),
            field_tp: field_tp.into_token_stream(),
            distinguisher: unwrap_distinguisher(distinguisher),
        }
    }
}

pub struct CallGroup {
    is_synthetic: bool, condition: SynTokenStream, stages: Vec<CallStage>,
}
impl CallGroup {
    pub fn new(condition: impl ToTokens, stages: Vec<CallStage>) -> CallGroup {
        CallGroup { is_synthetic: false, condition: condition.into_token_stream(), stages }
    }
}

pub enum EventHandlerTarget<'a> {
    Ident(&'a Ident), Type(&'a Type),
}
fn make_call(
    field: impl ToTokens, tp: impl ToTokens, is_async: bool, is_direct: bool,
    distinguisher: impl ToTokens,
) -> SynTokenStream {
    let args = if is_direct {
        quote! { _target, _ev, _state }
    } else {
        quote! { self.target, unsafe { &mut *self.ev }, unsafe { &mut *self.state } }
    };
    let call_fn = if is_async { quote! { on_phase_async } } else { quote! { on_phase } };
    quote! {
        ::static_events::private::#call_fn::<
            #tp, __EventDispatch, __EventType, __EventPhase, #distinguisher,
        >(#field, #args)
    }
}
pub fn make_merge_event_handler(
    ctx: &GensymContext, name: EventHandlerTarget, item_generics: &Generics,
    distinguisher: Option<SynTokenStream>, mut groups: Vec<CallGroup>, common: Vec<CallStage>,
) -> SynTokenStream {
    let distinguisher = unwrap_distinguisher(distinguisher);
    let ctx = ctx.derive(&distinguisher);

    let future_type = ctx.gensym("FutureImpl");
    let future_state = ctx.gensym("FutureImplState");

    let event_generics_raw = quote! {
        '__EventLifetime,
        __EventDispatch: ::static_events::Events,
        __EventType: ::static_events::Event + '__EventLifetime,
        __EventPhase: ::static_events::handlers::EventPhase + '__EventLifetime,
    };
    let event_generics = generics(&event_generics_raw);

    let handler_generics = merge_generics(&event_generics, item_generics);
    let (handler_impl_bounds, handler_ty_param, handler_where_bounds) =
        handler_generics.split_for_impl();
    let (self_impl_bounds, self_ty_param, self_where_bounds) =
        item_generics.split_for_impl();

    let name = match name {
        EventHandlerTarget::Ident(id) => quote! { #id #self_ty_param },
        EventHandlerTarget::Type(tp) => quote! { #tp },
    };

    let handler_ty = quote! { ::static_events::handlers::EventHandler<
        '__EventLifetime, __EventDispatch, __EventType, __EventPhase, #distinguisher,
    > };

    let mut is_implemented_expr = quote! { false };
    let mut is_async_expr = quote! { false };
    let mut sync_actions = SynTokenStream::new();
    let mut other_items = SynTokenStream::new();
    let mut future_init_action = SynTokenStream::new();
    let mut future_init_matcher = quote! { ::static_events::private::event_error() };
    let mut future_resume_matcher = SynTokenStream::new();
    let mut future_data_variants = SynTokenStream::new();
    let mut future_data_fns = SynTokenStream::new();

    let mut common_group = CallGroup::new(quote! { }, common);
    common_group.is_synthetic = true;
    let mut all_groups = Vec::new();
    all_groups.push(common_group);
    all_groups.append(&mut groups);
    for (group_i, group) in all_groups.iter().enumerate() {
        let is_synthetic = group.is_synthetic;
        let mut sync_stage = SynTokenStream::new();

        for (stage_i, stage) in group.stages.iter().enumerate() {
            let variant_name = ident!("Yielded_{}_{}", group_i, stage_i);
            let run_variant_fn = ident!("run_step_{}_{}", group_i, stage_i);
            let resume_variant_fn = ident!("resume_step_{}_{}", group_i, stage_i);
            let make_future = ident!("make_future_{}_{}", group_i, stage_i);
            let extract_fn = ctx.gensym_id("extract",
                                           format_args!("{}_{}", group_i, stage_i));
            let existential_ty = ctx.gensym_id("ExistentialFuture",
                                               format_args!("{}_{}", group_i, stage_i));
            let next_variant_fn = if group.stages.len() == stage_i + 1 {
                if is_synthetic { ident!("do_selection") } else { ident!("done") }
            } else {
                ident!("run_step_{}_{}", group_i, stage_i + 1)
            };

            let extract_field = &stage.extract_field;
            let field_tp = &stage.field_tp;
            let distinguisher = &stage.distinguisher;

            let call_direct = make_call(quote!(subhandler), field_tp, false, true , distinguisher);
            let call_sync   = make_call(quote!(subhandler), field_tp, false, false, distinguisher);
            let call_async  = make_call(quote!(subhandler), field_tp, true , false, distinguisher);

            let check_args = quote! { ::<
                '__EventLifetime,
                #field_tp, __EventDispatch, __EventType, __EventPhase, #distinguisher
            > };

            other_items.extend(quote! {
                #[inline(always)]
                fn #extract_fn #self_impl_bounds (this: &#name) -> &#field_tp #self_where_bounds {
                    #extract_field
                }

                existential type #existential_ty #handler_impl_bounds #handler_where_bounds:
                    ::std::future::Future<Output = ::static_events::EventResult> +
                    '__EventLifetime;
            });
            is_implemented_expr.extend(is_implemented(field_tp, distinguisher));
            is_async_expr.extend(is_async(field_tp, distinguisher));
            future_data_variants.extend(quote! {
                #variant_name(#existential_ty #handler_ty_param),
            });
            future_data_fns.extend(quote! {
                #[inline(always)]
                fn #resume_variant_fn(
                    &mut self, context: &mut ::std::task::Context<'_>,
                ) -> ::std::task::Poll<::static_events::EventResult> {
                    use ::std::future::Future;
                    if let #future_state::#variant_name(future) = &mut self.fut_state {
                        self.is_poisoned = true;
                        let res = unsafe { ::std::pin::Pin::new_unchecked(future) }.poll(context);
                        self.is_poisoned = false;
                        match res {
                            ::std::task::Poll::Ready(::static_events::EvOk) => {
                                self.fut_state = #future_state::Errored;
                                self.#next_variant_fn(context)
                            },
                            ::std::task::Poll::Ready(r) => {
                                self.fut_state = #future_state::Done;
                                ::std::task::Poll::Ready(r)
                            },
                            ::std::task::Poll::Pending => ::std::task::Poll::Pending,
                        }
                    } else {
                        ::static_events::private::event_error()
                    }
                }

                #[inline(always)]
                fn #make_future(
                    &self, subhandler: &'__EventLifetime #field_tp,
                ) -> #existential_ty #handler_ty_param {
                    #call_async
                }

                #[inline(always)]
                fn #run_variant_fn(
                    &mut self, context: &mut ::std::task::Context<'_>,
                ) -> ::std::task::Poll<::static_events::EventResult> {
                    self.fut_state = #future_state::Errored;
                    let subhandler = #extract_fn(self.this);
                    if !::static_events::private::is_implemented #check_args() {
                        self.#next_variant_fn(context)
                    } else if !::static_events::private::is_async #check_args() {
                        let result = #call_sync;
                        match result {
                            ::static_events::EvOk => self.#next_variant_fn(context),
                            r => {
                                self.fut_state = #future_state::Done;
                                ::std::task::Poll::Ready(r)
                            },
                        }
                    } else {
                        let handler = self.#make_future(subhandler);
                        self.fut_state = #future_state::#variant_name(handler);
                        self.#resume_variant_fn(context)
                    }
                }
            });
            sync_stage.extend(quote! {{
                let subhandler = #extract_fn(self);
                match #call_direct {
                    ::static_events::EvOk => { }
                    e => return e,
                }
            }});
            future_resume_matcher.extend(quote! {
                #variant_name(_) => fut.#resume_variant_fn(context),
            });
        }

        if is_synthetic {
            let init_fn = if group.stages.len() == 0 {
                ident!("do_selection")
            } else {
                ident!("run_step_{}_0", group_i)
            };
            future_init_action.extend(quote! { fut.#init_fn(context) });
            sync_actions.extend(sync_stage);
        } else {
            let condition_fn = ctx.gensym_id("check_condition", group_i);
            let condition = &group.condition;
            let first_variant_fn = if group.stages.len() == 0 {
                ident!("done")
            } else {
                ident!("run_step_{}_0", group_i)
            };
            other_items.extend(quote! {
                #[inline(always)]
                fn #condition_fn #self_impl_bounds (this: &#name) -> bool {
                    #condition
                }
            });
            future_init_matcher = quote! {
                if #condition_fn(self.this) {
                    self.#first_variant_fn(context)
                } else {
                    #future_init_matcher
                }
            };
            sync_actions.extend(quote! {
                if #condition_fn(self) {
                    #sync_stage
                    return ::static_events::EvOk
                }
            });
        }
    }

    quote! {
        #other_items
        enum #future_state #handler_impl_bounds #handler_where_bounds {
            NeverRun, Done, Errored,
            PhantomDefs(
                ::std::marker::PhantomData<&'__EventLifetime __EventDispatch>,
                ::std::marker::PhantomData<&'__EventLifetime mut __EventType>,
                ::std::marker::PhantomData<&'__EventLifetime mut __EventType::State>,
                ::std::marker::PhantomData<fn(__EventPhase) -> __EventPhase>,
            ),
            #future_data_variants
        }
        struct #future_type #handler_impl_bounds #handler_where_bounds {
            this: &'__EventLifetime #name #self_ty_param,
            target: &'__EventLifetime ::static_events::Handler<__EventDispatch>,
            ev: *mut __EventType,
            state: *mut __EventType::State,
            is_poisoned: bool,
            fut_state: #future_state #handler_ty_param,
        }
        impl #handler_impl_bounds #future_type #handler_ty_param #handler_where_bounds {
            #future_data_fns

            #[inline(always)]
            fn do_selection(
                &mut self, context: &mut ::std::task::Context<'_>,
            ) -> ::std::task::Poll<::static_events::EventResult> {
                #future_init_matcher
            }

            #[inline(always)]
            fn done(
                &mut self, _context: &mut ::std::task::Context<'_>,
            ) -> ::std::task::Poll<::static_events::EventResult> {
                ::std::task::Poll::Ready(::static_events::EvOk)
            }
        }
        impl #handler_impl_bounds ::std::future::Future
            for #future_type #handler_ty_param #handler_where_bounds
        {
            type Output = ::static_events::EventResult;

            #[inline(always)]
            fn poll(
                self: ::std::pin::Pin<&mut Self>, context: &mut ::std::task::Context<'_>,
            ) -> ::std::task::Poll<Self::Output> {
                use #future_state::*;
                let fut = unsafe { self.get_unchecked_mut() };
                if fut.is_poisoned {
                    fut.fut_state = Errored;
                    fut.is_poisoned = false;
                }
                match &fut.fut_state {
                    NeverRun => #future_init_action,
                    Done => ::static_events::private::async_already_done_error(),
                    Errored => ::static_events::private::async_panicked_error(),
                    #future_resume_matcher
                    _ => ::static_events::private::event_error(),
                }
            }
        }
        impl #handler_impl_bounds #handler_ty for #name #handler_where_bounds {
            const IS_IMPLEMENTED: bool = #is_implemented_expr;
            const IS_ASYNC: bool = #is_async_expr;

            #[inline(always)]
            fn on_phase(
                &'__EventLifetime self,
                _target: &'__EventLifetime ::static_events::Handler<__EventDispatch>,
                _ev: &'__EventLifetime mut __EventType,
                _state: &'__EventLifetime mut __EventType::State,
            ) -> ::static_events::EventResult {
                #sync_actions
                ::static_events::private::event_error()
            }

            type FutureType = ::static_events::private::FutureSyncnessWrapper<
                #future_type #handler_ty_param,
                (
                    &'__EventLifetime #name #self_ty_param,
                    &'__EventLifetime ::static_events::Handler<__EventDispatch>,
                    &'__EventLifetime mut __EventType,
                    &'__EventLifetime mut __EventType::State,
                    #future_state #handler_ty_param,
                ),
            >;

            #[inline(always)]
            fn on_phase_async (
                &'__EventLifetime self,
                target: &'__EventLifetime ::static_events::Handler<__EventDispatch>,
                ev: &'__EventLifetime mut __EventType,
                state: &'__EventLifetime mut __EventType::State,
            ) -> Self::FutureType {
                ::static_events::private::FutureSyncnessWrapper::new(
                    #future_type {
                        this: self, target, ev, state, is_poisoned: false,
                        fut_state: #future_state::NeverRun,
                    }
                )
            }
        }
    }
}
