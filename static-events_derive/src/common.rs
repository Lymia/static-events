use crate::utils::*;
use proc_macro2::{TokenStream as SynTokenStream};
use syn::*;
use quote::*;

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
        ::static_events::private::#call::<
            '__EventLifetime, #tp, __EventDispatch, __EventType, __EventPhase, #distinguisher,
        >()
    }
}

/// Generates `|| (...)::IS_IMPLEMENTED`
pub fn is_implemented(tp: impl ToTokens, distinguisher: impl ToTokens) -> SynTokenStream {
    bool_val(tp, distinguisher, quote! { is_implemented })
}
/// Generates `|| (...)::IS_ASYNC`
pub fn is_async(tp: impl ToTokens, distinguisher: impl ToTokens) -> SynTokenStream {
    bool_val(tp, distinguisher, quote! { is_async })
}

/// A particular call to make during an event dispatch.
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

/// A particular arm of an event dispatch (e.g. if the Events is implemented on an enum).
pub struct CallGroup {
    is_common_group: bool, matcher: SynTokenStream, stages: Vec<CallStage>,
}
impl CallGroup {
    pub fn new(matcher: impl ToTokens, stages: Vec<CallStage>) -> CallGroup {
        CallGroup { is_common_group: false, matcher: matcher.into_token_stream(), stages }
    }
}

pub enum EventHandlerTarget<'a> {
    Ident(&'a Ident), Type(&'a Type),
}
fn make_call(
    field: impl ToTokens, tp: impl ToTokens, is_async: bool, distinguisher: impl ToTokens,
) -> SynTokenStream {
    let call_fn = if is_async { quote! { on_phase_async } } else { quote! { on_phase } };
    quote! {
        ::static_events::private::#call_fn::<
            #tp, __EventDispatch, __EventType, __EventPhase, #distinguisher,
        >(#field, _target, _ev, _state)
    }
}
pub fn make_merge_event_handler(
    name: EventHandlerTarget, item_generics: &Generics,
    distinguisher: Option<SynTokenStream>, mut groups: Vec<CallGroup>, common: Vec<CallStage>,
) -> SynTokenStream {
    let distinguisher = unwrap_distinguisher(distinguisher);

    let event_generics_raw = quote! {
        '__EventLifetime,
        __EventDispatch: ::static_events::Events,
        __EventType: ::static_events::Event + '__EventLifetime,
        __EventPhase: ::static_events::handlers::EventPhase + '__EventLifetime,
    };
    let event_generics = generics(&event_generics_raw);

    let handler_generics = merge_generics(&event_generics, item_generics);
    let no_lt_generics = strip_lifetimes(&handler_generics);

    let (handler_impl_bounds, handler_ty_param, handler_where_bounds) =
        handler_generics.split_for_impl();
    let (_, self_ty_param, _) = item_generics.split_for_impl();

    let (_, no_lt_ty_param, _) = no_lt_generics.split_for_impl();
    let no_lt_turbofish = no_lt_ty_param.as_turbofish();

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
    let mut async_actions = SynTokenStream::new();
    let mut sync_match = SynTokenStream::new();
    let mut async_match = SynTokenStream::new();

    let mut common_group = CallGroup::new(quote! { }, common);
    common_group.is_common_group = true;
    let mut all_groups = Vec::new();
    all_groups.push(common_group);
    all_groups.append(&mut groups);
    for group in all_groups {
        let mut sync_stage = SynTokenStream::new();
        let mut async_stage = SynTokenStream::new();

        for stage in group.stages {
            let extract_field = &stage.extract_field;
            let field_tp = &stage.field_tp;
            let distinguisher = &stage.distinguisher;

            let is_implemented = is_implemented(field_tp, distinguisher);
            let is_async = is_async(field_tp, distinguisher);
            let call_sync = make_call(quote!(#extract_field), field_tp, false, distinguisher);
            let call_async = make_call(quote!(#extract_field), field_tp, true , distinguisher);

            is_implemented_expr.extend(quote! { || #is_implemented });
            is_async_expr.extend(quote! { || #is_async });
            sync_stage.extend(quote! {{
                match #call_sync {
                    ::static_events::EvOk => { }
                    e => return e,
                }
            }});
            async_stage.extend(quote! {{
                let result = if #is_async {
                    #call_async.await
                } else {
                    #call_sync
                };
                match result {
                    ::static_events::EvOk => { }
                    e => return e,
                }
            }});
        }

        if group.is_common_group {
            sync_actions.extend(sync_stage);
            async_actions.extend(async_stage);
        } else {
            let matcher = &group.matcher;
            sync_match.extend(quote! { #matcher => { #sync_stage } });
            async_match.extend(quote! { #matcher => { #async_stage } });
        }
    }

    quote! {
        async fn __merge_events_async_wrapper #handler_impl_bounds (
            _this: &#name,
            _target: &'__EventLifetime ::static_events::Handler<__EventDispatch>,
            _ev: &'__EventLifetime mut __EventType,
            _state: &'__EventLifetime mut __EventType::State,
        ) -> ::static_events::EventResult #handler_where_bounds {
            #async_actions
            match _this {
                #async_match
            }
            ::static_events::EvOk
        }

        type __MergeEventsWrapperFut #handler_impl_bounds #handler_where_bounds =
                    impl ::std::future::Future<Output = ::static_events::EventResult> +
                    '__EventLifetime;

        impl #handler_impl_bounds #handler_ty for #name #handler_where_bounds {
            const IS_IMPLEMENTED: bool = #is_implemented_expr;
            const IS_ASYNC: bool = #is_async_expr;

            #[inline]
            fn on_phase(
                &'__EventLifetime self,
                _target: &'__EventLifetime ::static_events::Handler<__EventDispatch>,
                _ev: &'__EventLifetime mut __EventType,
                _state: &'__EventLifetime mut __EventType::State,
            ) -> ::static_events::EventResult {
                let _this = self;
                #sync_actions
                match _this {
                    #sync_match
                }
                ::static_events::EvOk
            }

            type FutureType = __MergeEventsWrapperFut #handler_ty_param;

            #[inline]
            fn on_phase_async(
                &'__EventLifetime self,
                target: &'__EventLifetime ::static_events::Handler<__EventDispatch>,
                ev: &'__EventLifetime mut __EventType,
                state: &'__EventLifetime mut __EventType::State,
            ) -> __MergeEventsWrapperFut #handler_ty_param {
                __merge_events_async_wrapper #no_lt_turbofish (self, target, ev, state)
            }
        }
    }
}
