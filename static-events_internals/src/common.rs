use crate::utils::*;
use proc_macro2::{TokenStream as SynTokenStream};
use syn::*;
use quote::*;

fn unwrap_distinguisher(
    crate_name: &SynTokenStream, distinguisher: Option<impl ToTokens>,
) -> SynTokenStream {
    match distinguisher {
        Some(x) => x.into_token_stream(),
        None => quote! { #crate_name::handlers::DefaultHandler },
    }
}
fn bool_val(
    crate_name: &SynTokenStream,
    tp: impl ToTokens, distinguisher: impl ToTokens, call: impl ToTokens,
) -> SynTokenStream {
    quote! {
        #crate_name::private::#call::<
            '__EventLifetime, #tp, __EventDispatch, __EventType, __EventPhase, #distinguisher,
        >()
    }
}

/// Generates `|| (...)::IS_IMPLEMENTED`
pub fn is_implemented(
    crate_name: &SynTokenStream, tp: impl ToTokens, distinguisher: impl ToTokens,
) -> SynTokenStream {
    bool_val(crate_name, tp, distinguisher, quote! { is_implemented })
}
/// Generates `|| (...)::IS_ASYNC`
pub fn is_async(
    crate_name: &SynTokenStream, tp: impl ToTokens, distinguisher: impl ToTokens,
) -> SynTokenStream {
    bool_val(crate_name, tp, distinguisher, quote! { is_async })
}

/// A particular call to make during an event dispatch.
pub struct CallStage {
    extract_field: SynTokenStream, field_tp: SynTokenStream, distinguisher: SynTokenStream,
}
impl CallStage {
    pub fn new(
        crate_name: &SynTokenStream,
        extract_field: impl ToTokens, field_tp: impl ToTokens,
        distinguisher: Option<SynTokenStream>,
    ) -> CallStage {
        CallStage {
            extract_field: extract_field.into_token_stream(),
            field_tp: field_tp.into_token_stream(),
            distinguisher: unwrap_distinguisher(crate_name, distinguisher),
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
    crate_name: &SynTokenStream,
    field: impl ToTokens, tp: impl ToTokens, is_async: bool, distinguisher: impl ToTokens,
) -> SynTokenStream {
    let call_fn = if is_async { quote! { on_phase_async } } else { quote! { on_phase } };
    quote! {
        #crate_name::private::#call_fn::<
            #tp, __EventDispatch, __EventType, __EventPhase, #distinguisher,
        >(#field, _target, _ev, _state)
    }
}
pub fn make_merge_event_handler(
    crate_name: &SynTokenStream, is_async_handler: bool,
    name: EventHandlerTarget, item_generics: &Generics,
    distinguisher: Option<SynTokenStream>, mut groups: Vec<CallGroup>, common: Vec<CallStage>,
) -> SynTokenStream {
    let distinguisher = unwrap_distinguisher(crate_name, distinguisher);

    let event_generics = generics(&quote! {
        '__EventLifetime,
        __EventDispatch: #crate_name::handlers::Events,
        __EventType: #crate_name::events::Event + '__EventLifetime,
        __EventPhase: #crate_name::handlers::EventPhase + '__EventLifetime,
    });

    let handler_generics = merge_generics(&event_generics, item_generics);

    let (handler_impl_bounds, _, handler_where_bounds) =
        handler_generics.split_for_impl();
    let (_, self_ty_param, _) = item_generics.split_for_impl();

    let name = match name {
        EventHandlerTarget::Ident(id) => quote! { #id #self_ty_param },
        EventHandlerTarget::Type(tp) => quote! { #tp },
    };

    let handler_ty = quote! { #crate_name::handlers::EventHandler<
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

            let is_implemented = is_implemented(crate_name, field_tp, distinguisher);
            let is_async = is_async(crate_name, field_tp, distinguisher);
            let call_sync = make_call(
                crate_name, quote!(#extract_field), field_tp, false, distinguisher,
            );
            let call_async = make_call(
                crate_name, quote!(#extract_field), field_tp, true , distinguisher,
            );

            is_implemented_expr.extend(quote! { || #is_implemented });
            is_async_expr.extend(quote! { || #is_async });
            sync_stage.extend(quote! {{
                match #call_sync {
                    #crate_name::events::EventResult::EvOk => { }
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
                    #crate_name::events::EventResult::EvOk => { }
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
    let async_events = if is_async_handler {
        let async_event_generics = generics(&quote! {
            '__EventLifetime,
            __EventDispatch: #crate_name::handlers::SyncEvents,
            __EventType: #crate_name::events::SyncEvent + '__EventLifetime,
            __EventPhase: #crate_name::handlers::EventPhase + '__EventLifetime,
        });

        let async_handler_ty = quote! { #crate_name::handlers::AsyncEventHandler<
            '__EventLifetime, __EventDispatch, __EventType, __EventPhase, #distinguisher,
        > };

        let async_handler_generics = merge_generics(&async_event_generics, item_generics);
        let no_lt_generics = strip_lifetimes(&async_handler_generics);

        let (async_handler_impl_bounds, async_handler_ty_param, async_handler_where_bounds) =
            async_handler_generics.split_for_impl();

        let (_, no_lt_ty_param, _) = no_lt_generics.split_for_impl();
        let no_lt_turbofish = no_lt_ty_param.as_turbofish();

        quote! {
            async fn __merge_events_async_wrapper #async_handler_impl_bounds (
                _this: &#name,
                _target: &'__EventLifetime #crate_name::handlers::Handler<__EventDispatch>,
                _ev: &'__EventLifetime mut __EventType,
                _state: &'__EventLifetime mut __EventType::State,
            ) -> #crate_name::events::EventResult #async_handler_where_bounds {
                #async_actions
                match _this {
                    #async_match
                }
                #crate_name::events::EventResult::EvOk
            }

            #crate_name::private::allow_existentials! {
                (__MergeEventsWrapperFut #async_handler_impl_bounds #async_handler_where_bounds)
                (::std::future::Future<Output = #crate_name::handlers::EventResult> +
                    '__EventLifetime)
            }

            impl #async_handler_impl_bounds #async_handler_ty
                for #name #async_handler_where_bounds
            {
                type FutureType = __MergeEventsWrapperFut #async_handler_ty_param;

                #[inline]
                fn on_phase_async(
                    &'__EventLifetime self,
                    target: &'__EventLifetime #crate_name::handlers::Handler<__EventDispatch>,
                    ev: &'__EventLifetime mut __EventType,
                    state: &'__EventLifetime mut __EventType::State,
                ) -> __MergeEventsWrapperFut #async_handler_ty_param {
                    __merge_events_async_wrapper #no_lt_turbofish (self, target, ev, state)
                }
            }
        }
    } else {
        SynTokenStream::new()
    };

    quote! {
        impl #handler_impl_bounds #handler_ty for #name #handler_where_bounds {
            const IS_IMPLEMENTED: bool = #is_implemented_expr;
            const IS_ASYNC: bool = #is_async_expr;

            #[inline]
            fn on_phase(
                &'__EventLifetime self,
                _target: &'__EventLifetime #crate_name::handlers::Handler<__EventDispatch>,
                _ev: &'__EventLifetime mut __EventType,
                _state: &'__EventLifetime mut __EventType::State,
            ) -> #crate_name::events::EventResult {
                let _this = self;
                #sync_actions
                match _this {
                    #sync_match
                }
                #crate_name::events::EventResult::EvOk
            }
        }

        #async_events
    }
}
