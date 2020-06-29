use proc_macro::TokenStream;
use static_events_internals::*;

#[proc_macro_derive(Events, attributes(subhandler, service, events))]
pub fn derive_events(input: TokenStream) -> TokenStream {
    try_syn!(DeriveStaticEvents::from_tokens_raw(input, false)).generate().into()
}

#[proc_macro_derive(SyncEvents, attributes(subhandler, service, events))]
pub fn derive_sync_events(input: TokenStream) -> TokenStream {
    try_syn!(DeriveStaticEvents::from_tokens_raw(input, true)).generate().into()
}

#[proc_macro_attribute]
pub fn events_impl(_: TokenStream, item: TokenStream) -> TokenStream {
    try_syn!(EventsImplAttr::from_tokens_raw(item, false)).generate().into()
}

#[proc_macro_attribute]
pub fn sync_events_impl(_: TokenStream, item: TokenStream) -> TokenStream {
    try_syn!(EventsImplAttr::from_tokens_raw(item, true)).generate().into()
}

derived_attr!(event_handler, events_impl);
