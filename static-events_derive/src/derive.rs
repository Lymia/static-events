use crate::common::*;
use crate::utils::*;
use darling::*;
use proc_macro::TokenStream;
use proc_macro2::{TokenStream as SynTokenStream};
use syn::{*, Error};
use syn::export::Span;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use quote::*;

fn dispatch_get_service(field: impl ToTokens) -> SynTokenStream {
    quote! {
        match ::static_events::handlers::Events::get_service::<__DowncastTarget>(#field) {
            Some(t) => return Some(t),
            None => { }
        }
    }
}
fn check_get_service_type(field: impl ToTokens) -> SynTokenStream {
    quote! {
        match ::static_events::private::CheckDowncast::<__DowncastTarget>::downcast_ref(#field) {
            Some(t) => return Some(t),
            None => { }
        }
    }
}

#[derive(Default)]
struct FieldAttrs {
    is_subhandler: bool,
    is_service: bool,
}
impl FieldAttrs {
    fn from_attrs(attrs: &[Attribute]) -> FieldAttrs {
        let mut tp = FieldAttrs::default();
        for attr in attrs {
            match last_path_segment(&attr.path).as_str() {
                "subhandler" => tp.is_subhandler = true,
                "service" => tp.is_service = true,
                _ => { }
            }
        }
        tp
    }
}

#[derive(FromDeriveInput)]
#[darling(attributes(events))]
pub struct EventsAttrs {
    ident: Ident,

    #[darling(default)]
    impl_on_external: Option<Ident>,
}

fn i_ident(i: usize) -> Ident {
    Ident::new(&format!("_field_{}", i), Span::call_site())
}

#[derive(Copy, Clone)]
enum Style {
    Unit, Struct, Tuple,
}
fn to_vec<T, P>(punctuated: &Punctuated<T, P>) -> Vec<&T> {
    punctuated.iter().collect()
}

#[derive(Default)]
struct DerivedImpl {
    arms: Vec<CallGroup>,
    subhandlers_exist: bool,
    get_service_body: SynTokenStream,
}
impl DerivedImpl {
    fn generate_arm(
        &mut self, self_path: SynTokenStream, tp: Style, fields: &[&Field],
        is_struct: bool,
    ) {
        let mut matches = SynTokenStream::new();
        for (i, field) in fields.iter().enumerate() {
            let field_ident = i_ident(i);
            let field_name = &field.ident;
            matches.extend(match tp {
                Style::Unit   => unimplemented!(),
                Style::Struct => quote!(#field_name: #field_ident,),
                Style::Tuple  => quote!(#field_ident,),
            });
        }
        let matches = match tp {
            Style::Unit   => matches,
            Style::Struct => quote! { { #matches } },
            Style::Tuple  => quote! { ( #matches ) },
        };

        let mut get_service = SynTokenStream::new();
        let mut stages = Vec::new();
        for (i, field) in fields.iter().enumerate() {
            let name = match tp {
                Style::Unit   => unimplemented!(),
                Style::Struct => {
                    let ident = &field.ident;
                    quote! { #ident }
                }
                Style::Tuple  => {
                    let i = syn::Index::from(i);
                    quote! { #i }
                }
            };
            let attr = FieldAttrs::from_attrs(&field.attrs);
            let field_ident = i_ident(i);

            if attr.is_service {
                get_service.extend(check_get_service_type(&field_ident));
            }
            if attr.is_subhandler {
                get_service.extend(dispatch_get_service(&field_ident));
                stages.push(CallStage::new(if is_struct {
                    quote! { &_this.#name }
                } else {
                    quote! {
                        if let #self_path #matches = _this {
                            #field_ident
                        } else {
                            ::static_events::private::event_error()
                        }
                    }
                }, &field.ty, None));
                self.subhandlers_exist = true;
            }
        }
        self.arms.push(CallGroup::new(match tp {
            Style::Unit   => quote! { #self_path },
            Style::Struct => quote! { #self_path {..} },
            Style::Tuple  => quote! { #self_path(..) },
        }, stages));
        self.get_service_body.extend(quote!(#self_path #matches => { #get_service }));
    }
    fn generate_arm_fields(
        &mut self, self_path: SynTokenStream, fields: &Fields, is_struct: bool,
    ) {
        match fields {
            Fields::Unit =>
                self.generate_arm(self_path, Style::Unit, &[], is_struct),
            Fields::Named(named) =>
                self.generate_arm(self_path, Style::Struct, &to_vec(&named.named), is_struct),
            Fields::Unnamed(unnamed) =>
                self.generate_arm(self_path, Style::Tuple, &to_vec(&unnamed.unnamed), is_struct),
        }
    }

    fn for_struct(ident: &Ident, fields: &DataStruct) -> DerivedImpl {
        let mut derived = Self::default();
        derived.generate_arm_fields(quote! { #ident }, &fields.fields, true);
        derived
    }
    fn for_enum(ident: &Ident, variants: &DataEnum) -> DerivedImpl {
        let mut derived = Self::default();
        for variant in &variants.variants {
            let variant_ident = &variant.ident;
            derived.generate_arm_fields(quote! { #ident::#variant_ident }, &variant.fields, false);
        }
        derived
    }

    fn make_impl(self, name: &Ident, input: &DeriveInput) -> SynTokenStream {
        let get_service_self = if FieldAttrs::from_attrs(&input.attrs).is_service {
            check_get_service_type(quote!(self))
        } else {
            quote! { }
        };
        let get_service_body = self.get_service_body;

        let (impl_bounds, ty_param, where_bounds) = input.generics.split_for_impl();

        let dist = Some(quote! { ::static_events::private::HandlerImplBlock });
        let event_handler = if self.subhandlers_exist {
            let mut common = Vec::new();
            common.push(CallStage::new(quote! { _this }, quote! { #name #ty_param }, dist));
            make_merge_event_handler(
                EventHandlerTarget::Ident(name), &input.generics, None, self.arms, common,
            )
        } else {
            let event_generics_raw = quote! {
                '__EventLifetime,
                __EventDispatch: ::static_events::Events,
                __EventType: ::static_events::Event + '__EventLifetime,
                __EventPhase: ::static_events::handlers::EventPhase + '__EventLifetime,
            };
            let event_generics = generics(&event_generics_raw);
            let handler_generics = merge_generics(&event_generics, &input.generics);
            let (handler_impl_bounds, handler_ty_param, handler_where_bounds) =
                handler_generics.split_for_impl();

            let is_implemented_expr = is_implemented(quote!(Self), dist.clone());
            let is_async_expr = is_async(quote!(Self), dist.clone());

            quote! {
                type __PassthroughEventFut #handler_impl_bounds #handler_where_bounds =
                    impl ::std::future::Future<Output = ::static_events::EventResult> +
                    '__EventLifetime;
                impl #handler_impl_bounds ::static_events::handlers::EventHandler<
                    '__EventLifetime, __EventDispatch, __EventType, __EventPhase,
                > for #name #handler_where_bounds {
                    const IS_IMPLEMENTED: bool = #is_implemented_expr;
                    const IS_ASYNC: bool = #is_async_expr;

                    #[inline]
                    fn on_phase(
                        &'__EventLifetime self,
                        target: &'__EventLifetime ::static_events::Handler<__EventDispatch>,
                        ev: &'__EventLifetime mut __EventType,
                        state: &'__EventLifetime mut __EventType::State,
                    ) -> ::static_events::EventResult {
                        ::static_events::private::on_phase::<
                            Self, __EventDispatch, __EventType, __EventPhase,
                            ::static_events::private::HandlerImplBlock,
                        >(self, target, ev, state)
                    }

                    type FutureType = __PassthroughEventFut #handler_ty_param;

                    #[inline]
                    fn on_phase_async (
                        &'__EventLifetime self,
                        target: &'__EventLifetime ::static_events::Handler<__EventDispatch>,
                        ev: &'__EventLifetime mut __EventType,
                        state: &'__EventLifetime mut __EventType::State,
                    ) -> __PassthroughEventFut #handler_ty_param {
                        ::static_events::private::on_phase_async::<
                            '__EventLifetime, Self, __EventDispatch, __EventType, __EventPhase,
                            ::static_events::private::HandlerImplBlock,
                        >(self, target, ev, state)
                    }
                }
            }
        };

        quote! {
            #[allow(non_snake_case)]
            const _: () = {
                impl #impl_bounds ::static_events::Events for #name #ty_param #where_bounds {
                    fn get_service<__DowncastTarget>(&self) -> Option<&__DowncastTarget> {
                        #get_service_self
                        match self { #get_service_body }
                        None
                    }
                }

                #event_handler
            };
        }
    }
}

pub fn derive_events(input: TokenStream) -> TokenStream {
    let input: DeriveInput = parse_macro_input!(input);
    let attrs: EventsAttrs = match EventsAttrs::from_derive_input(&input) {
        Ok(attrs) => attrs,
        Err(e) => return e.write_errors().into(),
    };
    let name = attrs.impl_on_external.as_ref().unwrap_or(&attrs.ident);
    let impl_data = match &input.data {
        Data::Struct(data) => DerivedImpl::for_struct(name, data),
        Data::Enum(data) => DerivedImpl::for_enum(name, data),
        Data::Union(_) => {
            return Error::new(
                input.span(), "EventDispatch can only be derived on a struct or enum.",
            ).to_compile_error().into()
        }
    };
    TokenStream::from(impl_data.make_impl(name, &input))
}
