use crate::common::*;
use proc_macro::TokenStream;
use proc_macro2::{TokenStream as SynTokenStream};
use syn::*;
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
    is_subhandler: bool, is_service: bool,
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

fn i_ident(i: usize) -> Ident {
    Ident::new(&format!("_field_{}", i), Span::call_site())
}

#[derive(Copy, Clone)]
enum ContainerType {
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
    fn generate_arm(&mut self, self_path: SynTokenStream, tp: ContainerType, fields: &[&Field]) {
        let mut matches = SynTokenStream::new();
        let mut get_service = SynTokenStream::new();
        let mut stages = Vec::new();
        for (i, field) in fields.iter().enumerate() {
            let field_ident = i_ident(i);
            let field_name = &field.ident;
            matches.extend(match tp {
                ContainerType::Unit   => unimplemented!(),
                ContainerType::Struct => quote!(#field_name: #field_ident,),
                ContainerType::Tuple  => quote!(#field_ident,),
            });
        }
        let matches = match tp {
            ContainerType::Unit   => matches,
            ContainerType::Struct => quote! { { #matches } },
            ContainerType::Tuple  => quote! { ( #matches ) },
        };
        for (i, field) in fields.iter().enumerate() {
            let attr = FieldAttrs::from_attrs(&field.attrs);
            let field_ident = i_ident(i);

            if attr.is_service {
                get_service.extend(check_get_service_type(&field_ident));
            }
            if attr.is_subhandler {
                get_service.extend(dispatch_get_service(&field_ident));
                stages.push(CallStage::new(quote! {
                    if let #self_path #matches = this {
                        #field_ident
                    } else {
                        unsafe { ::std::hint::unreachable_unchecked() }
                    }
                }, &field.ty, None));
                self.subhandlers_exist = true;
            }
        }
        self.arms.push(CallGroup::new(quote! {
            if let #self_path #matches = this { true } else { false }
        }, stages));
        self.get_service_body.extend(quote!(#self_path #matches => { #get_service }));
    }
    fn generate_arm_fields(&mut self, self_path: SynTokenStream, fields: &Fields) {
        match fields {
            Fields::Unit =>
                self.generate_arm(self_path, ContainerType::Unit, &[]),
            Fields::Named(named) =>
                self.generate_arm(self_path, ContainerType::Struct, &to_vec(&named.named)),
            Fields::Unnamed(unnamed) =>
                self.generate_arm(self_path, ContainerType::Tuple, &to_vec(&unnamed.unnamed)),
        }
    }

    fn for_struct(input: &DeriveInput, fields: &DataStruct) -> DerivedImpl {
        let mut derived = Self::default();
        let ident = &input.ident;
        derived.generate_arm_fields(quote! { #ident }, &fields.fields);
        derived
    }
    fn for_enum(input: &DeriveInput, variants: &DataEnum) -> DerivedImpl {
        let mut derived = Self::default();
        let ident = &input.ident;
        for variant in &variants.variants {
            let variant_ident = &variant.ident;
            derived.generate_arm_fields(quote! { #ident::#variant_ident }, &variant.fields);
        }
        derived
    }

    fn make_impl(self, ctx: &GensymContext, input: &DeriveInput) -> SynTokenStream {
        let get_service_self = if FieldAttrs::from_attrs(&input.attrs).is_service {
            check_get_service_type(quote!(self))
        } else {
            quote! { }
        };
        let get_service_body = self.get_service_body;

        let name = &input.ident;
        let (impl_bounds, ty_param, where_bounds) = input.generics.split_for_impl();

        let dist = Some(quote! { ::static_events::private::HandlerImplBlock });
        let event_handler = if self.subhandlers_exist {
            let mut common = Vec::new();
            common.push(CallStage::new(quote! { this }, quote! { #name #ty_param }, dist));
            make_merge_event_handler(&ctx, EventHandlerTarget::Ident(name),
                                     &input.generics, None, self.arms, common)
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

            let existential_ty = ctx.gensym("ExistentialFuture");
            quote! {
                existential type #existential_ty #handler_impl_bounds #handler_where_bounds:
                    ::std::future::Future<Output = ::static_events::EventResult> +
                    '__EventLifetime;
                impl #handler_impl_bounds ::static_events::handlers::EventHandler<
                    '__EventLifetime, __EventDispatch, __EventType, __EventPhase,
                > for #name #handler_where_bounds {
                    const IS_IMPLEMENTED: bool = false #is_implemented_expr;
                    const IS_ASYNC: bool = false #is_async_expr;

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

                    type FutureType = #existential_ty #handler_ty_param;

                    #[inline]
                    fn on_phase_async (
                        &'__EventLifetime self,
                        target: &'__EventLifetime ::static_events::Handler<__EventDispatch>,
                        ev: &'__EventLifetime mut __EventType,
                        state: &'__EventLifetime mut __EventType::State,
                    ) -> #existential_ty #handler_ty_param {
                        ::static_events::private::on_phase_async::<
                            '__EventLifetime, Self, __EventDispatch, __EventType, __EventPhase,
                            ::static_events::private::HandlerImplBlock,
                        >(self, target, ev, state)
                    }
                }
            }
        };

        let impl_name = ctx.gensym("DeriveWrapper");
        quote! {
            #[allow(non_snake_case)]
            const #impl_name: () = {
                impl #impl_bounds ::static_events::Events for #name #ty_param #where_bounds {
                    fn get_service<__DowncastTarget>(&self) -> Option<&__DowncastTarget> {
                        #get_service_self
                        match self { #get_service_body }
                        None
                    }
                }

                #event_handler

                ()
            };
        }
    }
}

pub fn derive_events(input: TokenStream) -> TokenStream {
    let ctx = GensymContext::new(&input);
    let input: DeriveInput = parse_macro_input!(input);
    let impl_data = match &input.data {
        Data::Struct(data) => DerivedImpl::for_struct(&input, data),
        Data::Enum(data) => DerivedImpl::for_enum(&input, data),
        Data::Union(_) => {
            input.span()
                .unstable()
                .error("EventDispatch can only be derived from a struct or enum.")
                .emit();
            return TokenStream::new()
        }
    };
    TokenStream::from(impl_data.make_impl(&ctx, &input))
}
