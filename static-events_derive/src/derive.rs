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
    on_phase_body: SynTokenStream,
    on_phase_async_body: SynTokenStream,
    get_service_body: SynTokenStream,
    is_implemented_expr: SynTokenStream,
    is_async_expr: SynTokenStream,
}
impl DerivedImpl {
    fn generate_arm(&mut self, self_path: SynTokenStream, tp: ContainerType, fields: &[&Field]) {
        let mut matches = SynTokenStream::new();
        let mut on_phase = SynTokenStream::new();
        let mut on_phase_async = SynTokenStream::new();
        let mut get_service = SynTokenStream::new();
        for (i, field) in fields.iter().enumerate() {
            let attrs = FieldAttrs::from_attrs(&field.attrs);
            let field_ident = i_ident(i);
            let field_name = &field.ident;

            matches.extend(match tp {
                ContainerType::Unit   => unimplemented!(),
                ContainerType::Struct => quote!(#field_name: #field_ident,),
                ContainerType::Tuple  => quote!(#field_ident,),
            });
            if attrs.is_service {
                get_service.extend(check_get_service_type(&field_ident));
            }
            if attrs.is_subhandler {
                on_phase.extend(dispatch_on_phase(&field_ident, &field.ty, false, None));
                on_phase_async.extend(dispatch_on_phase(&field_ident, &field.ty, true, None));
                get_service.extend(dispatch_get_service(&field_ident));
                self.is_implemented_expr.extend(is_implemented(&field.ty, None));
                self.is_async_expr.extend(is_async(&field.ty, None));
            }
        }
        let matches = match tp {
            ContainerType::Unit   => matches,
            ContainerType::Struct => quote! { { #matches } },
            ContainerType::Tuple  => quote! { ( #matches ) },
        };
        self.on_phase_body      .extend(quote!(#self_path #matches => { #on_phase       }));
        self.on_phase_async_body.extend(quote!(#self_path #matches => { #on_phase_async }));
        self.get_service_body   .extend(quote!(#self_path #matches => { #get_service    }));
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
        let DerivedImpl {
            on_phase_body, on_phase_async_body, get_service_body,
            is_implemented_expr, is_async_expr,
        } = self;
        let attrs = FieldAttrs::from_attrs(&input.attrs);

        let dist = Some(quote! { ::static_events::private::HandlerImplBlock });
        let on_phase_self = dispatch_on_phase(quote!(self), quote!(Self), false, dist.clone());
        let on_phase_async_self = dispatch_on_phase(quote!(self), quote!(Self), true, dist.clone());
        let is_implemented_self = is_implemented(quote!(Self), dist.clone());
        let is_async_self = is_async(quote!(Self), dist.clone());
        let get_service_self = if attrs.is_service {
            check_get_service_type(quote!(self))
        } else {
            quote! { }
        };

        let name = &input.ident;
        let (impl_bounds, ty_param, where_bounds) = input.generics.split_for_impl();
        let event_handler = make_universal_event_handler(
            &ctx, EventHandlerTarget::Ident(name), &input.generics, None,
            quote! {
                false #is_implemented_self #is_implemented_expr
            },
            quote! {
                false #is_async_self #is_async_expr
            },
            quote! {
                #on_phase_self
                match self { #on_phase_body }
                ::static_events::EvOk
            },
            quote! {
                #on_phase_async_self
                match self { #on_phase_async_body }
                ::static_events::EvOk
            },
        );

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
