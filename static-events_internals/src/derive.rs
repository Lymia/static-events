use crate::common::*;
use crate::errors::Result;
use crate::utils::*;
use darling::*;
use proc_macro2::{TokenStream as SynTokenStream};
use syn::*;
use syn::export::Span;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use quote::*;

fn dispatch_get_service(crate_name: &SynTokenStream, field: impl ToTokens) -> SynTokenStream {
    quote! {
        match #crate_name::handlers::Events::get_service::<__DowncastTarget>(#field) {
            Some(t) => return Some(t),
            None => { }
        }
    }
}
fn check_get_service_type(crate_name: &SynTokenStream, field: impl ToTokens) -> SynTokenStream {
    quote! {
        match #crate_name::private::CheckDowncast::<__DowncastTarget>::downcast_ref(#field) {
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
struct EventsAttrs {
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

/// A type for including the effect of `#[derive(Events)]` in a procedural macro.
pub struct DeriveStaticEvents {
    arms: Vec<CallGroup>,
    crate_name: SynTokenStream,
    subhandlers_exist: bool,
    get_service_body: SynTokenStream,
    discriminator: Vec<Type>,
    name: Ident,
    input: DeriveInput,
    is_async_handler: bool,
}
impl DeriveStaticEvents {
    /// Parses a derive input block.
    pub fn new(
        input: &DeriveInput, crate_name: Option<SynTokenStream>, is_async_handler: bool,
    ) -> Result<Self> {
        let crate_name = crate_name.unwrap_or(quote! { ::static_events });

        let attrs: EventsAttrs = EventsAttrs::from_derive_input(input)?;
        let name = attrs.impl_on_external.as_ref().unwrap_or(&attrs.ident);
        let name = name.clone();

        let mut data = DeriveStaticEvents {
            arms: Vec::new(),
            crate_name,
            subhandlers_exist: false,
            get_service_body: Default::default(),
            discriminator: Vec::new(),
            name: name.clone(),
            input: input.clone(),
            is_async_handler,
        };
        match &input.data {
            Data::Struct(input_data) => data.generate_struct(&name, input_data),
            Data::Enum(input_data) => data.generate_enum(&name, input_data),
            Data::Union(_) =>
                error(input.span(), "EventDispatch can only be derived on a struct or enum.")?,
        }
        Ok(data)
    }

    /// Sets the crate name to use. Overrides any manual tags on the derive.
    pub fn set_crate_name(&mut self, name: impl ToTokens) {
        self.crate_name = name.to_token_stream();
    }

    /// Parses a token stream.
    pub fn from_tokens(toks: impl ToTokens, is_async_handler: bool) -> Result<Self> {
        Self::new(&mut parse2(toks.into_token_stream())?, None, is_async_handler)
    }

    /// Parses a token stream.
    pub fn from_tokens_raw(toks: proc_macro::TokenStream, is_async_handler: bool) -> Result<Self> {
        Self::new(&mut parse(toks)?, None, is_async_handler)
    }

    fn generate_arm(
        &mut self, self_path: SynTokenStream, tp: Style, fields: &[&Field],
        is_struct: bool,
    ) {
        let crate_name = &self.crate_name;

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
                get_service.extend(check_get_service_type(&self.crate_name, &field_ident));
            }
            if attr.is_subhandler {
                get_service.extend(dispatch_get_service(&self.crate_name, &field_ident));
                stages.push(CallStage::new(crate_name, if is_struct {
                    quote! { &_this.#name }
                } else {
                    quote! {
                        if let #self_path #matches = _this {
                            #field_ident
                        } else {
                            #crate_name::private::event_error()
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

    fn generate_struct(
        &mut self, ident: &Ident, fields: &DataStruct,
    ) {
        self.generate_arm_fields(quote! { #ident }, &fields.fields, true);
    }
    fn generate_enum(
        &mut self, ident: &Ident, variants: &DataEnum,
    ) {
        for variant in &variants.variants {
            let variant_ident = &variant.ident;
            self.generate_arm_fields(quote! { #ident::#variant_ident }, &variant.fields, false);
        }
    }

    /// Adds a discriminator to the events handler.
    pub fn add_discriminator(&mut self, discriminator: Type) {
        self.discriminator.push(discriminator);
    }

    /// Generates an impl block for the events handler.
    pub fn generate(self) -> SynTokenStream {
        let crate_name = &self.crate_name;
        
        let name = self.name;
        let input = self.input;

        let get_service_self = if FieldAttrs::from_attrs(&input.attrs).is_service {
            check_get_service_type(crate_name, quote!(self))
        } else {
            quote! { }
        };
        let get_service_body = self.get_service_body;

        let (impl_bounds, ty_param, where_bounds) = input.generics.split_for_impl();
        let dist = Some(quote! { #crate_name::private::HandlerImplBlock });

        let mut common = Vec::new();
        common.push(CallStage::new(
            crate_name, quote! { _this }, quote! { #name #ty_param }, dist,
        ));
        for dist in self.discriminator {
            common.push(CallStage::new(
                crate_name,
                quote! { _this }, quote! { #name #ty_param }, Some(quote! { #dist }),
            ));
        }

        let event_handler = make_merge_event_handler(
            crate_name, self.is_async_handler,
            EventHandlerTarget::Ident(&name), &input.generics, None, self.arms, common,
        );
        let sync_event_impl = if self.is_async_handler {
            quote! {
                impl #impl_bounds #crate_name::handlers::AsyncEvents
                    for #name #ty_param #where_bounds { }
            }
        } else {
            SynTokenStream::new()
        };

        quote! {
            #[allow(non_snake_case)]
            const _: () = {
                impl #impl_bounds #crate_name::handlers::Events for #name #ty_param #where_bounds {
                    fn get_service<__DowncastTarget>(&self) -> Option<&__DowncastTarget> {
                        #get_service_self
                        match self { #get_service_body }
                        None
                    }
                }
                #sync_event_impl
                #event_handler
            };
        }
    }
}