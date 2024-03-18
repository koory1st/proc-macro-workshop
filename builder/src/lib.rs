use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::quote;
use syn::{self, Data::Struct, DataStruct, Field, Fields::Named, FieldsNamed, Type, Path, Error, Attribute};
use syn::DeriveInput;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::token::Comma;

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let st = syn::parse_macro_input!(input as DeriveInput);

    let rt = get_expand(&st);

    match rt {
        Ok(ts) => {ts.into()}
        Err(e) => {e.into_compile_error().into()}
    }
}

fn get_expand(st: &DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let ident = &st.ident;
    let ident_literal = st.ident.to_string();

    let builder_name = format!("{}Builder", ident_literal);
    let builder_ident = syn::Ident::new(&builder_name, st.span());

    let data = get_data(st)?;

    let mut setter_fields = proc_macro2::TokenStream::new();
    let mut builder_struct_fields = proc_macro2::TokenStream::new();
    let mut builder_to_fields = proc_macro2::TokenStream::new();
    let mut build_fn_fields = proc_macro2::TokenStream::new();
    for one in data.iter() {
        let each_content = get_each_content(&one.attrs);
        let field_ident = &one.ident;
        let (field_type_ident, is_option) = get_type(&one.ty)?;

        let setter_ts = quote!(
            fn #field_ident(&mut self, value: #field_type_ident) -> &mut Self {
                self.#field_ident = std::option::Option::Some(value);
                self
            }
        );
        setter_fields.extend(setter_ts);

        let builder_ts = quote!(
            #field_ident: std::option::Option<#field_type_ident>,
        );
        builder_struct_fields.extend(builder_ts);

        let build_fn_ts = if is_option {
            quote!(
                #field_ident: self.#field_ident.clone(),
            )
        } else {
            quote!(
                #field_ident: self.#field_ident.clone().unwrap(),
            )
        };
        build_fn_fields.extend(build_fn_ts);

        let builder_to_ts = quote!(
            #field_ident: std::option::Option::None,
        );
        builder_to_fields.extend(builder_to_ts);
    }
    let rt = quote!(
        struct #builder_ident {
            #builder_struct_fields
        }
        impl #ident {
          pub fn builder() -> #builder_ident {
            #builder_ident {
              #builder_to_fields
            }
          }
        }
        impl #builder_ident {
            #setter_fields
            fn build(&self) -> std::result::Result<Command, Box<dyn std::error::Error>> {
                Ok(Command {
                    #build_fn_fields
                })
            }
        }
    );
    Ok(rt)
}

fn get_each_content(attrs: &Vec<Attribute>) -> Option<proc_macro2::TokenStream> {
    if attrs.len()  == 0 {
        return None;
    }

    let path = attrs.iter().map(|item| {
        if item.path.segments.iter().find(|ident| { ident.ident == "builder" }).is_none() {
            return;
        }
        // let each_field = item.tokens
    });
    dbg!(&attrs);

    None
}

fn get_type(type_:&Type) -> syn::Result<(&Type,bool)> {
    if let Type::Path(syn::TypePath{path: Path{ref segments, ..},..}) = type_ {
        if let Some(segment) = segments.last() {
            if segment.ident != "Option" {
                return Ok((type_, false));
            }
            if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments { ref args, .. }) = segment.arguments {
                if let Some(syn::GenericArgument::Type(ref ty)) = args.first() {
                    return Ok((ty, true));
                }
            }
        }
    }
    Err(Error::new_spanned(type_, "No type"))
}


fn get_data(st: &DeriveInput) -> syn::Result<&Punctuated<Field, Comma>> {
    if let Struct(DataStruct { fields: Named(FieldsNamed { named: ref fields, .. }), .. }) = st.data {
        return Ok(fields);
    }

    Err(Error::new_spanned(st, "No struct"))
}
