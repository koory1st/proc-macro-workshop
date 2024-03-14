use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::quote;
use syn::{self, Data::Struct, DataStruct, Field, Fields::Named, FieldsNamed, Type, Path};
use syn::DeriveInput;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::token::Comma;

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let st = syn::parse_macro_input!(input as syn::DeriveInput);

    let rt = get_expand(&st);

    match rt {
        Ok(ts) => {ts.into()}
        Err(e) => {e.into_compile_error().into()}
    }
}

fn get_expand(st: &DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let ident = &st.ident;
    let ident_literal = st.ident.to_string();

    let data = get_data(st)?;
    let data_idents:Vec<_>  = data.iter().map(|f| &f.ident).collect();
    let data_types:Vec<_>  = data.iter().map(|f| &f.ty).collect();

    let builder_name = format!("{}Builder", ident_literal);
    let builder_ident = syn::Ident::new(&builder_name, st.span());

    let (builder_fields, setters) = gen_setter(&data_idents, &data_types)?;
    let build_fn = gen_build_fields(&data_idents)?;

    let rt = quote!(
        struct #builder_ident {
            #builder_fields
        }
        impl #ident {
          pub fn builder() -> #builder_ident {
            #builder_ident {
              #(#data_idents: std::option::Option::None),*
            }
          }
        }
        impl #builder_ident {
            #setters
        }
        impl CommandBuilder {
            fn build(&self) -> std::result::Result<Command, Box<dyn std::error::Error>> {
                Ok(Command {
                    #build_fn
                })
            }
        }
    );
    Ok(rt)
}

fn gen_build_fields(idents: &Vec<&Option<Ident>>) -> syn::Result<proc_macro2::TokenStream> {
    let mut rt = proc_macro2::TokenStream::new();
    for ident in idents.iter() {
        let one = quote!(
            #ident: self.#ident.clone().unwrap(),
        );
        rt.extend(one);
    }

    Ok(rt)
}

fn gen_setter(idents: &Vec<&Option<Ident>>, types: &Vec<&Type>) -> syn::Result<(proc_macro2::TokenStream, proc_macro2::TokenStream)> {
    let mut setter_fields = proc_macro2::TokenStream::new();
    let mut builder_fields = proc_macro2::TokenStream::new();

    for (ident, type_) in idents.iter().zip(types.iter()) {
        let ty = match get_type_in_option(type_) {
            None => type_,
            Some(t) => t,
        };

        let setter_ts = quote!(
            fn #ident(&mut self, value: #ty) -> &mut Self {
                self.#ident = std::option::Option::Some(value);
                self
            }
        );
        eprintln!("setter_ts:{:#?}", &setter_ts);
        setter_fields.extend(setter_ts);

        let builder_ts = quote!(
            #ident: std::option::Option<#ty>,
        );
        eprintln!("builder_ts:{:#?}", &builder_ts);
        builder_fields.extend(builder_ts);
    }

    Ok((builder_fields, setter_fields))
}

fn get_type_in_option(type_:&Type) -> Option<&Type> {
    if let syn::Type::Path(syn::TypePath{path: syn::Path{ref segments, ..},..}) = type_ {
        if let Some(segment) = segments.last() {
            if segment.ident != "Option" {
                return Some(type_);
            }
            if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments{ref args, ..}) = segment.arguments {
                if let Some(syn::GenericArgument::Type(ref ty)) = args.first() {
                    return Some(ty);
                }
            }
        }
    }
    None
}

fn get_data(st: &DeriveInput) -> syn::Result<&Punctuated<Field, Comma>> {
    if let Struct(DataStruct { fields: Named(FieldsNamed { named: ref fields, .. }), .. }) = st.data {
        return Ok(fields);
    }

    Err(syn::Error::new_spanned(st, "No struct"))
}
