use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::quote;
use syn::{self, Data::Struct, DataStruct, Field, Fields::Named, FieldsNamed, Type};
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

    let setters = gen_setter(&data_idents, &data_types)?;

    let rt = quote!(
        struct #builder_ident {
            #(#data_idents: std::option::Option<#data_types>),*
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
    );
    Ok(rt)
}

fn gen_setter(idents: &Vec<&Option<Ident>>, types: &Vec<&Type>) -> syn::Result<proc_macro2::TokenStream> {
    let mut rt = proc_macro2::TokenStream::new();

    for (ident, type_) in idents.iter().zip(types.iter()) {
        let one_ts = quote!(
            fn #ident(&mut self, value: #type_) -> &mut Self {
                self.#ident = std::option::Option::Some(value);
                self
            }
        );
        rt.extend(one_ts);
    }

    Ok(rt)
}

fn get_data(st: &DeriveInput) -> syn::Result<&Punctuated<Field, Comma>> {
    if let Struct(DataStruct { fields: Named(FieldsNamed { named: ref fields, .. }), .. }) = st.data {
        return Ok(fields);
    }

    Err(syn::Error::new_spanned(st, "No struct"))
}
