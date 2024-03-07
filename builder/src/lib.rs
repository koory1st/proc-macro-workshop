use proc_macro::TokenStream;
use quote::quote;
use syn::{self, Data::Struct, DataStruct, Field, Fields::Named, FieldsNamed};
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
    let ident_literal = st.ident.to_string();

    let data = get_data(st)?;
    let data_idents:Vec<_>  = data.iter().map(|f| &f.ident).collect();
    let data_types:Vec<_>  = data.iter().map(|f| &f.ty).collect();

    let builder_name = format!("{}Builder", ident_literal);
    let builder_ident = syn::Ident::new(&builder_name, st.span());

    let rt = quote!(
      struct #builder_ident{
            #(#data_idents: std::option::Option<#data_types>),*
      }
    );
    Ok(rt)
}

fn get_data(st: &DeriveInput) -> syn::Result<&Punctuated<Field, Comma>> {
    if let Struct(DataStruct { fields: Named(FieldsNamed { named: ref fields, .. }), .. }) = st.data {
        return Ok(fields);
    }

    Err(syn::Error::new_spanned(st, "No struct"))
}
