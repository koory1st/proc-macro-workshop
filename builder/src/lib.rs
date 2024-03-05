use proc_macro::TokenStream;
use quote::quote;
use syn;
use syn::DeriveInput;
use syn::spanned::Spanned;

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
    // let data = st.data;

    let builder_name = format!("{}Builder", ident_literal);
    let builder_ident = syn::Ident::new(&builder_name, st.span());

    let rt = quote!(
      struct #builder_ident{}
    );
    Ok(rt)
}
