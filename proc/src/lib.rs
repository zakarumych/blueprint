extern crate proc_macro;

use proc_macro::TokenStream;
use quote;
use syn::{self, spanned::Spanned};

#[proc_macro_derive(Blueprinted, attributes(blueprint))]
pub fn blueprinted_derive(input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);

    let (impl_generics, type_generics, where_clause) = input.generics.split_for_impl();

    match &input.data {
        syn::Data::Union(data) => {
            return quote::quote_spanned!(data.union_token.span() => ::core::compile_error!("Union are not supported")).into();
        }
        syn::Data::Struct(data) => {
            let ident = &input.ident;
            let name = syn::LitStr::new(&ident.to_string(), input.ident.span());

            match &data.fields {
                syn::Fields::Unit => {
                    quote::quote_spanned! { input.span() =>
                        impl ::blueprint::Blueprinted for #ident {
                            const BLUEPRINT: &'static ::blueprint::Blueprint<'static> = &::blueprint::Blueprint {
                                name: ::blueprint::RefBox::Ref(#name),
                                kind: ::blueprint::BlueprintKind::Unit,
                            };
                        }
                    }
                }
                syn::Fields::Named(fields) => {
                    let fields = fields.named.iter().map(|field| {
                        let ident = field.ident.as_ref().unwrap();
                        let name = syn::LitStr::new(&ident.to_string(), input.ident.span());
                        let ty = &field.ty;

                        quote::quote_spanned! { field.span() =>
                            ::blueprint::FieldBlueprint {
                                name: ::blueprint::RefBox::Ref(#name),
                                blueprint: ::blueprint::RefBox::Ref( <#ty as ::blueprint::Blueprinted>::BLUEPRINT ),
                            }
                        }
                    });

                    quote::quote_spanned! { input.span() =>
                        impl #impl_generics ::blueprint::Blueprinted for #ident #type_generics #where_clause {
                            const BLUEPRINT: &'static ::blueprint::Blueprint<'static> = &::blueprint::Blueprint {
                                name: ::blueprint::RefBox::Ref(#name),
                                kind: ::blueprint::BlueprintKind::Struct(::blueprint::StructBlueprint {
                                    fields: ::blueprint::RefBox::Ref(&[
                                        #( #fields, )*
                                    ]),
                                }),
                            };
                        }
                    }
                }

                syn::Fields::Unnamed(fields) => {
                    let fields = fields.unnamed.iter().map(|field| {
                        let ty = &field.ty;

                        quote::quote_spanned! { field.span() =>
                            ::blueprint::RefBox::Ref( <#ty as ::blueprint::Blueprinted>::BLUEPRINT )
                        }
                    });

                    quote::quote_spanned! { input.span() =>
                        impl #impl_generics ::blueprint::Blueprinted for #ident #type_generics #where_clause {
                            const BLUEPRINT: &'static ::blueprint::Blueprint<'static> = &::blueprint::Blueprint {
                                name: ::blueprint::RefBox::Ref(#name),
                                kind: ::blueprint::BlueprintKind::Tuple( ::blueprint::TupleBlueprint {
                                    elements: ::blueprint::RefBox::Ref(&[
                                        #( #fields, )*
                                    ]),
                                }),
                            };
                        }
                    }
                }
            }
        }
        syn::Data::Enum(data) => {
            let variants = data.variants.iter().map(|variant| {
                let variant_ident = &variant.ident;
                let variant_name = syn::LitStr::new(&variant_ident.to_string(), variant_ident.span());
                
                match &variant.fields {
                    syn::Fields::Unit => {
                        quote::quote_spanned! { input.span() =>
                            ::blueprint::VariantBlueprint {
                                name: ::blueprint::RefBox::Ref(#variant_name),
                                kind: ::blueprint::VariantBlueprintKind::Unit,
                            }
                        }
                    }
                    syn::Fields::Named(fields) => {
                        let fields = fields.named.iter().map(|field| {
                            let ident = field.ident.as_ref().unwrap();
                            let name = syn::LitStr::new(&ident.to_string(), input.ident.span());
                            let ty = &field.ty;

                            quote::quote_spanned! { field.span() =>
                                ::blueprint::FieldBlueprint {
                                    name: ::blueprint::RefBox::Ref(#name),
                                    blueprint: ::blueprint::RefBox::Ref( <#ty as ::blueprint::Blueprinted>::BLUEPRINT ),
                                }
                            }
                        });

                        quote::quote_spanned! { input.span() =>
                            ::blueprint::VariantBlueprint {
                                name: ::blueprint::RefBox::Ref(#variant_name),
                                kind: ::blueprint::VariantBlueprintKind::Struct(::blueprint::StructBlueprint {
                                    fields: ::blueprint::RefBox::Ref(&[
                                        #( #fields, )*
                                    ]),
                                }),
                            }
                        }
                    }

                    syn::Fields::Unnamed(fields) => {
                        let fields = fields.unnamed.iter().map(|field| {
                            let ty = &field.ty;

                            quote::quote_spanned! { field.span() =>
                                ::blueprint::RefBox::Ref( <#ty as ::blueprint::Blueprinted>::BLUEPRINT )
                            }
                        });

                        quote::quote_spanned! { input.span() =>
                            ::blueprint::VariantBlueprint {
                                name: ::blueprint::RefBox::Ref(#variant_name),
                                kind: ::blueprint::VariantBlueprintKind::Tuple( ::blueprint::TupleBlueprint {
                                    elements: ::blueprint::RefBox::Ref(&[
                                        #( #fields, )*
                                    ]),
                                }),
                            }
                        }
                    }
            }});

            let ident = &input.ident;
            let name = syn::LitStr::new(&ident.to_string(), input.ident.span());

            quote::quote_spanned! { input.span() =>
                impl #impl_generics ::blueprint::Blueprinted for #ident #type_generics #where_clause {
                    const BLUEPRINT: &'static ::blueprint::Blueprint<'static> = &::blueprint::Blueprint {
                        name: ::blueprint::RefBox::Ref(#name),
                        kind: ::blueprint::BlueprintKind::Enum( ::blueprint::EnumBlueprint {
                            variants: ::blueprint::RefBox::Ref(&[
                                #( #variants, )*
                            ]),
                        }),
                    };
                }
            }
        }
    }
    .into()
}
