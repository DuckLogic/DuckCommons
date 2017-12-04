#![feature(box_patterns, box_syntax)]
#![allow(unused_variables)] // The compiler doesn't know when quote! uses variables :(
#![recursion_limit = "1024"]
extern crate proc_macro;
extern crate syn;
#[macro_use]
extern crate quote;
extern crate regex;
#[macro_use]
extern crate lazy_static;

use std::io::{self, Write};
use std::env::{self, VarError};
use std::collections::HashSet;
use std::time::{Instant, Duration};

use regex::Regex;
use proc_macro::TokenStream;
use syn::{
    DeriveInput, Body, Lit, VariantData, Attribute, MetaItem, NestedMetaItem, Ty, PathParameters,
    Field, Ident
};
use std::process::{Stdio, Command};

/*
 * !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 * TODO: This is a heaping mound of spaghetti code!
 * !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 */

#[proc_macro_derive(GILClone)]
pub fn gil_clone(input: TokenStream) -> TokenStream {
    let start = Instant::now();
    let text = input.to_string();
    let ast = syn::parse_derive_input(&text).unwrap();
    let tokens = impl_gil_clone(&ast);
    debug_derive(start.elapsed(), "GILClone", &ast.ident, &tokens);
    tokens.parse().unwrap()
}
fn impl_gil_clone(ast: &DeriveInput) -> quote::Tokens {
    let name = &ast.ident;
    let clone_impl = match ast.body {
        Body::Enum(ref variants) => {
            let match_cases = variants.iter().map(|variant| {
                let ident = &variant.ident;
                match variant.data {
                    VariantData::Struct(ref fields) => {
                        let clones = fields.iter().map(|field| {
                            let name = field.ident.as_ref().unwrap();
                            quote!(let #name = (*#name).cloned(python))
                        }).collect::<Vec<_>>();
                        let field_names = fields.iter().map(|field| {
                            field.ident.as_ref().unwrap().clone()
                        }).collect::<Vec<_>>();
                        let destructuring = DestructuringStyle::Ref.destructure(&variant.data);
                        quote!(#name::#ident #destructuring => {
                            #(clones;)*
                            #name::#ident { #(field_name),* }
                        })
                    },
                    VariantData::Tuple(ref fields) => {
                        let clones = (0..fields.len()).map(|id| {
                            let ident = Ident::new(format!("value_{}", id));
                            quote!((*#ident).cloned(python))
                        }).collect::<Vec<_>>();
                        let destructuring = DestructuringStyle::Ref.destructure(&variant.data);
                        quote!(#name::#ident #destructuring => #name::#ident(#(#clones),*))
                    },
                    VariantData::Unit => quote!(#name::#ident => #name::#ident),
                }
            }).collect::<Vec<_>>();
            quote! {
                match *self {
                    #(#match_cases),*
                }
            }
        },
        Body::Struct(VariantData::Tuple(ref fields)) if fields.len() == 1
            && is_raw_pointer(&fields[0].ty) => {
            /*
             * This is probably my favorite special-cased hack in this whole mess,
             * where we need to implement GILClone for a newtype wrapper around a raw pointer.
             * The unchecked cast is kinda-sorta unsafe but not really,
             * since the only assumption is that `as_ref` preserves the underlying type.
             */
            quote! {
                let raw_cloned = self.as_ref().cloned(python);
                debug_assert!(Self::check_exact(python, &raw_cloned));
                unsafe { Self::unchecked_cast_from(raw_cloned) }
            }
        },
        Body::Struct(ref body) => {
            let clones = body.fields().iter().map(|field| {
                let name = field.ident.as_ref().unwrap();
                quote!(let #name = self.#name.cloned(python))
            }).collect::<Vec<_>>();
            let field_names = body.fields().iter().map(|field| {
                field.ident.as_ref().unwrap().clone()
            }).collect::<Vec<_>>();
            quote! {
                #(#clones;)*
                #name { #(#field_names),* }
            }
        }
    };
    quote! {
        impl ::magic::clone::GILClone for #name {
            #[allow(unused)]
            #[inline]
            fn cloned(&self, python: Python) -> Self {
                use ::magic::clone::GILClone;
                #clone_impl
            }
        }
    }
}

fn destructure_each<F>(target: &VariantData, mut func: F) -> quote::Tokens
    where F: FnMut(&Field) -> DestructuringStyle {
    match *target {
        VariantData::Struct(ref fields) => {
            let destructuring = fields.iter().map(|field| {
                let name = field.ident.as_ref().unwrap();
                let style = func(field).create();
                quote!(#style #name)
            }).collect::<Vec<_>>();
            quote! { { #(#destructuring,)* } }
        },
        VariantData::Tuple(ref fields) => {
            let destructuring = fields.iter().enumerate().map(|(id, field)| {
                let ident = Ident::new(format!("value_{}", id));
                let style = func(field).create();
                quote!(#style #ident)
            }).collect::<Vec<_>>();
            quote! { (#(#destructuring),*) }
        },
        VariantData::Unit => quote!(())
    }
}

#[derive(Copy, Clone)]
#[allow(dead_code)]
enum DestructuringStyle {
    Ref,
    RefMut,
    Move
}
impl DestructuringStyle {
    #[inline]
    fn destructure(&self, variant: &VariantData) -> quote::Tokens {
        destructure_each(variant, |_| *self)
    }
    #[inline]
    fn create(&self) -> quote::Tokens {
        match *self {
            DestructuringStyle::Ref => quote!(ref),
            DestructuringStyle::RefMut => quote!(ref mut),
            DestructuringStyle::Move => quote!(),
        }
    }
}

#[proc_macro_derive(AutoError, attributes(error))]
pub fn auto_error(input: TokenStream) -> TokenStream {
    let text = input.to_string();
    let start = Instant::now();
    let ast = syn::parse_derive_input(&text).unwrap();
    let tokens = impl_auto_error(&ast);
    debug_derive(start.elapsed(), "AutoError", &ast.ident, &tokens);
    tokens.parse().unwrap()
}
fn impl_auto_error(ast: &DeriveInput) -> quote::Tokens {
    let error_name = &ast.ident;
    let defaults = ErrorConfiguration::determine(&ast.attrs)
        .unwrap_or_else(|reason| panic!("Invalid attributes for {}, {}", error_name, reason));
    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();
    match ast.body {
        Body::Enum(ref variants) => {
            let mut description_cases = Vec::new();
            let mut cause_cases = Vec::new();
            let mut display_cases = Vec::new();
            let mut from_implementations = Vec::new();
            for variant in &*variants {
                let name = &variant.ident;
                let config = ErrorConfiguration::determine(&variant.attrs)
                    .unwrap_or_else(|reason| {
                        panic!("Invalid attributes for {}::{}, {}", error_name, name, reason)
                    });
                let description = config.description.as_ref().unwrap_or_else(|| {
                    defaults.description.as_ref().unwrap_or_else(|| {
                        panic!("Missing description for {}::{}", error_name, name)
                    })
                });
                let destructuring = DestructuringStyle::Ref.destructure(&variant.data);
                let case = quote!(#error_name::#name #destructuring =>);
                description_cases.push(quote! { #case #description });
                let cause_field = find_field(variant.data.fields(), "cause");
                let cause_type = cause_field.map(|field| &field.ty);
                cause_cases.push(if cause_field.is_some() && !config.cause_ignored {
                    let cause_name = cause_field.unwrap().ident.as_ref();
                    quote!(#case Some(#cause_name))
                } else {
                    quote!(#case None)
                });
                if (cause_field.is_some() && variant.data.fields().len() == 1) || config.force_from {
                    if variant.data.fields().len() != 1 {
                        unimplemented!("Default fields for From")
                    }
                    let cause_type = cause_type.unwrap_or_else(|| {
                        panic!("{}::{} must have a `cause` field to derive a From implementation!", error_name, name)
                    });
                    from_implementations.push(quote! {
                        impl #impl_generics From<#cause_type> for #error_name #ty_generics #where_clause {
                            #[inline]
                            fn from(cause: #cause_type) -> Self {
                                #error_name::#name { cause }
                            }
                        }
                    })
                }
                if let Some(display) = config.display_format.as_ref().or_else(|| defaults.display_format.as_ref()) {
                    let passed_fields = DisplayString::parse(display)
                        .access_variables();
                    display_cases.push(quote!(#case write!(f, #display, #passed_fields)))
                } else {
                    display_cases.push(quote!(#case f.write_str(#description)))
                }
            }
            quote! {
                impl #impl_generics ::utils::AutoError for #error_name #ty_generics #where_clause {}
                impl #impl_generics ::std::error::Error for #error_name #ty_generics #where_clause {
                    #[inline]
                    #[allow(unused)]
                    fn description(&self) -> &str {
                        match *self {
                            #(#description_cases,)*
                        }
                    }
                    #[inline]
                    #[allow(unused)]
                    fn cause(&self) -> Option<&::std::error::Error> {
                        match *self {
                            #(#cause_cases,)*
                        }
                    }
                }
                impl #impl_generics ::std::fmt::Display for #error_name #ty_generics #where_clause {
                    #[inline]
                    #[allow(unused)]
                    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                        match *self {
                            #(#display_cases,)*
                        }
                    }
                }
                #(#from_implementations)*
            }
        },
        Body::Struct(VariantData::Tuple(ref fields)) if fields.len() == 1 => {
            // Implement newtype structs by delegating to the underlying error
            quote! {
                impl #impl_generics ::utils::AutoError for #error_name #ty_generics #where_clause {}
                impl #impl_generics ::std::error::Error for #error_name #ty_generics #where_clause {
                    #[inline]
                    fn description(&self) -> &str {
                        self.0.description()
                    }
                    #[inline]
                    fn cause(&self) -> Option<&::std::error::Error> {
                        self.0.cause()
                    }
                }
                impl #impl_generics ::std::fmt::Display for #error_name #ty_generics #where_clause {
                    #[inline]
                    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                        write!(f, "{}", self.0)
                    }
                }
            }
        }
        Body::Struct(ref variant) => {
            let description = defaults.description.unwrap_or_else(|| {
                panic!("Missing description for {}", error_name)
            });
            let display = if let Some(ref display) = defaults.display_format {
                let passed_fields = DisplayString::parse(display)
                    .access_fields(quote!(self));
                quote!(write!(f, #display, #passed_fields))
            } else {
                quote!(f.write_str(#description))
            };
            let cause = find_field(variant.fields(), "cause")
                .map(|field| &field.ty);
            let give_cause = if cause.is_some() { quote! { Some(&self.cause) } } else { quote!(None) };
            quote! {
                impl #impl_generics ::utils::AutoError for #error_name #ty_generics #where_clause {}
                impl #impl_generics ::std::error::Error for #error_name #ty_generics #where_clause {
                    #[inline]
                    fn description(&self) -> &str {
                        #description
                    }
                    #[inline]
                    fn cause(&self) -> Option<&::std::error::Error> {
                        #give_cause
                    }
                }
                impl #impl_generics ::std::fmt::Display for #error_name #ty_generics #where_clause {
                    #[inline]
                    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                        #display
                    }
                }
            }
        }
    }
}

struct DisplayString {
    indexed_fields: u32,
    named_fields: HashSet<String>
}
impl DisplayString {
    #[inline]
    fn use_indexed_field(&mut self, index: u32) {
        self.indexed_fields = self.indexed_fields.max(index + 1);
    }
    fn access_variables(&self) -> quote::Tokens {
        self.access(
            |index| {
                let name = Ident::new(format!("value_{}", index));
                quote!(#name)
            },
            |name| quote!(#name)
        )
    }
    fn access_fields(&self, target: quote::Tokens) -> quote::Tokens {
        self.access(
            |index| quote!(#target.#index),
            |name| quote!(#target.#name)
        )
    }
    #[inline]
    fn access<IF, NF>(&self, mut indexed: IF, mut named: NF) -> quote::Tokens
        where IF: FnMut(u32) -> quote::Tokens, NF: FnMut(&Ident) -> quote::Tokens {
        let mut result = quote::Tokens::new();
        for index in 0..self.indexed_fields {
            result.append(indexed(index));
            result.append(",");
        }
        for name in &self.named_fields {
            result.append(name);
            result.append("=");
            result.append(named(&Ident::new(name.clone())));
            result.append(",");
        }
        result
    }
}
impl DisplayString {
    fn parse(target: &str) -> DisplayString {
        let mut remaining = target;
        let mut result = DisplayString {
            indexed_fields: 0,
            named_fields: HashSet::with_capacity(16)
        };
        // TODO: Handle escaped `{{` and `}}` characters
        let mut current_index = 0;
        while let Some(index) = remaining.find('{') {
            remaining = &remaining[index + 1..];
            if let Some(end) = remaining.find('}') {
                let (name, newly_remaining) = remaining.split_at(end);
                remaining = newly_remaining;
                let name = if let Some(meta_end) = name.rfind(':') {
                    &name[..meta_end]
                } else {
                    name
                };
                if name.is_empty() {
                    result.use_indexed_field(current_index);
                    current_index += 1;
                } else if let Ok(index) = name.parse::<u32>() {
                    result.use_indexed_field(index);
                } else {
                    result.named_fields.insert(name.to_owned());
                }
            } else {
                panic!("Unmatched `{{` in {:?}", target)
            }
        }
        result
    }
}

/*
#[proc_macro_derive(ContextReallocate, attributes(context))]
pub fn context_reallocate(input: TokenStream) -> TokenStream {
    let text = input.to_string();
    let ast = syn::parse_derive_input(&text).unwrap();
    let tokens = context_reallocate_impl(&ast);
    debug_derive("ContextReallocate", &ast.ident, &tokens);
    tokens.parse().unwrap()
}
fn context_reallocate_impl(target: &DeriveInput) -> quote::Tokens {
    let target_name = &target.ident;
    let mut context_type = None;
    'attributeHandling: for attr in &*target.attrs {
        if attr.name() == "context" {
            if let MetaItem::List(_, ref nested) = attr.value {
                if nested.len() == 1 {
                    if let NestedMetaItem::MetaItem(MetaItem::Word(ref value)) = nested[0] {
                        if let Some(ref existing) = context_type {
                            panic!("Specified multiple contexts: {} and {}", existing, value);
                        }
                        context_type = Some(value.clone());
                        continue 'attributeHandling;
                    }
                }
            }
            panic!("Invalid context attribute: {}", create_tokens(attr));
        }
    }
    let context_type = context_type.expect("Must specify a context to reallocate in!");
    match target.body {
        Body::Enum(ref variants) => {
            let mut reallocate_cases = Vec::new();
            for variant in &*variants {
                let name = &variant.ident;
                let destructuring = DestructuringStyle::Ref.destructure(&variant.data);
                reallocate_cases.push(match variant.data {
                    VariantData::Struct(ref fields) => {
                        let clones = fields.iter().map(|field| {
                            let name = field.ident.as_ref().unwrap();
                            quote!(let #name = (*#name).reallocate(target_context))
                        }).collect::<Vec<_>>();
                        let field_names = fields.iter().map(|field| {
                            field.ident.as_ref().unwrap().clone()
                        }).collect::<Vec<_>>();
                        quote!(#target_name::#name #destructuring => {
                            #(#clones;)*
                            #target_name::#name { #(#field_names),* }
                        })
                    },
                    VariantData::Tuple(ref fields) => {
                        let clones = fields.iter().enumerate().map(|(id, _)| {
                            let name = Ident::new(format!("value_{}", id));
                            quote!((*#name).reallocate(target_context))
                        }).collect::<Vec<_>>();
                        quote!(#target_name::#name #destructuring => {
                            #target_name::#name(#(#clones),*)
                        })
                    },
                    VariantData::Unit => quote!(#target_name::#name => #target_name::#name)
                });
            }
            quote! {
                impl<'a, 'b> ContextReallocate<'a, 'b, #context_type> for #target_name<'a> {
                    type Reallocated = #target_name<'b>;
                    fn reallocate(&self, target_context: &'b #context_type) -> #target_name<'b> {
                        match *self {
                            #(#reallocate_cases,)*
                        }
                    }
                }
            }
        },
        Body::Struct(VariantData::Struct(ref fields)) => {
            let clones = fields.iter().map(|field| {
                let name = field.ident.as_ref().unwrap();
                quote!(let #name = (*#name).reallocate(target_context))
            }).collect::<Vec<_>>();
            let field_names = fields.iter().map(|field| {
                field.ident.as_ref().unwrap().clone()
            }).collect::<Vec<_>>();
            quote! {
                impl<'a, 'b> ContextReallocate<'a, 'b, #context_type> for #target_name<'a> {
                    type Reallocated = #target_name<'b>;
                    fn reallocate(&self, target_context: &'b #context_type) -> #target_name<'b> {
                        #(clones;)*
                        #target_name { #(field_names,)* }
                    }
                }
            }
        },
        Body::Struct(VariantData::Tuple(_)) => unimplemented!("#[derive(ContextReallocate)] for tuple structs!"),
        Body::Struct(VariantData::Unit) => unimplemented!("#[derive(ContextReallocate)] for unit structs!")
    }
}
*/


#[proc_macro_derive(SimpleParseError, attributes(parse_error))]
pub fn simple_parse_error(input: TokenStream) -> TokenStream {
    let text = input.to_string();
    let start = Instant::now();
    let ast = syn::parse_derive_input(&text).unwrap();
    let tokens = simple_parse_error_impl(&ast);
    debug_derive(start.elapsed(), "SimpleParseError", &ast.ident, &tokens);
    tokens.parse().unwrap()
}
fn simple_parse_error_impl(ast: &DeriveInput) -> quote::Tokens {
    let error_name = &ast.ident;
    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();
    match ast.body {
        Body::Enum(ref variants) => {
            let mut cause_cases = Vec::new();
            let mut index_offset_cases = Vec::new();
            let mut index_cases = Vec::new();
            let mut from_implementations = Vec::new();
            for variant in variants {
                let name = &variant.ident;
                let cause = find_field(variant.data.fields(), "cause")
                    .map(|field| &field.ty);
                let index_type = find_field(variant.data.fields(), "index")
                    .map(|field| &field.ty)
                    .unwrap_or_else(|| panic!("{}::{} must have an index field!", error_name, name));
                let optional_index = match BasicType::determine(index_type) {
                    Some(BasicType::MachineInteger(false)) => false,
                    Some(BasicType::Option(box BasicType::MachineInteger(false))) => true,
                    _ => panic!("index field for {}::{} must be a usize or Option<usize>")
                };
                if cause.is_some() {
                    cause_cases.push(quote!(#error_name::#name { ref cause, .. } => ::parse::_cast_parse_error(cause)));
                } else {
                    cause_cases.push(quote!(#error_name::#name { .. } => None));
                }
                if let Some(cause) = cause {
                    if variant.data.fields().len() == 2 {
                        from_implementations.push(quote! {
                            impl  #impl_generics  ::parse::FromParseError<#cause> for #error_name #ty_generics #where_clause {
                                #[inline]
                                fn from_cause(index: usize, cause: #cause) -> Self {
                                    #error_name::#name { index, cause }
                                }
                            }
                        })
                    }
                }
                if !optional_index {
                    index_offset_cases.push(quote! {
                        #error_name::#name { ref mut index, .. } => {
                            *index = new_index as usize;
                        }
                    });
                } else {
                    index_offset_cases.push(quote! {
                        #error_name::#name { ref mut index, .. } => {
                            *index = Some(new_index as usize);
                        }
                    });
                }
                if !optional_index {
                    index_cases.push(quote!(#error_name::#name { index, .. } => index));
                } else {
                    index_cases.push(quote!(#error_name::#name { index, .. } => index.unwrap_or_else(|| ::parse::_missing_index(self))))
                }
            }
            quote! {
                impl #impl_generics ::parse::SimpleParseError for #error_name #ty_generics #where_clause {
                    #[inline]
                    fn index(&self) -> usize {
                        match *self {
                            #(#index_cases),*
                        }
                    }
                    #[inline]
                    fn offset(&mut self, offset: isize) {
                        // NOTE: We need to look before we leap to keep the borrow checker happy -_-
                        let new_index = (self.index() as isize) + offset;
                        assert!(new_index >= 0, "Unable to offset {:?} by {}", self, offset);
                        match *self {
                            #(#index_offset_cases),*
                        }
                    }
                    #[inline]
                    fn parse_cause(&self) -> Option<&::parse::SimpleParseError> {
                        match *self {
                            #(#cause_cases),*
                        }
                    }
                }
                #(#from_implementations)*
            }
        },
        Body::Struct(VariantData::Tuple(ref fields)) => {
            assert_eq!(fields.len(), 1, "Can only derive SimpleParseError for newtype tuple structs!");

            let delegate_type = &fields[0];
            quote! {
                impl #impl_generics ::parse::SimpleParseError for #error_name #ty_generics #where_clause {
                    #[inline]
                    fn index(&self) -> usize {
                        self.0.index()
                    }
                    #[inline]
                    fn offset(&mut self, offset: isize) {
                        self.0.offset(offset);
                    }
                    #[inline]
                    fn parse_cause(&self) -> Option<&::parse::SimpleParseError> {
                        self.0.parse_cause()
                    }
                }
            }
        },
        Body::Struct(ref variant) => {
            let cause = find_field(variant.fields(), "cause")
                .map(|field| &field.ty);
            let give_cause = if cause.is_some() { quote! { Some(&self.cause) } } else { quote!(None) };
            quote! {
                impl #impl_generics ::parse::SimpleParseError for #error_name #ty_generics #where_clause {
                    #[inline]
                    fn index(&self) -> usize {
                        self.index
                    }
                    #[inline]
                    fn offset(&mut self, offset: isize) {
                        let result = (self.index as isize) + offset;
                        assert!(result >= 0, "Unable to offset {:?} by {}", self, offset);
                        self.index = result as usize;
                    }
                    #[inline]
                    fn parse_cause(&self) -> Option<&::parse::SimpleParseError> {
                        #give_cause
                    }
                }
            }
        }
    }
}
#[derive(Debug)]
struct ErrorConfiguration {
    description: Option<String>,
    force_from: bool,
    cause_ignored: bool,
    display_format: Option<String>
}
impl ErrorConfiguration {
    fn determine(attrs: &[Attribute]) -> Result<ErrorConfiguration, String> {
        let mut description = None;
        let mut display_format = None;
        let mut cause_ignored = false;
        let mut force_from = false;
        for attr in attrs {
            if attr.name() == "error" {
                if let MetaItem::List(_, ref entries) = attr.value {
                    'attributeHandling: for entry in entries.iter() {
                        if let NestedMetaItem::MetaItem(MetaItem::List(ref name, ref items)) = *entry {
                            match (name.as_ref(), items.len()) {
                                ("description", 1) => {
                                    if let NestedMetaItem::Literal(Lit::Str(ref value, _)) = items[0] {
                                        if description.is_some() {
                                            return Err("Multiple error descriptions given".to_owned())
                                        }
                                        description = Some(value.clone());
                                        continue 'attributeHandling;
                                    }
                                },
                                ("display", 1) => {
                                    if let NestedMetaItem::Literal(Lit::Str(ref value, _)) = items[0] {
                                        if display_format.is_some() {
                                            return Err("Multiple error display formats given".to_owned())
                                        }
                                        display_format = Some(value.clone());
                                        continue 'attributeHandling;
                                    }
                                },
                                ("from", 1) => {
                                    if let NestedMetaItem::MetaItem(MetaItem::Word(ref option)) = items[0] {
                                        match option.as_ref() {
                                            "forced" => {
                                                if force_from {
                                                    return Err("Already from(forced)".to_owned())
                                                }
                                                force_from = true;
                                            },
                                            _ => {
                                                return Err(format!("Unknown from option: from({})", option))
                                            }
                                        }
                                        continue 'attributeHandling;
                                    }
                                }
                                ("cause", 1) => {
                                    if let NestedMetaItem::MetaItem(MetaItem::Word(ref option)) = items[0] {
                                        match option.as_ref() {
                                            "ignored" => {
                                                if cause_ignored {
                                                    return Err("Already cause(ignored)".to_owned())
                                                }
                                                cause_ignored = true;
                                            },
                                            _ => {
                                                return Err(format!("Unknown cause option: cause({})", option))
                                            }
                                        }
                                        continue 'attributeHandling;
                                    }
                                },
                                _ => {}
                            }
                        }
                        // Fallthrough to more specific error
                        return Err(format!("Invalid nested attribute: `{}`: {:#?}", create_tokens(entry), entry))
                    }
                } else {
                    // Otherwise we need to error
                    return Err(format!("Invalid attribute `{}`: {:#?}", create_tokens(attr), attr))
                }
            }
        }
        Ok(ErrorConfiguration {
            description, display_format, force_from, cause_ignored
        })
    }
}
#[inline]
fn is_raw_pointer(target: &Ty) -> bool {
    if let Ty::Ptr(_) = *target {
        true
    } else {
        false
    }
}
enum BasicType {
    /// A machine sized integer (usize) with the specified flag indicating if it's signed
    MachineInteger(bool),
    /// An integer with the specified number of bits,
    /// with negative values denoting signed integers and positive indicating signed.
    Integer(i32),
    Option(Box<BasicType>)
}
lazy_static! {
    static ref INTEGER_PATTERN: Regex = Regex::new("([iu])(8|16|32|64|128)").unwrap();
}
impl BasicType {
    fn determine(target: &Ty) -> Option<BasicType> {
        if let Ty::Path(None, ref path) = *target {
            if path.segments.len() == 1 {
                let segment = &path.segments[0];
                let name = segment.ident.as_ref();
                match name {
                    "Option" => {
                        if let PathParameters::AngleBracketed(ref params) = segment.parameters {
                            if let Some(known) = BasicType::determine(&params.types[0]) {
                                return Some(BasicType::Option(box known))
                            }
                        }
                    },
                    "usize" => return Some(BasicType::MachineInteger(false)),
                    "isize" => return Some(BasicType::MachineInteger(true)),
                    _ => {
                        if let Some(captures) = INTEGER_PATTERN.captures(name) {
                            let signed = &captures[1] == "i";
                            let bits = captures[2].parse::<i32>().unwrap();
                            return Some(BasicType::Integer(if signed { -bits } else { bits }));
                        }
                    }
                }
            }
        }
        None
    }
}

fn find_field<'a>(targets: &'a [Field], name: &str) -> Option<&'a Field> {
    for field in targets {
        if let Some(ref ident) = field.ident {
            if ident == name {
                return Some(field)
            }
        }
    }
    None
}

#[inline]
fn create_tokens<T>(target: &T) -> ::quote::Tokens where T: ::quote::ToTokens {
    let mut tokens = ::quote::Tokens::new();
    target.to_tokens(&mut tokens);
    tokens
}
fn debug_derive(duration: Duration, name: &str, ident: &Ident, tokens: &quote::Tokens) {
    match env::var("DEBUG_DERIVE") {
        Ok(target) => {
            if target == name {
                println!("derive({}) for {}:", name, ident);
                let indent = " ".repeat(2);
                let code = match rustfmt(tokens.as_str()) {
                    Ok(Some(formatted)) => formatted,
                    Ok(None) => {
                        println!("{}// NOTE: Failed to rustfmt", indent);
                        tokens.as_str().to_owned()
                    },
                    Err(e) => panic!("Failed to rustfmt: {}")
                };
                for line in code.lines() {
                    println!("{}{}", indent, line);
                }
            }
        }
        Err(VarError::NotPresent) => {},
        Err(VarError::NotUnicode(_)) => panic!("Invalid unicode for DEBUG_DERIVE!")
    }
    match env::var("PROFILE_DERIVE") {
        Ok(target) => {
            if target == "1" || target == name {
                println!("derive({}) for {}: {} ms", name, ident, milliseconds(duration))
            }
        },
        Err(VarError::NotPresent) => {},
        Err(VarError::NotUnicode(_)) => panic!("Invalid unicode for PROFILE_DERIVE!")
    }
}
#[inline]
fn milliseconds(duration: Duration) -> u64 {
    ((duration.as_secs() as f64) * 10e3
        + (duration.subsec_nanos() as f64) * 10e-6).round() as u64
}
fn rustfmt(target: &str) -> io::Result<Option<String>> {
    let mut child = Command::new("rustfmt")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::null()) // Suppress warnings
        .spawn()?;
    child.stdin.as_mut().unwrap()
        .write_all(target.as_bytes())?;
    let output = child.wait_with_output()?;
    match output.status.code() {
        Some(0) | Some(3) => Ok(Some(String::from_utf8(output.stdout).expect("Invalid UTF8 from rustfmt"))),
        Some(2) => Ok(None), // Failed to parse properly
        _ => panic!("Unexpected error running rustfmt")
    }
}
