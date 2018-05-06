#![feature(box_patterns, box_syntax)]
#![allow(unused_variables)] // The compiler doesn't know when quote! uses variables :(
#![allow(dead_code)] // TODO: Cleanup
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
    DeriveInput, Data, Lit, Attribute, Path, Type, PathArguments, AngleBracketedGenericArguments,
    Field, Ident, Fields, Meta, NestedMeta, DataEnum, DataStruct, GenericArgument,
    MetaList, MetaNameValue, TypePath
};
use quote::ToTokens;
use std::process::{Stdio, Command};

/*
 * !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 * TODO: This is a heaping mound of spaghetti code!
 * !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 */

fn destructure_each<F>(fields: &Fields, mut func: F) -> quote::Tokens
    where F: FnMut(&Field) -> DestructuringStyle {
    match *fields {
        Fields::Named(_) => {
            let destructuring = fields.iter().map(|field| {
                let name = field.ident.as_ref().unwrap();
                let style = func(field).create();
                quote!(#style #name)
            }).collect::<Vec<_>>();
            quote! { { #(#destructuring,)* } }
        },
        Fields::Unnamed(_) => {
            let destructuring = fields.iter().enumerate().map(|(id, field)| {
                let ident = Ident::from(format!("value_{}", id));
                let style = func(field).create();
                quote!(#style #ident)
            }).collect::<Vec<_>>();
            quote! { (#(#destructuring),*) }
        },
        Fields::Unit => quote!()
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
    fn destructure(&self, variant: &Fields) -> quote::Tokens {
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


#[proc_macro_derive(Step)]
pub fn step(input: TokenStream) -> TokenStream {
    let start = Instant::now();
    let ast = syn::parse(input).unwrap();
    let tokens = impl_step(&ast);
    debug_derive(start.elapsed(), "Step", &ast.ident, &tokens);
    tokens.into()
}

fn impl_step(ast: &DeriveInput) -> quote::Tokens {
    let name = &ast.ident;
    if let Data::Struct(DataStruct { fields: ref fields @ Fields::Unnamed(_), .. }) = ast.data {
        assert_eq!(fields.iter().len(), 1, "Unable to derive Step for {}, must have only one field", name);
        match BasicType::determine(&fields.iter().nth(0).unwrap().ty) {
            Some(BasicType::MachineInteger(_)) | Some(BasicType::Integer(_)) => {
                quote! {
                    impl ::std::iter::Step for #name {
                        #[inline]
                        fn steps_between(start: &#name, end: &#name) -> Option<usize> {
                            if *start <= *end {
                                Some(end.0.wrapping_sub(start.0) as usize)
                            } else {
                                None
                            }
                        }

                        #[inline]
                        fn replace_one(&mut self) -> Self {
                            ::std::mem::replace(self, #name(1))
                        }

                        #[inline]
                        fn replace_zero(&mut self) -> Self {
                            ::std::mem::replace(self, #name(0))
                        }

                        #[inline]
                        fn add_one(&self) -> Self {
                            #name(self.0 + 1)
                        }

                        #[inline]
                        fn sub_one(&self) -> Self {
                            #name(self.0 - 1)
                        }

                        #[inline]
                        fn add_usize(&self, n: usize) -> Option<#name> {
                            ::std::iter::Step::add_usize(&self.0, n).map(#name)
                        }
                    }
                }
            },
            _ => {
                panic!(
                    "Unable to derive Step for {}, invalid inner type {:?}",
                    name, &fields.iter().nth(0).unwrap().ty
                )
            }
        }
    } else {
        panic!("Unable to derive Step for {}, must be tuple struct", name)
    }
}

/// Implement `slog::SerdeValue` on the specified type by delegating to `SerializeValue`
#[proc_macro_derive(SerdeValue, attributes(serialize_fallback))]
pub fn serde_value(input: TokenStream) -> TokenStream {
    let start = Instant::now();
    let ast = syn::parse(input).unwrap();
    let tokens = impl_serde_value(&ast);
    debug_derive(start.elapsed(), "SerdeValue", &ast.ident, &tokens);
    tokens.into()
}

fn impl_serde_value(ast: &DeriveInput) -> ::quote::Tokens {
    let target_name = &ast.ident;
    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();
    let serialize_fallback = match ast.attrs.iter()
        .find(|p| p.path == Path::from("serialize_fallback")) {
        None => String::from(r##"format!("{:?}", self)"##),
        Some(ref attribute) => {
            if let Some(Meta::NameValue(MetaNameValue { lit: Lit::Str(ref value), .. })) = attribute.interpret_meta() {
                value.value()
            } else {
                panic!("Invalid attribute: {:?}", attribute)
            }
        }
    };
    quote! {
        impl #impl_generics ::slog::Value for #target_name #ty_generics #where_clause {
            #[inline]
            fn serialize(&self, _record: &::slog::Record, key: ::slog::Key, serializer: &mut ::slog::Serializer) -> ::slog::Result {
                serializer.emit_serde(key, self)
            }
        }
        impl #impl_generics ::slog::SerdeValue for #target_name #ty_generics #where_clause {
            fn serialize_fallback(&self, key: ::slog::Key, serializer: &mut ::slog::Serializer) -> ::slog::Result {
                let value = #serialize_fallback;
                serializer.emit_str(key, &*value)
            }

            #[inline]
            fn as_serde(&self) -> &::erased_serde::Serialize {
                self
            }
            #[inline]
            fn to_sendable(&self) -> Box<::slog::SerdeValue + Send + 'static> {
                let value = ::duckcommons::SerializeValue(self);
                ::slog::SerdeValue::to_sendable(&value)
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
                let name = Ident::from(format!("value_{}", index));
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
            result.append_all(indexed(index));
            ",".to_tokens(&mut result);
        }
        for name in &self.named_fields {
            name.to_tokens(&mut result);
            "=".to_tokens(&mut result);
            result.append_all(named(&Ident::from(name.clone())));
            ",".to_tokens(&mut result);
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

#[proc_macro_derive(SimpleParseError, attributes(parse_error))]
pub fn simple_parse_error(input: TokenStream) -> TokenStream {
    let start = Instant::now();
    let ast = syn::parse(input).unwrap();
    let tokens = simple_parse_error_impl(&ast);
    debug_derive(start.elapsed(), "SimpleParseError", &ast.ident, &tokens);
    tokens.into()
}
fn simple_parse_error_impl(ast: &DeriveInput) -> quote::Tokens {
    let error_name = &ast.ident;
    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();
    match ast.data {
        Data::Enum(DataEnum { ref variants, .. }) => {
            let mut cause_cases = Vec::new();
            let mut location_mut_cases = Vec::new();
            let mut location_cases = Vec::new();
            let mut from_implementations = Vec::new();
            for variant in variants {
                let name = &variant.ident;
                let cause = find_field(variant.fields.iter(), "cause")
                    .map(|field| &field.ty);
                let location_type = find_field(variant.fields.iter(), "location")
                    .map(|field| &field.ty)
                    .unwrap_or_else(|| panic!("{}::{} must have a location field!", error_name, name));
                let optional_location = match BasicType::determine(location_type) {
                    Some(BasicType::NamedType(ref name)) if name == "Location" => false,
                    Some(BasicType::Option(box BasicType::NamedType(ref name))) if name == "Location" => true,
                    _ => panic!("location field for {}::{} must be a Location or Option<Location>")
                };
                if cause.is_some() {
                    cause_cases.push(quote!(#error_name::#name { ref cause, .. } => ::duckcommons::parse::_cast_parse_error(cause)));
                } else {
                    cause_cases.push(quote!(#error_name::#name { .. } => None));
                }
                if let Some(cause) = cause {
                    if variant.fields.iter().len() == 2 {
                        from_implementations.push(quote! {
                            impl  #impl_generics ::duckcommons::parse::FromParseError<#cause> for #error_name #ty_generics #where_clause {
                                #[inline]
                                fn from_cause(location: Location, cause: #cause) -> Self {
                                    #error_name::#name { location, cause }
                                }
                            }
                        })
                    }
                }
                if !optional_location {
                    location_mut_cases.push(quote!(#error_name::#name { ref mut location, .. } => location));
                } else {
                    location_mut_cases.push(quote! {
                        #error_name::#name { location: Some(ref mut location), .. } => location,
                        #error_name::#name { location: None, .. } => ::duckcommons::parse::_missing_index(self)
                    });
                }
                if !optional_location {
                    location_cases.push(quote!(#error_name::#name { location, .. } => location  ));
                } else {
                    location_cases.push(quote! {
                        #error_name::#name { location: Some(location), .. } => location,
                        #error_name::#name { location: None, .. } => ::duckcommons::parse::_missing_index(self)
                    })
                }
            }
            quote! {
                impl #impl_generics ::duckcommons::parse::SimpleParseError for #error_name #ty_generics #where_clause {
                    #[inline]
                    fn location(&self) -> ::duckcommons::parse::Location {
                        match *self {
                            #(#location_cases),*
                        }
                    }
                    #[inline]
                    fn location_mut(&mut self) -> &mut ::duckcommons::parse::Location {
                        match *self {
                            #(#location_mut_cases),*
                        }
                    }
                    #[inline]
                    fn parse_cause(&self) -> Option<&::duckcommons::parse::SimpleParseError> {
                        match *self {
                            #(#cause_cases),*
                        }
                    }
                }
                #(#from_implementations)*
            }
        },
        Data::Struct(DataStruct { fields: ref fields @ Fields::Unnamed(_) , .. }) => {
            assert_eq!(fields.iter().len(), 1, "Can only derive SimpleParseError for newtype tuple structs!");
            let delegate_type = fields.iter().nth(0).unwrap();
            quote! {
                impl #impl_generics ::duckcommons::parse::SimpleParseError for #error_name #ty_generics #where_clause {
                    #[inline]
                    fn location(&self) -> ::duckcommons:parse::Location {
                        self.0.location()
                    }
                    #[inline]
                    fn location_mut(&self) -> &mut ::duckcommons:parse::Location {
                        self.0.location_mut()
                    }
                    #[inline]
                    fn parse_cause(&self) -> Option<&::duckcommons::parse::SimpleParseError> {
                        self.0.parse_cause()
                    }
                }
            }
        },
        Data::Struct(DataStruct { fields: ref fields @ Fields::Named(_), .. }) => {
            let cause = find_field(fields, "cause")
                .map(|field| &field.ty);
            let give_cause = if cause.is_some() { quote! { Some(&self.cause) } } else { quote!(None) };
            quote! {
                impl #impl_generics ::duckcommons::parse::SimpleParseError for #error_name #ty_generics #where_clause {
                    #[inline]
                    fn location(&self) -> ::duckcommons:parse::Location {
                        self.location
                    }
                    #[inline]
                    fn location_mut(&mut self) -> &mut ::duckcommons:parse::Location {
                        &mut self.location
                    }
                    #[inline]
                    fn parse_cause(&self) -> Option<&::duckcommons::parse::SimpleParseError> {
                        #give_cause
                    }
                }
            }
        },
        Data::Struct(DataStruct { fields: Fields::Unit, .. }) => unimplemented!("Unit structs"),
        Data::Union(_) => unimplemented!("Unions")
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
        let error_path = Path::from("error");
        for attr in attrs {
            if attr.path == error_path {
                if let Some(Meta::List(MetaList { ref nested, .. })) = attr.interpret_meta() {
                    'attributeHandling: for entry in nested.iter() {
                        if let NestedMeta::Meta(Meta::List(MetaList { ref ident, ref nested, .. })) = *entry {
                            match (ident.as_ref(), nested.len()) {
                                ("description", 1) => {
                                    if let NestedMeta::Literal(Lit::Str(ref value)) = nested[0] {
                                        if description.is_some() {
                                            return Err("Multiple error descriptions given".to_owned())
                                        }
                                        description = Some(value.value());
                                        continue 'attributeHandling;
                                    }
                                },
                                ("display", 1) => {
                                    if let NestedMeta::Literal(Lit::Str(ref value)) = nested[0] {
                                        if display_format.is_some() {
                                            return Err("Multiple error display formats given".to_owned())
                                        }
                                        display_format = Some(value.value());
                                        continue 'attributeHandling;
                                    }
                                },
                                ("from", 1) => {
                                    if let NestedMeta::Meta(Meta::Word(ref option)) = nested[0] {
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
                                    if let NestedMeta::Meta(Meta::Word(ref option)) = nested[0] {
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
#[derive(Debug)]
enum BasicType {
    /// A machine sized integer (usize) with the specified flag indicating if it's signed
    MachineInteger(bool),
    /// An integer with the specified number of bits,
    /// with negative values denoting signed integers and positive indicating signed.
    Integer(i32),
    NamedType(String),
    Option(Box<BasicType>)
}
lazy_static! {
    static ref INTEGER_PATTERN: Regex = Regex::new("([iu])(8|16|32|64|128)").unwrap();
}
impl BasicType {
    #[allow(dead_code)]
    fn name(&self) -> String {
        match *self {
            BasicType::MachineInteger(true) => "isize".to_owned(),
            BasicType::MachineInteger(false) => "usize".to_owned(),
            BasicType::Integer(0) => unreachable!("Invalid type: {:?}", self),
            BasicType::Integer(width) => {
                if width < 0 {
                    format!("i{}", -width)
                } else {
                    format!("u{}", width)
                }
            },
            BasicType::NamedType(ref name) => name.to_owned(),
            BasicType::Option(ref inner) => format!("Option<{}>", inner.name()),
        }
    }
    fn determine(target: &Type) -> Option<BasicType> {
        if let Type::Path(TypePath { qself: None, ref path }) = *target {
            if path.segments.len() == 1 {
                let segment = &path.segments[0];
                let name = segment.ident.as_ref();
                match name {
                    "Option" => {
                        if let PathArguments::AngleBracketed(AngleBracketedGenericArguments { ref args, .. }) = segment.arguments {
                            if let GenericArgument::Type(ref param_type) = args[0] {
                                if let Some(known) = BasicType::determine(param_type) {
                                    return Some(BasicType::Option(box known))
                                }
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
                        } else {
                            return Some(BasicType::NamedType(name.to_owned()))
                        }
                    }
                }
            }

        }
        None
    }
}

fn find_field<'a, I>(targets: I, name: &str) -> Option<&'a Field> where I: IntoIterator<Item=&'a Field> {
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
                let code = match rustfmt(&tokens.to_string()) {
                    Ok(Some(formatted)) => formatted,
                    Ok(None) => {
                        println!("{}// NOTE: Failed to rustfmt", indent);
                        tokens.to_string()
                    },
                    Err(e) => panic!("Failed to rustfmt: {}", tokens.to_string())
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
