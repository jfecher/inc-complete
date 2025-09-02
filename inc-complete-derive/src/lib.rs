//! Derive macro for automatically implementing Storage trait
//! This generates a call to the impl_storage! macro based on the struct fields.

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{
    Attribute, Data, DeriveInput, Expr, ExprLit, Fields, GenericArgument, Lit, Meta, PathArguments,
    Type, TypePath, parse_macro_input, spanned::Spanned,
};

// =============================================================================
// Helper Functions for Attribute Parsing
// =============================================================================

/// Parse a type attribute from a Meta::NameValue
fn parse_type_attribute(
    name_value: &syn::MetaNameValue,
    expected_name: &str,
) -> syn::Result<proc_macro2::TokenStream> {
    let name = name_value
        .path
        .get_ident()
        .ok_or_else(|| syn::Error::new_spanned(&name_value.path, "expected identifier"))?
        .to_string();

    if name != expected_name {
        return Err(syn::Error::new_spanned(
            &name_value.path,
            format!("expected '{}', found '{}'", expected_name, name),
        ));
    }

    let expr = &name_value.value;
    Ok(quote::quote! { #expr })
}

/// Parse an integer attribute from a Meta::NameValue
fn parse_int_attribute(
    name_value: &syn::MetaNameValue,
    expected_name: &str,
) -> syn::Result<proc_macro2::TokenStream> {
    let name = name_value
        .path
        .get_ident()
        .ok_or_else(|| syn::Error::new_spanned(&name_value.path, "expected identifier"))?
        .to_string();

    if name != expected_name {
        return Err(syn::Error::new_spanned(
            &name_value.path,
            format!("expected '{}', found '{}'", expected_name, name),
        ));
    }

    if let Expr::Lit(ExprLit {
        lit: Lit::Int(lit_int),
        ..
    }) = &name_value.value
    {
        Ok(quote! { #lit_int })
    } else {
        Err(syn::Error::new_spanned(
            &name_value.value,
            format!("{} must be an integer", expected_name),
        ))
    }
}

/// Parse a boolean flag attribute from a Meta::Path
fn parse_flag_attribute(path: &syn::Path, expected_name: &str) -> syn::Result<bool> {
    if path.is_ident(expected_name) {
        Ok(true)
    } else {
        Err(syn::Error::new_spanned(
            path,
            format!("unknown flag attribute, expected '{}'", expected_name),
        ))
    }
}

/// Extract generic type T from Container<T>
fn extract_generic_type(ty: &Type, container_suffix: &str) -> Option<Type> {
    if let Type::Path(TypePath { path, .. }) = ty {
        if let Some(segment) = path.segments.last() {
            if segment.ident.to_string().ends_with(container_suffix) {
                if let PathArguments::AngleBracketed(args) = &segment.arguments {
                    if let Some(GenericArgument::Type(inner_type)) = args.args.first() {
                        return Some(inner_type.clone());
                    }
                }
            }
        }
    }
    None
}

/// Parser specifically for Input derive attributes
struct InputAttributeParser {
    id: Option<proc_macro2::TokenStream>,
    output_type: Option<proc_macro2::TokenStream>,
    storage_type: Option<proc_macro2::TokenStream>,
    assume_changed: bool,
}

impl InputAttributeParser {
    fn new() -> Self {
        Self {
            id: None,
            output_type: None,
            storage_type: None,
            assume_changed: false,
        }
    }

    fn parse_attribute_list(&mut self, attr: &Attribute) -> syn::Result<()> {
        let meta = attr.meta.clone();
        if let Meta::List(meta_list) = meta {
            let parsed: syn::punctuated::Punctuated<Meta, syn::Token![,]> =
                meta_list.parse_args_with(syn::punctuated::Punctuated::parse_terminated)?;

            for meta in parsed {
                match meta {
                    Meta::Path(path) => {
                        if parse_flag_attribute(&path, "assume_changed")? {
                            self.assume_changed = true;
                        }
                    }
                    Meta::NameValue(name_value) => {
                        let name = name_value
                            .path
                            .get_ident()
                            .ok_or_else(|| {
                                syn::Error::new_spanned(&name_value.path, "expected identifier")
                            })?
                            .to_string();

                        match name.as_str() {
                            "id" => {
                                self.id = Some(parse_int_attribute(&name_value, "id")?);
                            }
                            "output" => {
                                self.output_type =
                                    Some(parse_type_attribute(&name_value, "output")?);
                            }
                            "storage" => {
                                self.storage_type =
                                    Some(parse_type_attribute(&name_value, "storage")?);
                            }
                            _ => {
                                return Err(syn::Error::new_spanned(
                                    &name_value.path,
                                    format!(
                                        "unknown attribute '{}' for Input derive. Valid attributes are: id, output, storage, assume_changed",
                                        name
                                    ),
                                ));
                            }
                        }
                    }
                    _ => {
                        return Err(syn::Error::new_spanned(
                            &meta,
                            "unsupported attribute format",
                        ));
                    }
                }
            }
        }
        Ok(())
    }

    fn validate(
        &self,
        attr_span: Span,
    ) -> syn::Result<(
        proc_macro2::TokenStream,
        proc_macro2::TokenStream,
        proc_macro2::TokenStream,
        bool,
    )> {
        let id = self
            .id
            .clone()
            .ok_or_else(|| syn::Error::new(attr_span, "missing required 'id' attribute"))?;
        let output_type = self
            .output_type
            .clone()
            .ok_or_else(|| syn::Error::new(attr_span, "missing required 'output' attribute"))?;
        let storage_type = self
            .storage_type
            .clone()
            .ok_or_else(|| syn::Error::new(attr_span, "missing required 'storage' attribute"))?;

        Ok((id, output_type, storage_type, self.assume_changed))
    }
}

/// Parser specifically for Intermediate macro attributes
struct IntermediateAttributeParser {
    id: Option<proc_macro2::TokenStream>,
    assume_changed: bool,
}

impl IntermediateAttributeParser {
    fn new() -> Self {
        Self {
            id: None,
            assume_changed: false,
        }
    }

    fn parse_meta(&mut self, meta: &Meta) -> syn::Result<()> {
        match meta {
            Meta::Path(path) => {
                if parse_flag_attribute(path, "assume_changed")? {
                    self.assume_changed = true;
                }
            }
            Meta::NameValue(name_value) => {
                let name = name_value
                    .path
                    .get_ident()
                    .ok_or_else(|| {
                        syn::Error::new_spanned(&name_value.path, "expected identifier")
                    })?
                    .to_string();

                match name.as_str() {
                    "id" => {
                        self.id = Some(parse_int_attribute(name_value, "id")?);
                    }
                    _ => {
                        return Err(syn::Error::new_spanned(
                            &name_value.path,
                            format!(
                                "unknown attribute '{}' for intermediate macro. Valid attributes are: id, assume_changed",
                                name
                            ),
                        ));
                    }
                }
            }
            _ => {
                return Err(syn::Error::new_spanned(
                    meta,
                    "unsupported attribute format",
                ));
            }
        }
        Ok(())
    }

    fn validate(&self, attr_span: Span) -> syn::Result<(proc_macro2::TokenStream, bool)> {
        let id = self
            .id
            .clone()
            .ok_or_else(|| syn::Error::new(attr_span, "missing required 'id' attribute"))?;

        Ok((id, self.assume_changed))
    }
}

// =============================================================================
// Storage Derive Macro
// =============================================================================

/// Derive macro for Storage trait
///
/// Usage:
/// ```rust
/// #[derive(Storage)]
/// struct MyStorage {
///     numbers: SingletonStorage<Number>,
///     strings: HashMapStorage<StringComputation>,
/// }
/// ```
///
/// This generates a call to `impl_storage!(MyStorage, numbers: Number, strings: StringComputation)`
/// which provides the actual implementation.
///
/// ## Skip Attribute
///
/// Fields can be excluded from the Storage implementation using `#[inc_complete(skip)]`:
///
/// ```rust
/// #[derive(Storage)]
/// struct MyStorage {
///     numbers: SingletonStorage<Number>,
///
///     #[inc_complete(skip)]
///     metadata: String,  // This field won't be included in Storage implementation
/// }
///
/// ## Computation Attribute
///
/// The computation type for a field can be manually specified using `#[inc_complete(computation = Type)]`.
/// This is required if the field type is not already generic over the computation type. For
/// example, `HashMapStorage<MyComputationType>` is generic over `MyComputationType` but
/// `MyStringStorage` is not:
///
/// ```rust
/// #[derive(Storage)]
/// struct MyStorage {
///     numbers: SingletonStorage<Number>,
///
///     #[inc_complete(computation = MyStringInput)]
///     strings: MyStringStorage,
/// }
/// ```
#[proc_macro_derive(Storage, attributes(inc_complete))]
pub fn derive_storage(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let struct_name = &input.ident;

    // Extract field information
    let fields = match &input.data {
        Data::Struct(data) => match &data.fields {
            Fields::Named(fields) => &fields.named,
            _ => {
                return syn::Error::new(
                    Span::call_site(),
                    "Storage derive only works on structs with named fields",
                )
                .to_compile_error()
                .into();
            }
        },
        _ => {
            return syn::Error::new(Span::call_site(), "Storage derive only works on structs")
                .to_compile_error()
                .into();
        }
    };

    // Extract computation types from storage fields
    let mut field_mappings = Vec::new();
    let mut accumulated = Vec::new();

    for field in fields {
        let field_name = field.ident.as_ref().unwrap();
        let field_type = &field.ty;

        // Parse attributes
        let mut skip_field = false;
        let mut is_accumulated = false;
        let mut manual_computation_type: Option<Type> = None;

        for attr in &field.attrs {
            if attr.path().is_ident("inc_complete") {
                match attr.meta {
                    Meta::List(ref list) => {
                        // Parse nested attributes like #[inc_complete(skip)] or #[inc_complete(computation = Type)]
                        let nested_result =
                            list.parse_args_with(|parser: syn::parse::ParseStream| {
                                while !parser.is_empty() {
                                    let lookahead = parser.lookahead1();
                                    if lookahead.peek(syn::Ident) {
                                        let ident: syn::Ident = parser.parse()?;
                                        if ident == "skip" {
                                            skip_field = true;
                                        } else if ident == "computation" {
                                            parser.parse::<syn::Token![=]>()?;
                                            manual_computation_type = Some(parser.parse()?);
                                        } else if ident == "accumulate" {
                                            is_accumulated = true;
                                        } else {
                                            return Err(syn::Error::new_spanned(
                                                ident,
                                                "expected 'skip' or 'computation'",
                                            ));
                                        }
                                    } else {
                                        return Err(lookahead.error());
                                    }

                                    if !parser.is_empty() {
                                        parser.parse::<syn::Token![,]>()?;
                                    }
                                }

                                Ok(())
                            });

                        if let Err(e) = nested_result {
                            return e.to_compile_error().into();
                        }
                    }
                    _ => {
                        return syn::Error::new_spanned(
                            attr,
                            "expected #[inc_complete(skip)] or #[inc_complete(computation = Type)]",
                        )
                        .to_compile_error()
                        .into();
                    }
                }
            }
        }

        if skip_field {
            // Skip this field - don't include it in the impl_storage! call
            continue;
        }

        // Determine the computation type
        let computation_type = if let Some(manual_type) = manual_computation_type {
            // Use manually specified type
            manual_type
        } else if let Some(extracted_type) = extract_generic_type(field_type, "Storage") {
            // Try to extract the generic type from SingletonStorage<T>, HashMapStorage<T>, etc.
            extracted_type
        } else {
            return syn::Error::new(
                field.span(),
                "Field must be a storage type like SingletonStorage<T>, HashMapStorage<T>, or use #[inc_complete(computation = Type)] to specify the type manually, or use #[inc_complete(skip)] to exclude it",
            )
            .to_compile_error()
            .into();
        };

        let item = quote! { #field_name: #computation_type, };
        if is_accumulated {
            accumulated.push(item);
        } else {
            field_mappings.push(item);
        }
    }

    // Generate a call to impl_storage! macro
    let expanded = quote! {
        inc_complete::impl_storage!(#struct_name, #(#field_mappings)* @accumulators { #(#accumulated)* });
    };

    TokenStream::from(expanded)
}

// =============================================================================
// Input Derive Macro
// =============================================================================

/// Derive macro for Input computation
///
/// Usage:
/// ```rust
/// #[derive(Input)]
/// #[inc_complete(id = 0, output = i32, storage = MyStorage)]
/// struct MyInput;
/// ```
///
/// This generates a call to `define_input!(0, MyInput -> i32, MyStorage)`
/// which provides the actual implementation.
///
/// Required attributes:
/// - `id`: The unique computation ID (integer)
/// - `output`: The output type
/// - `storage`: The storage type
///
/// Optional attribute:
/// - `assume_changed`: If present, indicates that the input is assumed to have changed
#[proc_macro_derive(Input, attributes(inc_complete))]
pub fn derive_input(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let struct_name = &input.ident;

    // Find the inc_complete attribute
    let inc_complete_attr = input
        .attrs
        .iter()
        .find(|attr| attr.path().is_ident("inc_complete"));

    let attr = match inc_complete_attr {
        Some(attr) => attr,
        None => {
            return syn::Error::new(
                Span::call_site(),
                "Input derive requires #[inc_complete(...)] attribute",
            )
            .to_compile_error()
            .into();
        }
    };

    // Parse the attribute arguments using the Input-specific parser
    let mut parser = InputAttributeParser::new();
    if let Err(err) = parser.parse_attribute_list(attr) {
        return err.to_compile_error().into();
    }

    let (id, output_type, storage_type, assume_changed) = match parser.validate(attr.span()) {
        Ok(values) => values,
        Err(err) => return err.to_compile_error().into(),
    };

    // Generate the define_input! call
    let expanded = if assume_changed {
        quote! {
            inc_complete::define_input!(#id, assume_changed #struct_name -> #output_type, #storage_type);
        }
    } else {
        quote! {
            inc_complete::define_input!(#id, #struct_name -> #output_type, #storage_type);
        }
    };

    TokenStream::from(expanded)
}

// =============================================================================
// Intermediate Macros
// =============================================================================

/// Procedural macro for defining intermediate computations directly on functions
///
/// Usage:
/// ```rust
/// // Assumes ComputeDouble struct is already defined
/// #[intermediate(id = 1)]
/// fn compute_double(_context: &ComputeDouble, db: &DbHandle<MyStorage>) -> i32 {
///     db.get(InputValue) * 2
/// }
/// ```
///
/// This generates a call to `define_intermediate!` using the extracted information.
/// Assumes the computation struct is already defined.
///
/// Required attributes:
/// - `id`: The computation ID (integer)
///
/// Optional attribute:
/// - `assume_changed`: If present, indicates that the input is assumed to have changed
///
/// The macro automatically extracts:
/// - **Output type** from the function return type (`i32`)
/// - **Computation type** from the first parameter (`ComputeDouble` from `&ComputeDouble`)
/// - **Storage type** from the second parameter (`MyStorage` from `&DbHandle<MyStorage>`)
/// - **Function name** automatically (`compute_double`)
///
#[proc_macro_attribute]
pub fn intermediate(args: TokenStream, input: TokenStream) -> TokenStream {
    let args = parse_macro_input!(args with syn::punctuated::Punctuated::<syn::Meta, syn::Token![,]>::parse_terminated);
    let input_fn = parse_macro_input!(input as syn::ItemFn);

    match process_intermediate_function(args, input_fn) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

// =============================================================================
// Helper Functions for Intermediate Macros
// =============================================================================

/// Process the intermediate function and generate the appropriate code
fn process_intermediate_function(
    args: syn::punctuated::Punctuated<syn::Meta, syn::Token![,]>,
    input_fn: syn::ItemFn,
) -> syn::Result<proc_macro2::TokenStream> {
    // Parse the attributes using the Intermediate-specific parser
    let mut parser = IntermediateAttributeParser::new();
    for arg in args {
        parser.parse_meta(&arg)?;
    }
    let (id, assume_changed) = parser.validate(Span::call_site())?;

    // Extract information from the function signature
    let fn_name = &input_fn.sig.ident;
    let output_type = extract_return_type(&input_fn.sig.output)?;
    let (computation_type, storage_type) = extract_types_from_params(&input_fn.sig.inputs)?;

    // Generate the define_intermediate call, and optionally the struct
    let expanded = if assume_changed {
        quote! {
            #input_fn

            inc_complete::define_intermediate!(#id, assume_changed #computation_type -> #output_type, #storage_type, #fn_name);
        }
    } else {
        quote! {
            #input_fn

            inc_complete::define_intermediate!(#id, #computation_type -> #output_type, #storage_type, #fn_name);
        }
    };

    Ok(expanded)
}

/// Extract the return type from a function signature
fn extract_return_type(output: &syn::ReturnType) -> syn::Result<proc_macro2::TokenStream> {
    match output {
        syn::ReturnType::Type(_, ty) => Ok(quote! { #ty }),
        syn::ReturnType::Default => Err(syn::Error::new(
            Span::call_site(),
            "function must have an explicit return type",
        )),
    }
}

/// Extract both computation type and storage type from function parameters
fn extract_types_from_params(
    inputs: &syn::punctuated::Punctuated<syn::FnArg, syn::Token![,]>,
) -> syn::Result<(proc_macro2::TokenStream, proc_macro2::TokenStream)> {
    let mut iter = inputs.iter();

    // Extract computation type from first parameter
    let first_arg = iter.next().ok_or_else(|| {
        syn::Error::new(
            Span::call_site(),
            "function must have at least two parameters: (&ComputationType, &DbHandle<StorageType>)",
        )
    })?;

    let computation_type = extract_reference_inner_type(
        first_arg,
        "first parameter must be a reference to the computation type (e.g., &ComputationType)",
    )?;

    // Extract storage type from second parameter
    let second_arg = iter.next().ok_or_else(|| {
        syn::Error::new(
            Span::call_site(),
            "function must have a second parameter of type &DbHandle<StorageType>",
        )
    })?;

    let storage_type = extract_dbhandle_storage_type(second_arg)?;

    Ok((computation_type, storage_type))
}

/// Extract inner type from a reference parameter
fn extract_reference_inner_type(
    arg: &syn::FnArg,
    error_msg: &str,
) -> syn::Result<proc_macro2::TokenStream> {
    if let syn::FnArg::Typed(pat_type) = arg {
        if let syn::Type::Reference(type_ref) = pat_type.ty.as_ref() {
            let inner_type = &type_ref.elem;
            Ok(quote! { #inner_type })
        } else {
            Err(syn::Error::new_spanned(arg, error_msg))
        }
    } else {
        Err(syn::Error::new_spanned(arg, error_msg))
    }
}

/// Extract storage type from &DbHandle<StorageType> parameter
fn extract_dbhandle_storage_type(arg: &syn::FnArg) -> syn::Result<proc_macro2::TokenStream> {
    let error_msg = "second parameter must be &DbHandle<StorageType>";

    if let syn::FnArg::Typed(pat_type) = arg {
        if let syn::Type::Reference(type_ref) = pat_type.ty.as_ref() {
            if let syn::Type::Path(type_path) = type_ref.elem.as_ref() {
                // Look for DbHandle<StorageType>
                if let Some(segment) = type_path.path.segments.last() {
                    if segment.ident == "DbHandle" {
                        if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                            if let Some(syn::GenericArgument::Type(storage_ty)) = args.args.first()
                            {
                                return Ok(quote! { #storage_ty });
                            }
                        }
                        return Err(syn::Error::new_spanned(
                            arg,
                            "DbHandle must have a generic type parameter for the storage type",
                        ));
                    }
                }
            }
        }
    }

    Err(syn::Error::new_spanned(arg, error_msg))
}
