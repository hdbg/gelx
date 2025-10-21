#![cfg_attr(docsrs, feature(doc_cfg))]
#![doc(html_logo_url = "https://raw.githubusercontent.com/ifiokjr/gelx/main/setup/assets/logo.png")]
#![doc = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/readme.md"))]

mod codegen;
mod constants;
mod errors;
mod metadata;
mod utils;

use std::collections::HashMap;
use std::sync::Arc;

use check_keyword::CheckKeyword;
use gel_protocol::common::Capabilities;
use gel_protocol::common::Cardinality;
use gel_protocol::common::CompilationOptions;
use gel_protocol::common::InputLanguage;
use gel_protocol::common::IoFormat;
use gel_protocol::descriptors::Descriptor;
use gel_protocol::descriptors::EnumerationTypeDescriptor;
use gel_protocol::descriptors::InputShapeElement;
use gel_protocol::descriptors::ShapeElement;
use gel_protocol::descriptors::TupleElement;
use gel_protocol::descriptors::TypePos;
use gel_protocol::descriptors::Typedesc;
use gel_protocol::server_message::CommandDataDescription1;
use gel_tokio::raw::Pool;
use gel_tokio::raw::PoolState;
use heck::ToPascalCase;
use heck::ToSnakeCase;
use proc_macro2::TokenStream;
use quote::format_ident;
use quote::quote;
use syn::Ident;
use syn::Token;
use syn::punctuated::Punctuated;
use tokio::runtime::Runtime;
use typed_builder::TypedBuilder;

pub use crate::codegen::*;
pub use crate::constants::*;
pub use crate::errors::*;
pub use crate::metadata::*;
pub use crate::utils::*;

/// Get the query descriptor asynchronously.
pub async fn get_descriptor(
	query: &str,
	metadata: &GelxMetadata,
) -> GelxCoreResult<CommandDataDescription1> {
	let config = metadata.gel_config()?;
	let state = Arc::new(PoolState::default());
	let pool = Pool::new(&config);
	let mut pool_connection = Box::pin(pool.acquire()).await?;
	let connection = pool_connection.inner();
	let flags = CompilationOptions {
		implicit_limit: None,
		implicit_typenames: false,
		implicit_typeids: false,
		explicit_objectids: true,
		allow_capabilities: Capabilities::ALL,
		io_format: IoFormat::Binary,
		expected_cardinality: Cardinality::Many,
		input_language: InputLanguage::EdgeQL,
	};

	let result = connection
		.parse(&flags, query, &state, &Arc::new(HashMap::default()))
		.await;

	Ok(result?)
}

/// Get the descriptor synchronously.
pub fn get_descriptor_sync(
	query: &str,
	metadata: &GelxMetadata,
) -> GelxCoreResult<CommandDataDescription1> {
	let rt = Runtime::new()?;
	let descriptor = rt.block_on(async { get_descriptor(query, metadata).await })?;

	Ok(descriptor)
}

pub fn generate_query_token_stream(
	descriptor: &CommandDataDescription1,
	name: &str,
	query: &str,
	metadata: &GelxMetadata,
	is_macro: bool,
) -> GelxCoreResult<TokenStream> {
	let input_ident = metadata.input_struct_ident();
	let output_ident = metadata.output_struct_ident();
	let props_ident = format_ident!("{PROPS_NAME}");

	let query_ident = metadata.query_function_ident();
	let query_prop_ident = format_ident!("{QUERY_PROP_NAME}");
	let module_name: Ident = format_ident!("{}", name.to_snake_case());
	let input = descriptor.input.decode()?;
	let output = descriptor.output.decode()?;
	let mut tokens: TokenStream = TokenStream::new();

	explore_descriptor(
		ExploreDescriptorProps::builder()
			.typedesc(&input)
			.is_input()
			.is_root()
			.descriptor(input.root())
			.root_name(&metadata.input_struct_name)
			.metadata(metadata)
			.is_macro_bool(is_macro)
			.build(),
		&mut tokens,
	)?;
	explore_descriptor(
		ExploreDescriptorProps::builder()
			.typedesc(&output)
			.is_root()
			.descriptor(output.root())
			.root_name(&metadata.output_struct_name)
			.metadata(metadata)
			.is_macro_bool(is_macro)
			.build(),
		&mut tokens,
	)?;

	let query_method = match descriptor.result_cardinality {
		Cardinality::NoResult => quote!(execute),
		Cardinality::AtMostOne => quote!(query_single),
		Cardinality::One => quote!(query_required_single),
		Cardinality::Many | Cardinality::AtLeastOne => quote!(query),
	};
	let exports_ident = metadata.exports_alias_ident();
	let query_constant = metadata.query_constant_ident();
	let mut query_props =
		vec![quote!(#query_prop_ident: impl #exports_ident::gel_tokio::QueryExecutor)];
	let args = vec![
		quote!(#query_constant),
		input.root().map_or(quote!(&()), |_| quote!(#props_ident)),
	];
	let inner_return = output.root().map_or(quote!(()), |_| quote!(#output_ident));
	let returns = wrap_token_with_cardinality(Some(descriptor.result_cardinality), inner_return);

	if input.root().is_some() {
		query_props.push(quote!(#props_ident: &#input_ident));
	}

	let query_annotation = metadata.features.annotate(FeatureName::Query, is_macro);

	let token_stream = quote! {
		pub mod #module_name {
			use ::gelx::exports as #exports_ident;

			/// Execute the desired query.
			#query_annotation
			pub async fn #query_ident(#(#query_props),*) -> ::core::result::Result<#returns, #exports_ident::gel_errors::Error> {
				#query_prop_ident.#query_method(#(#args),*).await
			}

			#tokens

			/// The original query string provided to the macro. Can be reused in your codebase.
			pub const #query_constant: &str = #query;
		}
	};

	Ok(token_stream)
}

fn wrap_token_with_cardinality(
	cardinality: Option<Cardinality>,
	token: TokenStream,
) -> TokenStream {
	let Some(cardinality) = cardinality else {
		return token;
	};

	match cardinality {
		Cardinality::NoResult => quote!(()),
		Cardinality::AtMostOne => quote!(Option<#token>),
		Cardinality::One => token,
		Cardinality::Many | Cardinality::AtLeastOne => quote!(Vec<#token>),
	}
}

#[derive(Debug, TypedBuilder)]
struct ExploreDescriptorProps<'a> {
	metadata: &'a GelxMetadata,
	typedesc: &'a Typedesc,
	#[builder(setter(strip_bool(fallback = is_macro_bool)))]
	is_macro: bool,
	#[builder(setter(strip_bool(fallback = is_input_bool)))]
	is_input: bool,
	#[builder(setter(strip_bool(fallback = is_root_bool)))]
	is_root: bool,
	descriptor: Option<&'a Descriptor>,
	root_name: &'a str,
}

type PartialExploreDescriptorProps<'a> = ExploreDescriptorPropsBuilder<
	'a,
	(
		(&'a GelxMetadata,),
		(&'a Typedesc,),
		(bool,),
		(bool,),
		(bool,),
		(),
		(),
	),
>;

impl<'a> ExploreDescriptorProps<'a> {
	fn into_props(self) -> PartialExploreDescriptorProps<'a> {
		let Self {
			typedesc,
			is_input,
			is_macro,
			metadata,
			..
		} = self;

		Self::builder()
			.typedesc(typedesc)
			.is_input_bool(is_input)
			.is_macro_bool(is_macro)
			.is_root_bool(false)
			.metadata(metadata)
	}
}

#[derive(Default)]
pub struct ExploreResult {
	pub token: Option<TokenStream>,
	pub conversion: ConvertionKind,
}
impl ExploreResult {
	pub fn map<F>(self, f: F) -> Self
	where
		F: FnOnce(TokenStream) -> TokenStream,
	{
		Self {
			token: self.token.map(f),
			..self
		}
	}
}
impl From<MappedScalar> for ExploreResult {
	fn from(value: MappedScalar) -> Self {
		Self {
			token: Some(value.token),
			conversion: value.convertion_kind,
		}
	}
}
impl From<TokenStream> for ExploreResult {
	fn from(value: TokenStream) -> Self {
		Self {
			token: Some(value),
			conversion: ConvertionKind::Direct,
		}
	}
}
impl From<Option<TokenStream>> for ExploreResult {
	fn from(value: Option<TokenStream>) -> Self {
		Self {
			token: value,
			conversion: ConvertionKind::Direct,
		}
	}
}

fn explore_descriptor(
	props @ ExploreDescriptorProps {
		typedesc,
		is_input,
		is_root,
		descriptor,
		root_name,
		metadata,
		is_macro,
	}: ExploreDescriptorProps,
	tokens: &mut TokenStream,
) -> GelxCoreResult<ExploreResult> {
	let root_ident = format_ident!("{root_name}");
	let exports_ident = metadata.exports_alias_ident();
	let Some(descriptor) = descriptor else {
		if is_root {
			tokens.extend(quote!(pub type #root_ident = ();));
		}

		return Ok(ExploreResult::default());
	};

	match descriptor {
		Descriptor::Set(set) => {
			let set_descriptor = typedesc.get(set.type_pos).ok();
			let sub_root_name = format!("{root_name}Set");
			let props = props
				.into_props()
				.descriptor(set_descriptor)
				.root_name(&sub_root_name)
				.build();
			let result =
				explore_descriptor(props, tokens)?.map(|result: TokenStream| quote!(Vec<#result>));

			if is_root {
				let result_tokens = result.token;
				tokens.extend(quote!(pub type #root_ident = #result_tokens;));
				Ok(ExploreResult {
					token: Some(quote!(#root_ident)),
					conversion: result.conversion,
				})
			} else {
				Ok(result)
			}
		}

		Descriptor::ObjectShape(object) => {
			let result = explore_object_shape_descriptor(
				StructElement::from_shape(&object.elements),
				typedesc,
				root_name,
				is_input,
				metadata,
				is_macro,
				tokens,
			)?;

			Ok(ExploreResult {
				token: result,
				..Default::default()
			})
		}

		Descriptor::BaseScalar(base_scalar) => {
			let mut result = uuid_to_token_name(&base_scalar.id, &exports_ident).into();

			let ExploreResult { token, .. } = &mut result;

			match is_root {
				true => {
					tokens.extend(quote!(pub type #root_ident = #token;));
					*token = Some(quote!(#root_ident));
					Ok(result)
				}
				false => Ok(result),
			}
		}

		Descriptor::Scalar(scalar) => {
			let Some(module_name) = &scalar.name.as_ref().map(ModuleName::from) else {
				return Ok(ExploreResult::default()); // should not happen
			};

			if module_name.is_system_namespace() {
				let mut result = uuid_to_token_name(&scalar.id, &exports_ident).into();

				let ExploreResult { token, .. } = &mut result;

				match is_root {
					true => {
						tokens.extend(quote!(pub type #root_ident = #token;));
						*token = Some(quote!(#root_ident));
						Ok(result)
					}
					false => Ok(result),
				}
			} else if is_macro {
				let Some(base_type_pos) = scalar.base_type_pos else {
					return Ok(ExploreResult::default()); // should not happen
				};

				let props = props
					.into_props()
					.descriptor(typedesc.get(base_type_pos).ok())
					.root_name(root_name)
					.build();

				explore_descriptor(props, tokens)
			} else {
				let module_ident = module_name.modules_path()?;
				let enum_ident = module_name.name_ident(false);

				Ok(ExploreResult {
					token: Some(quote!(super::#module_ident::#enum_ident)),
					..Default::default()
				})
			}
		}

		Descriptor::Tuple(tuple) => {
			let mut tuple_tokens = Punctuated::<_, Token![,]>::new();

			for (index, element) in tuple.element_types.iter().enumerate() {
				let sub_root_name = format!("{root_name}{index}");
				let result = explore_descriptor(
					ExploreDescriptorProps::builder()
						.typedesc(typedesc)
						.is_input_bool(is_input)
						.descriptor(typedesc.get(*element).ok())
						.root_name(&sub_root_name)
						.metadata(metadata)
						.build(),
					tokens,
				)?;

				tuple_tokens.push(result.token);
			}

			let mut result = quote!((#tuple_tokens )).into();

			let ExploreResult { token, .. } = &mut result;

			match is_root {
				true => {
					tokens.extend(quote!(pub type #root_ident = #token;));
					*token = Some(quote!(#root_ident));
					Ok(result)
				}
				false => Ok(result),
			}
		}

		Descriptor::NamedTuple(named_tuple) => {
			let result = explore_object_shape_descriptor(
				StructElement::from_named_tuple(&named_tuple.elements),
				typedesc,
				root_name,
				is_input,
				metadata,
				is_macro,
				tokens,
			)?;

			Ok(result.into())
		}

		Descriptor::Array(array) => {
			let array_descriptor = typedesc.get(array.type_pos).ok();
			let sub_root_name = format!("{root_name}Array");
			let props = props
				.into_props()
				.descriptor(array_descriptor)
				.root_name(&sub_root_name)
				.build();

			let mut result = explore_descriptor(props, tokens)?.map(|result| quote!(Vec<#result>));

			let ExploreResult { token, .. } = &mut result;

			match is_root {
				true => {
					tokens.extend(quote!(pub type #root_ident = #token;));
					*token = Some(quote!(#root_ident));
					Ok(result)
				}
				false => Ok(result),
			}
		}

		Descriptor::Enumeration(enumeration) => {
			// TODO: support ephemeral enums not defined in the schema
			let mut result = if is_macro {
				// Inline the enum in the macro output.
				explore_enumeration_descriptor(enumeration, metadata, tokens, is_macro)
			} else {
				// Otherwise reference the enum from the generated module which this is a part
				// of.
				let Some(name) = &enumeration.name else {
					return Ok(quote!(String).into());
				};

				let module_name: ModuleName = name.into();
				let module_ident = module_name.modules_path()?;
				let enum_ident = module_name.name_ident(false);

				quote!(super::#module_ident::#enum_ident)
			}
			.into();

			let ExploreResult { token, .. } = &mut result;

			match is_root {
				true => {
					tokens.extend(quote!(pub type #root_ident = #token;));
					*token = Some(quote!(#root_ident));
					Ok(result)
				}
				false => Ok(result),
			}
		}

		Descriptor::InputShape(object) => {
			let result = explore_object_shape_descriptor(
				StructElement::from_input_shape(&object.elements),
				typedesc,
				root_name,
				is_input,
				metadata,
				is_macro,
				tokens,
			)?;

			Ok(result.into())
		}

		Descriptor::Range(range) => {
			let range_descriptor = typedesc.get(range.type_pos).ok();
			let sub_root_name = format!("{root_name}Range");
			let props = props
				.into_props()
				.descriptor(range_descriptor)
				.root_name(&sub_root_name)
				.build();

			let mut result = explore_descriptor(props, tokens)?
				.map(|result| quote!(#exports_ident::gel_protocol::model::Range<#result>));

			let ExploreResult { token, .. } = &mut result;

			match is_root {
				true => {
					tokens.extend(quote!(pub type #root_ident = #token;));
					*token = Some(quote!(#root_ident));
					Ok(result)
				}
				false => Ok(result),
			}
		}

		Descriptor::MultiRange(_) => todo!("`multirange` not in the `gel_protocol` crate"),
		Descriptor::TypeAnnotation(_) => todo!("type annotations are not supported"),
		Descriptor::Object(_object_type_descriptor) => todo!("implement `Object`"),
		Descriptor::Compound(_compound_type_descriptor) => todo!("implement `Compound`"),
		Descriptor::SQLRow(_sqlrow_descriptor) => todo!("implement `SQLRow`"),
	}
}

/// Explore the enumeration descriptor and return the root type. Defaults to
/// `String` if the enumeration is valid.
fn explore_enumeration_descriptor(
	enumeration: &EnumerationTypeDescriptor,
	metadata: &GelxMetadata,
	tokens: &mut TokenStream,
	is_macro: bool,
) -> TokenStream {
	let Some(name) = &enumeration.name else {
		return quote!(String);
	};

	let name = name.to_pascal_case().into_safe();
	let root_ident = format_ident!("{name}");

	let enum_tokens = generate_enum(metadata, &enumeration.members, &name, is_macro);

	tokens.extend(enum_tokens);
	quote!(#root_ident)
}

fn explore_object_shape_descriptor(
	elements: Vec<StructElement<'_>>,
	typedesc: &Typedesc,
	root_name: &str,
	is_input: bool,
	metadata: &GelxMetadata,
	is_macro: bool,
	tokens: &mut TokenStream,
) -> GelxCoreResult<Option<TokenStream>> {
	let mut impl_named_args = vec![];
	let mut struct_fields = vec![];
	let root_ident = format_ident!("{root_name}");
	let exports_ident = metadata.exports_alias_ident();
	for element in elements {
		let descriptor = typedesc.get(element.type_pos()).ok();
		let name = &element.name();
		let safe_name = name.to_snake_case().into_safe();
		let safe_name_ident = format_ident!("{safe_name}");
		let pascal_name = name.to_pascal_case();
		let sub_root_name = format!("{root_name}{pascal_name}").into_safe();
		let sub_props = ExploreDescriptorProps::builder()
			.typedesc(typedesc)
			.is_input_bool(is_input)
			.descriptor(descriptor)
			.root_name(&sub_root_name)
			.metadata(metadata)
			.is_macro_bool(is_macro)
			.build();
		let output = explore_descriptor(sub_props, tokens)?;
		let output_token = element.wrap(&output.token);
		let serde_annotation = (&safe_name != name).then_some(metadata.features.wrap_annotation(
			FeatureName::Serde,
			&quote!(serde(rename = #name)),
			is_macro,
		));
		let query_annotation = (&safe_name != name).then_some(metadata.features.wrap_annotation(
			FeatureName::Query,
			&quote!(gel(rename = #name)),
			is_macro,
		));

		let builder_fields = {
			match element.cardinality() {
				Cardinality::AtMostOne => {
					let fallback_ident = format_ident!("{safe_name_ident}_opt");
					Some(quote!(default, setter(into, strip_option(fallback = #fallback_ident))))
				}
				Cardinality::One => Some(quote!(setter(into))),
				Cardinality::Many => Some(quote!(default)),
				Cardinality::NoResult | Cardinality::AtLeastOne => None,
			}
		};
		let builder_annotation =
			(is_input && builder_fields.is_some()).then_some(metadata.features.wrap_annotation(
				FeatureName::Builder,
				&quote!(builder(#builder_fields)),
				is_macro,
			));

		struct_fields.push(quote! {
			#serde_annotation
			#query_annotation
			#builder_annotation
			pub #safe_name_ident: #output_token,
		});

		if is_input {
			let src_ident = quote!(self.#safe_name_ident);
			match (element.cardinality()) {
				(Cardinality::One) => {
					let conv = output
						.conversion
						.generate_encoding_chunk(&src_ident, &exports_ident);
					impl_named_args.push(quote!(#name => #conv,));
				}
				(Cardinality::NoResult) => {
					panic!("Fallible conversion kind is not supported for `NoResult` cardinality");
				}
				(Cardinality::AtMostOne) => {
					let closure_capture_ident = format_ident!("closure_opt_{}", safe_name_ident);
					let conv = output
						.conversion
						.generate_encoding_chunk(&quote!(#closure_capture_ident), &exports_ident);
					impl_named_args.push(quote! {
						#name => {
						  match self.#safe_name_ident.clone() {
							  Some(#closure_capture_ident) => {
								  Some( #conv )
							  },
							  None => None,
						  }
						},
					});
				}
				(Cardinality::Many | Cardinality::AtLeastOne) => {
					let closure_capture_ident = format_ident!("many_{}", safe_name_ident);
					let conv = output
						.conversion
						.generate_encoding_chunk(&quote!(#closure_capture_ident), &exports_ident);
					impl_named_args.push(quote! {
						#name => {
							let mut vec = Vec::with_capacity(self.#safe_name_ident.len());
							for #closure_capture_ident in &self.#safe_name_ident {
								vec.push( #conv );
							}
							vec
						},
					});
				}
			}
		}
	}

	let impl_tokens = is_input.then_some(quote! {
		impl #exports_ident::gel_protocol::query_arg::QueryArgs for #root_ident {
			fn encode(&self, encoder: &mut #exports_ident::gel_protocol::query_arg::Encoder) -> core::result::Result<(), #exports_ident::gel_errors::Error> {
				let map = #exports_ident::gel_protocol::named_args! {
					#(#impl_named_args)*
				};

				map.encode(encoder)
			}
		}
	});

	let derive_macro_paths = metadata.struct_derive_macro_paths();
	let struct_derive_tokens = metadata.features.get_struct_derive_features(
		&exports_ident,
		&derive_macro_paths,
		is_input,
		is_macro,
	);
	let struct_tokens = quote! {
		#struct_derive_tokens
		pub struct #root_ident {
			#(#struct_fields)*
		}

		#impl_tokens
	};

	tokens.extend(struct_tokens);

	Ok(Some(quote!(#root_ident)))
}

pub enum StructElement<'a> {
	Shape(&'a ShapeElement),
	InputShape(&'a InputShapeElement),
	Tuple(&'a TupleElement),
}

impl<'a> StructElement<'a> {
	pub fn from_shape(elements: &'a [ShapeElement]) -> Vec<StructElement<'a>> {
		elements.iter().map(From::from).collect::<Vec<_>>()
	}

	pub fn from_input_shape(elements: &'a [InputShapeElement]) -> Vec<StructElement<'a>> {
		elements.iter().map(From::from).collect::<Vec<_>>()
	}

	pub fn from_named_tuple(elements: &'a [TupleElement]) -> Vec<StructElement<'a>> {
		elements.iter().map(From::from).collect::<Vec<_>>()
	}

	pub fn name(&self) -> String {
		match self {
			StructElement::Shape(shape) => shape.name.clone(),
			StructElement::InputShape(input_shape) => input_shape.name.clone(),
			StructElement::Tuple(tuple) => tuple.name.clone(),
		}
	}

	pub fn type_pos(&self) -> TypePos {
		match self {
			StructElement::Shape(shape) => shape.type_pos,
			StructElement::InputShape(input_shape) => input_shape.type_pos,
			StructElement::Tuple(tuple) => tuple.type_pos,
		}
	}

	pub fn wrap(&self, token: &Option<TokenStream>) -> TokenStream {
		if let Cardinality::AtMostOne = self.cardinality() {
			quote!(Option<#token>)
		} else {
			quote!(#token)
		}
	}

	pub fn cardinality(&self) -> Cardinality {
		match self {
			StructElement::Shape(shape) => shape.cardinality.unwrap_or(Cardinality::NoResult),
			StructElement::InputShape(input_shape) => {
				input_shape.cardinality.unwrap_or(Cardinality::NoResult)
			}
			StructElement::Tuple(_) => Cardinality::NoResult,
		}
	}
}

impl<'a> From<&'a ShapeElement> for StructElement<'a> {
	fn from(value: &'a ShapeElement) -> Self {
		StructElement::Shape(value)
	}
}

impl<'a> From<&'a InputShapeElement> for StructElement<'a> {
	fn from(value: &'a InputShapeElement) -> Self {
		StructElement::InputShape(value)
	}
}

impl<'a> From<&'a TupleElement> for StructElement<'a> {
	fn from(value: &'a TupleElement) -> Self {
		StructElement::Tuple(value)
	}
}
