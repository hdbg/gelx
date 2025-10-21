#![doc(html_logo_url = "https://raw.githubusercontent.com/ifiokjr/gelx/main/setup/assets/logo.png")]

use std::path::Path;
use std::path::PathBuf;

use check_keyword::CheckKeyword;
use clap::Parser;
use gelx_core::GelxCoreError;
use gelx_core::GelxCoreResult;
use gelx_core::GelxMetadata;
use gelx_core::ModuleOutputs;
use gelx_core::generate_module_outputs;
use gelx_core::generate_query_token_stream;
use gelx_core::get_descriptor;
use heck::ToSnakeCase;
use proc_macro2::TokenStream;
use quote::format_ident;
use quote::quote;
use similar::ChangeTag;
use similar::TextDiff;
use tokio::fs;

pub mod prelude {
	pub use clap::Parser;
}

/// A CLI for generating typed Rust code from Gel queries. This is available as
/// library code in case you want to further customize the generated output.
#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
pub struct Cli {
	#[clap(subcommand)]
	pub command: Commands,

	/// Optional working directory to run the command from.
	#[clap(long, value_parser = clap::value_parser!(PathBuf), global = true)]
	pub cwd: Option<PathBuf>,
}

impl Cli {
	/// Runs the CLI.
	pub async fn run(&self) -> GelxCoreResult<()> {
		let current_dir = std::env::current_dir()?;

		// Change current directory if --cwd is provided
		if let Some(path) = &self.cwd {
			let absolute_path = current_dir.join(path);

			if !absolute_path.is_dir() {
				let message = format!(
					"Error: Invalid --cwd path: {} is not a directory or does not exist.",
					path.display()
				);
				eprintln!("{message}");

				return Err(GelxCoreError::Custom(message));
			}

			std::env::set_current_dir(absolute_path)?;
			eprintln!("Running from directory: {}", path.display());
		}

		// Load metadata from Cargo.toml or gelx.toml in the current (potentially
		// changed) directory
		let current_dir = std::env::current_dir()?;
		let metadata = GelxMetadata::try_new(&current_dir)?;
		let root_path = metadata.root_path.clone().unwrap_or(current_dir);

		match self.command {
			Commands::Generate { json } => Self::generate(&metadata, &root_path, json).await,
			Commands::Check => Self::check(&metadata, &root_path).await,
		}
	}

	/// Generates the module outputs from the queries in the current directory.
	pub async fn outputs(
		metadata: &GelxMetadata,
		root_path: impl AsRef<Path>,
	) -> GelxCoreResult<ModuleOutputs> {
		let root_path = root_path.as_ref();
		let queries_path = root_path.join(&metadata.queries_path);
		let query_tokens = if queries_path.is_dir() {
			Self::process_queries_recursive(&queries_path, metadata).await?
		} else {
			TokenStream::new()
		};

		let mut outputs = generate_module_outputs(metadata).await?;
		outputs.append_to_root(&query_tokens);

		Ok(outputs)
	}

	/// Recursively processes query files and directories, generating the
	/// appropriate module structure.
	async fn process_queries_recursive(
		dir_path: &Path,
		metadata: &GelxMetadata,
	) -> GelxCoreResult<TokenStream> {
		let mut tokens = TokenStream::new();
		// not async to make sorting easier
		let mut entries = dir_path.read_dir()?.collect::<Result<Vec<_>, _>>()?;
		entries.sort_by_key(std::fs::DirEntry::path);

		for entry in entries {
			let path = entry.path();

			if path.is_file() {
				// Process .edgeql files
				if path.extension().is_some_and(|ext| ext == "edgeql") {
					let query_content = fs::read_to_string(&path).await?;
					let file_stem = path.file_stem().unwrap_or_default().to_string_lossy();
					let module_name = file_stem.to_snake_case();

					eprintln!("Processing query: {}", path.display());
					let descriptor = get_descriptor(&query_content, metadata).await?;
					let token_stream = generate_query_token_stream(
						&descriptor,
						&module_name,
						&query_content,
						metadata,
						false,
					)?;

					tokens.extend(token_stream);
				}
			} else if path.is_dir() {
				// Recursively process subdirectories
				let dir_name = path
					.file_name()
					.unwrap_or_default()
					.to_string_lossy()
					.to_string();
				let safe_dir_name = dir_name.to_snake_case().into_safe();
				let module_ident = format_ident!("{}", safe_dir_name);

				eprintln!("Processing directory: {}", path.display());
				let submodule_tokens =
					Box::pin(Self::process_queries_recursive(&path, metadata)).await?;

				// Only generate module if it has content
				if !submodule_tokens.is_empty() {
					tokens.extend(quote! {
						pub mod #module_ident {
							use super::*;

							#submodule_tokens
						}
					});
				}
			}
		}

		Ok(tokens)
	}

	/// Generates Rust code from the crate in the current directory.
	pub async fn generate(
		metadata: &GelxMetadata,
		root_path: impl AsRef<Path>,
		json: bool,
	) -> GelxCoreResult<()> {
		eprintln!("Generating code...");
		let root_path = root_path.as_ref();
		let outputs = Self::outputs(metadata, root_path).await?;
		let output_path = root_path.join(&metadata.output_path);

		if json {
			let json = outputs.to_map()?;
			println!("{}", serde_json::to_string_pretty(&json)?);
		} else {
			let _ = fs::remove_dir_all(&output_path).await; // clean up the output directory
			fs::create_dir_all(&output_path).await?;
			eprintln!("Writing generated code to {}", output_path.display());
			outputs.write_to_fs(&output_path).await?;

			eprintln!(
				"Successfully wrote generated code to {}",
				metadata.output_path.display()
			);
		}

		Ok(())
	}

	/// Checks if the generated Rust code is up-to-date
	pub async fn check(metadata: &GelxMetadata, root_path: impl AsRef<Path>) -> GelxCoreResult<()> {
		eprintln!("Checking code...");
		let root_path = root_path.as_ref();
		let output_path = root_path.join(&metadata.output_path);

		if !output_path.exists() {
			let message = format!(
				"Error: Output file {} does not exist. Run `gelx generate` first.",
				metadata.output_path.display()
			);
			eprintln!("{message}");

			return Err(GelxCoreError::Custom(message));
		}

		let generated_map = Self::outputs(metadata, root_path).await?.to_map()?;
		let existing_map = ModuleOutputs::try_new(&output_path, &output_path)
			.await?
			.to_map()?;
		let mut comparison = Vec::new();

		for (path, content) in &generated_map {
			let Some(existing_content) = existing_map.get(path) else {
				comparison.push(Comparison::Add(path.clone()));
				continue;
			};

			if existing_content != content {
				let diff = TextDiff::from_lines(existing_content, content);
				let mut changes = Vec::new();

				for change in diff.iter_all_changes() {
					let sign = match change.tag() {
						ChangeTag::Delete => "-",
						ChangeTag::Insert => "+",
						ChangeTag::Equal => " ",
					};
					changes.push(format!("{sign}{change}"));
				}

				comparison.push(Comparison::Change(path.clone(), changes));
			}
		}

		for path in existing_map.keys() {
			if !generated_map.contains_key(path) {
				comparison.push(Comparison::Remove(path.clone()));
			}
		}

		if comparison.is_empty() {
			eprintln!("Generated code is up-to-date.");
			return Ok(());
		}

		for change in comparison {
			match change {
				Comparison::Add(path) => {
					eprintln!("Added: {}", root_path.join(&path).display());
				}
				Comparison::Remove(path) => {
					eprintln!("Removed: {}", root_path.join(&path).display());
				}
				Comparison::Change(path, diffs) => {
					eprintln!("Changed: {}", root_path.join(&path).display());

					for diff in diffs {
						eprintln!("{diff}");
					}
				}
			}
		}

		Ok(())
	}
}

#[derive(Parser, Debug)]
pub enum Commands {
	/// Generates Rust code from the crate in the current directory.
	Generate {
		/// Print the generated code as JSON to stdout instead of writing to the
		/// directory.
		#[clap(long)]
		json: bool,
	},
	/// Checks if the generated Rust code is up-to-date
	Check,
}

pub enum Comparison {
	/// The file was added.
	Add(PathBuf),
	/// The file was removed.
	Remove(PathBuf),
	/// There are changes to the file.
	Change(PathBuf, Vec<String>),
}
