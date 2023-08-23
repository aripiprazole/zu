#![feature(never_type)]
#![feature(box_patterns)]
#![feature(exhaustive_patterns)]
#![feature(type_changing_struct_update)]

use clap::Parser;
use miette::IntoDiagnostic;

/// The abstract syntax tree for the language.
pub mod ast;

/// Parser LALRPOP mod.
pub mod parser;

/// Resolver module. It does handles the imports and the references.
///
/// It's the second phase of the compiler.
pub mod resolver;

// Type elaborator, it does the type checking stuff.
pub mod elab;

/// Simplifies the tree to the quoted terms.
pub mod erase;

/// Pretty print the elaborated ast
pub mod show;

/// Simple program to run `zu` language.
#[derive(clap::Parser, Debug)]
#[command(author, version, about, long_about = None)]
#[command(propagate_version = true)]
pub struct Command {
    #[arg(short, long)]
    pub include: Vec<String>,

    /// The file we would like to run, type check, etc
    pub main: String,
}

fn main() -> miette::Result<()> {
    bupropion::BupropionHandlerOpts::install(|| {
        // Build the bupropion handler options, for specific
        // error presenting.
        bupropion::BupropionHandlerOpts::new()
    })?;

    let command = Command::parse();

    fern::Dispatch::new()
        // Perform allocation-free log formatting
        .format(|out, message, record| {
            out.finish(format_args!(
                "{}: {}",
                record.level(),
                message
            ))
        })
        // Add blanket level filter -
        .level(log::LevelFilter::Debug)
        // - and per-module overrides
        .level_for("hyper", log::LevelFilter::Info)
        // Output to stdout, files, and other Dispatch configurations
        .chain(std::io::stdout())
        .apply()
        .into_diagnostic()?;

    let resolver = resolver::Resolver::new(command.main, command.include)?;
    resolver.resolve_and_import()?;

    Ok(())
}
