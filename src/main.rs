#![feature(trait_alias)]
#![feature(never_type)]
#![feature(box_patterns)]
#![feature(type_changing_struct_update)]
#![feature(associated_type_defaults)]

use clap::Parser;
use lalrpop_util::lalrpop_mod;
use miette::IntoDiagnostic;

lalrpop_mod! {
    #[allow(warnings)]
    /// The parsing module
    pub zu
}

/// The abstract syntax tree for the language.
pub mod ast;

/// Simplifies the tree to the quoted terms.
pub mod erase;

/// Pretty print the elaborated ast
pub mod show;

/// The compiler passes.
pub mod pass {
    /// Resolver module. It does handles the imports and the references.
    ///
    /// It's the second phase of the compiler.
    pub mod resolver;

    // Type elaborator module, it does the type checking stuff.
    pub mod elab;

    /// Parser LALRPOP module.
    pub mod parser;

    /// The closure converter module.
    pub mod closure_conv;
}

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
        .format(|out, message, record| out.finish(format_args!("{}: {}", record.level(), message)))
        // Add blanket level filter -
        .level(log::LevelFilter::Debug)
        // - and per-module overrides
        .level_for("hyper", log::LevelFilter::Info)
        // Output to stdout, files, and other Dispatch configurations
        .chain(std::io::stdout())
        .apply()
        .into_diagnostic()?;

    let resolver = pass::resolver::Resolver::new(command.main, command.include)?;
    resolver.resolve_and_import()?;

    Ok(())
}
