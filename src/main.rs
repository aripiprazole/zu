#![feature(trait_alias)]
#![feature(never_type)]
#![feature(box_patterns)]
#![feature(type_changing_struct_update)]
#![feature(associated_type_defaults)]

use clap::Parser;
use lalrpop_util::lalrpop_mod;
use miette::IntoDiagnostic;
use passes::{elab::Elab, resolver::Resolver};

// The lalrpop module, it does generate the parser and lexer
// for the language.
lalrpop_mod! {
    #[allow(warnings)]
    /// The parsing module
    pub zu
}

/// The abstract syntax tree for the language. The abstract
/// syntax tree is the tree that represents the program
/// in a tree form.
pub mod ast;

/// Simplifies the tree to the quoted terms, it does
/// erase a location information.
pub mod erase;

/// Pretty print the elaborated ast to the screen
/// a type.
pub mod show;

/// The compiler passes.
pub mod passes {
    /// Resolver module. It does handles the imports and the references.
    ///
    /// It's the second phase of the compiler.
    pub mod resolver;

    /// Type elaborator module, it does the type checking stuff.
    ///
    /// It's the third phase of the compiler.
    pub mod elab;

    /// Parser LALRPOP module. It does uses a parse generator to
    /// generate a parser and lexer for the language.
    pub mod parser;

    /// The closure converter module. It converts the functions to closures.
    ///
    /// It's the third phase of the compiler.
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

fn program() -> miette::Result<()> {
    bupropion::BupropionHandlerOpts::install(|| {
        // Build the bupropion handler options, for specific
        // error presenting.
        bupropion::BupropionHandlerOpts::new()
    })
    .into_diagnostic()?;

    let mut elab = Elab::default();
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

    // Resolve the file and import the declarations
    // from the file.
    let file = Resolver::new(command.main, command.include)?.resolve_and_import()?;
    let file = elab.elaborate(file)?;
    let _ = file;

    Ok(())
}

// The main function wrapper around [`crate::program`].
fn main() {
    // Avoid printing print `Error: ` before the error message
    // to maintain the language beauty!
    if let Err(e) = program() {
        eprintln!("{e:?}");
        std::process::exit(1);
    }
}
