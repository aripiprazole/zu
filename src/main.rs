#![feature(cell_update)]
#![feature(trait_alias)]
#![feature(never_type)]
#![feature(box_patterns)]
#![feature(type_changing_struct_update)]
#![feature(associated_type_defaults)]

use clap::Parser;
use lalrpop_util::lalrpop_mod;
use miette::IntoDiagnostic;
use nfe::Nfe;
use owo_colors::OwoColorize;
use passes::elab::Elab;
use passes::elab::Reporter;
use passes::resolver::Resolver;

// The lalrpop module, it does generate the parser and lexer
// for the language.
lalrpop_mod! {
    #[allow(warnings)]
    /// The parsing module
    pub zu
}

/// Meta variables stuff, like holes, etc.
pub mod meta;

/// Debruijin indexes module. It does convert the
/// named variables to the debruijin indexes.
pub mod debruijin;

/// The abstract syntax tree for the language. The abstract
/// syntax tree is the tree that represents the program
/// in a tree form.
pub mod ast;

/// Simplifies the tree to the quoted terms, it does
/// erase a location information.
pub mod quoting;

/// Pretty print the elaborated ast to the screen
/// a type.
pub mod show;

/// Pretty print abstract syntax tree that represents
/// the normalised terms.
pub mod nfe;

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

  ///// The closure converter module. It converts the functions to closures.
  /////
  ///// It's the third phase of the compiler.
  // pub mod closure_conv;
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

#[derive(Debug, Clone, PartialEq)]
pub struct LoggerReporter;

impl Reporter for LoggerReporter {
  /// Evaluates a value at a specific location.
  fn evaluate(&self, value: Nfe, location: ast::Location) -> miette::Result<()> {
    let filename = location.filename;
    let start = location.start;
    let end = location.end;

    log::info!("evaluated {} at {filename}:{start}:{end}", value);
    Ok(())
  }

  /// Checks a value at a specific location.
  fn check(&self, value: Nfe, location: ast::Location) -> miette::Result<()> {
    let filename = location.filename;
    let start = location.start;
    let end = location.end;

    log::info!("checked {} at {filename}:{start}:{end}", value);
    Ok(())
  }
}

/// Capitalizes the first character in s.
fn capitalize(s: &str) -> String {
  let mut c = s.chars();
  match c.next() {
    None => String::new(),
    Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
  }
}

/// Logger function for the fern logger.
///
/// It does format the log message to a specific format.
fn log(out: fern::FormatCallback, message: &std::fmt::Arguments, record: &log::Record) {
  let style = match record.level() {
    log::Level::Error => owo_colors::Style::new().red().bold(),
    log::Level::Warn => owo_colors::Style::new().yellow().bold(),
    log::Level::Info => owo_colors::Style::new().bright_blue().bold(),
    log::Level::Debug => owo_colors::Style::new().bright_red().bold(),
    log::Level::Trace => owo_colors::Style::new().bright_cyan().bold(),
  };
  let level = capitalize(&record.level().to_string().to_lowercase());
  let level = level.style(style);

  out.finish(format_args!("  {level:>7} {}", message))
}

/// The main function of the program.
fn program() -> miette::Result<()> {
  // Initialize the bupropion handler with miette
  bupropion::BupropionHandlerOpts::install(|| {
    // Build the bupropion handler options, for specific
    // error presenting.
    bupropion::BupropionHandlerOpts::new()
  })
  .into_diagnostic()?;

  // Initialize the logger
  fern::Dispatch::new() // Perform allocation-free log formatting
    .format(log) // Add blanket level filter -
    .level(log::LevelFilter::Debug) // - and per-module overrides
    .level_for("hyper", log::LevelFilter::Info) // Output to stdout, files, and other Dispatch configurations
    .chain(std::io::stdout())
    .apply()
    .into_diagnostic()?;

  let command = Command::parse();

  let resolver = Resolver::new(command.main, command.include)?;
  let mut elab = Elab::new(resolver.files.clone(), LoggerReporter);

  // Resolve the file and import the declarations
  // from the file.
  let file = resolver.resolve_and_import()?;
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
