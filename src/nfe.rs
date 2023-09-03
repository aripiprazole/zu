use std::fmt::Display;

/// The separator of a term.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Sep {
  Comma, // ,
  Semi,  // ;

  /// *nothing*
  None,
}

/// The disposal, if the terms are disposed horizontally or vertically.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Disposal {
  /// (
  ///   a,
  ///   b,
  /// )
  Vertical,

  /// (a, b)
  Horizontal,
}

/// Deliminator of a term.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Delim {
  Paren,   // ( ... )
  Bracket, // [ ... ]
  Brace,   // { ... }
  Angle,   // < ... >
  None,    // ...
}

/// Normal form of terms, that is used for the pretty printing.
///
/// Ou nota fiscal se você estiver no brasil.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Nfe {
  /// Simple string value that wraps a text.
  S(String),

  /// Wraps a list of terms. It can contain a separator and a
  /// delimiter.
  Apply {
    /// The arguments of the application.
    values: Vec<Nfe>,

    // SECTION: Pretty printing
    sep: Sep,
    delim: Delim,
    disposal: Disposal,
  },
}

impl Display for Nfe {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Nfe::S(v) => write!(f, "{v}"),
      Nfe::Apply { .. } => todo!(),
    }
  }
}
