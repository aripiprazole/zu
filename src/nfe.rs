use std::fmt::Display;


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Sep {
  Comma,
  Semi,
  None,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Delim {
  Paren,
  Bracket,
  Brace,
  Angle,
  None,
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
    args: Vec<Nfe>,
    sep: Sep,
    delim: Delim,
  },
}

impl Display for Nfe {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Nfe::S(v) => write!(f, "{v}"),
      Nfe::Apply { .. } => todo!()
    }
  }
}
