use std::fmt::Display;

/// The separator of a term.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub enum Sep {
  Comma, // ,
  Semi,  // ;
  Colon, // :

  /// *nothing*
  #[default]
  None,
}

/// The disposal, if the terms are disposed horizontally or vertically.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub enum Disposal {
  /// (
  ///   a,
  ///   b,
  /// )
  Vertical,

  /// (a, b)
  #[default]
  Horizontal,
}

/// Deliminator of a term.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub enum Delim {
  Paren,   // ( ... )
  Bracket, // [ ... ]
  Brace,   // { ... }
  Angle,   // < ... >

  #[default]
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
      Nfe::Apply { values, sep, .. } => {
        let mut iter = values.iter();

        if let Some(first) = iter.next() {
          write!(f, "{}", first)?;

          for value in iter {
            match sep {
              Sep::Comma => write!(f, ", {}", value)?,
              Sep::Semi => write!(f, "; {}", value)?,
              Sep::Colon => write!(f, ": {}", value)?,
              Sep::None => write!(f, "{}", value)?,
            }
          }
        }

        Ok(())
      },
    }
  }
}
