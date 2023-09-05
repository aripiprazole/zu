use std::fmt::Display;

/// The separator of a term.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub enum Sep {
  Comma, // ,
  Semi,  // ;
  Colon, // :
  ArrL,  // ->
  ArrR,  // <-

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
  None, // ...
}

/// Normal form of terms, that is used for the pretty printing.
///
/// Ou nota fiscal se você estiver no brasil.
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub enum Nfe {
  /// Simple string value that wraps a text.
  S(String),

  /// Normal form of a function.
  #[default]
  Nil,

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
      Nfe::Nil => write!(f, "?"),
      Nfe::Apply { values, sep, delim, .. } => {
        let mut iter = values.iter();
        match delim {
            Delim::Paren => write!(f, "(")?,
            Delim::Bracket => write!(f, "[")?,
            Delim::Brace => write!(f, "{{")?,
            Delim::Angle => write!(f, "<")?,
            Delim::None => {},
        }

        if let Some(first) = iter.next() {
          write!(f, "{}", first)?;

          for value in iter {
            match sep {
              Sep::Comma => write!(f, ", {value}")?,
              Sep::Semi => write!(f, "; {value}")?,
              Sep::Colon => write!(f, " : {value}")?,
              Sep::None => write!(f, "{value}")?,
              Sep::ArrL => write!(f, " -> {value}")?,
              Sep::ArrR => write!(f, " <- {value}")?,
            }
          }
        }

        match delim {
            Delim::Paren => write!(f, ")")?,
            Delim::Bracket => write!(f, "]")?,
            Delim::Brace => write!(f, "}}")?,
            Delim::Angle => write!(f, ">")?,
            Delim::None => {},
        }

        Ok(())
      }
    }
  }
}
