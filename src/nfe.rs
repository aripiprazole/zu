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

/// The prefix of a term.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub enum Prefix {
  #[default]
  None,
  Plus,     // +
  Bang,     // !
  Question, // ?
  Tilde,    // ~
  Lambda,   // \
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

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct Apply {
  /// The arguments of the application.
  pub values: Vec<Nfe>,

  // SECTION: Pretty printing
  pub prefix: Prefix,
  pub sep: Sep,
  pub delim: Delim,
  pub disposal: Disposal,
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
  Apply(Apply),
}

impl Display for Nfe {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Nfe::S(v) => write!(f, "{v}"),
      Nfe::Nil => write!(f, "?"),
      Nfe::Apply(Apply {
        values,
        sep,
        delim,
        prefix,
        ..
      }) => {
        let mut iter = values.iter();
        match delim {
          Delim::Paren => write!(f, "(")?,
          Delim::Bracket => write!(f, "[")?,
          Delim::Brace => write!(f, "{{")?,
          Delim::Angle => write!(f, "<")?,
          Delim::None => {}
        }

        match prefix {
          Prefix::None => write!(f, "")?,
          Prefix::Plus => write!(f, "+")?,
          Prefix::Bang => write!(f, "!")?,
          Prefix::Question => write!(f, "?")?,
          Prefix::Tilde => write!(f, "~")?,
          Prefix::Lambda => write!(f, "\\")?,
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
          Delim::None => {}
        }

        Ok(())
      }
    }
  }
}
