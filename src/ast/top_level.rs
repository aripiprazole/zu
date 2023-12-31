use super::*;

/// Represents a command downgrade from statement, just like @type.
///
/// ## Examples
///
/// ```haskell
/// @type 10
/// ```
#[derive(Debug, Clone)]
pub struct Check<S: state::State> {
  pub value: Term<S>,
  pub meta: S::Meta,
}

impl<S: state::State> Element<S> for Check<S> {
  fn meta(&self) -> &S::Meta {
    &self.meta
  }
}

/// Represents a command downgrade from statement, just like @eval and @type.
///
/// ## Examples
///
/// ```haskell
/// @eval 10
/// ```
#[derive(Debug, Clone)]
pub struct Eval<S: state::State> {
  pub value: Term<S>,
  pub meta: S::Meta,
}

impl<S: state::State> Element<S> for Eval<S> {
  fn meta(&self) -> &S::Meta {
    &self.meta
  }
}

/// A statement. It can be an inductive type, or a downgrade.
#[derive(Clone)]
pub enum TopLevel<S: state::State> {
  Error(Error<S>),

  /// An inductive type is a statement that introduces a new inductive
  /// type.
  Inductive(Inductive<S>),

  /// A binding is a statement that introduces a new binding.
  Binding(Binding<S>),

  // SECTION: Commands
  /// Downgrades a statement to a value.
  Eval(Eval<S>),

  /// Gets a type for a term.
  Check(Check<S>),

  /// Defines a signature.
  Signature(S::Siganture),

  /// Imports a name temporally until it's
  /// propertly resolved.
  Import(S::Import),
}

impl<S: state::State> TopLevel<S> {
  pub fn as_declaration(&self) -> Option<&dyn Declaration<S>> {
    match self {
      Self::Inductive(inductive) => Some(inductive),
      Self::Binding(binding) => Some(binding),
      _ => None,
    }
  }
}

impl<S: state::State> Debug for TopLevel<S> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Self::Error(arg0) => arg0.fmt(f),
      Self::Inductive(arg0) => arg0.fmt(f),
      Self::Binding(arg0) => arg0.fmt(f),
      Self::Eval(arg0) => arg0.fmt(f),
      Self::Check(arg0) => arg0.fmt(f),
      Self::Signature(arg0) => arg0.fmt(f),
      Self::Import(arg0) => arg0.fmt(f),
    }
  }
}

impl<S: state::State> Recovery<S> for TopLevel<S> {
  fn recover_from_error(error: Error<S>) -> Self {
    TopLevel::Error(error)
  }
}

impl<S: state::State> Element<S> for TopLevel<S> {
  fn meta(&self) -> &S::Meta {
    match self {
      TopLevel::Error(error) => &error.meta,
      TopLevel::Inductive(inductive) => &inductive.meta,
      TopLevel::Binding(binding) => &binding.meta,
      TopLevel::Eval(downgrade) => &downgrade.meta,
      TopLevel::Check(downgrade) => &downgrade.meta,
      Self::Signature(downgrade) => downgrade.meta(),
      TopLevel::Import(downgrade) => downgrade.meta(),
    }
  }
}
