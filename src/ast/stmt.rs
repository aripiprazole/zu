use super::*;

/// Represents a command downgrade from statement, just like @type.
///
/// ## Examples
///
/// ```haskell
/// @type 10
/// ```
#[derive(Debug, Clone)]
pub struct Type<S: state::State> {
    pub value: Term<S>,
    pub location: S::Location,
}

impl<S: state::State> Element<S> for Type<S> {
    fn location(&self) -> &S::Location {
        &self.location
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
    pub location: S::Location,
}

impl<S: state::State> Element<S> for Eval<S> {
    fn location(&self) -> &S::Location {
        &self.location
    }
}

/// A statement. It can be an inductive type, or a downgrade.
#[derive(Clone)]
pub enum Stmt<S: state::State> {
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
    Type(Type<S>),

    /// Imports a name temporally until it's
    /// propertly resolved
    Import(S::Import),
}

impl<S: state::State> Stmt<S> {
    pub fn as_declaration(&self) -> Option<&dyn Declaration<S>> {
        match self {
            Self::Inductive(inductive) => Some(inductive),
            Self::Binding(binding) => Some(binding),
            _ => None,
        }
    }
}

impl<S: state::State> Debug for Stmt<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Error(arg0) => arg0.fmt(f),
            Self::Inductive(arg0) => arg0.fmt(f),
            Self::Binding(arg0) => arg0.fmt(f),
            Self::Eval(arg0) => arg0.fmt(f),
            Self::Type(arg0) => arg0.fmt(f),
            Self::Import(arg0) => arg0.fmt(f),
        }
    }
}

impl<S: state::State> Recovery<S> for Stmt<S> {
    fn recover_from_error(error: Error<S>) -> Self {
        Stmt::Error(error)
    }
}

impl<S: state::State> Element<S> for Stmt<S> {
    fn location(&self) -> &S::Location {
        match self {
            Stmt::Error(error) => &error.location,
            Stmt::Inductive(inductive) => &inductive.location,
            Stmt::Binding(binding) => &binding.location,
            Stmt::Eval(downgrade) => &downgrade.location,
            Stmt::Type(downgrade) => &downgrade.location,
            Stmt::Import(downgrade) => downgrade.location(),
        }
    }
}
