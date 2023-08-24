use std::{fmt::Debug, marker::PhantomData, rc::Rc};

/// File definition, it contains all the statements,
/// the module name, and a base location for it as anchor
/// for the statements.
#[derive(Default, Debug, Clone)]
pub struct File<S: state::State> {
    pub name: String,
    pub stmts: Vec<Stmt<S>>,
    pub meta: S::Meta,
}

/// The ast GAT state. It's more likelly a Tree That Grow, with the
/// rust features, but that's it.
pub mod state {
    /// Retro-compatibility, just for not having to change the existing source code using
    /// the old state import of [`crate::ast::state`].
    pub use super::State;
}

pub trait Ast<S: State> = Element<S> + Debug + Clone;

/// Just to represent nodes.
pub trait Node<S: state::State>: Ast<S> {}

/// Represents the syntax state, if it's resolved, or just parsed, it's useful for not
/// having to redeclare the same types.
pub trait State: Default + Debug + Clone {
    // SECTION: Auxiliary
    type Parameters: Debug + Clone = Self::Definition;
    type Arguments: Debug + Clone = Vec<Term<Self>>;
    type NameSet: Debug + Clone = Self::Definition;

    // SECTION: Elements
    type Definition: Ast<Self> = Rc<Definition<Self>>;
    type Closure: Ast<Self> = Fun<Self>;
    type Elim: Ast<Self> = Elim<Self>;
    type Reference: Ast<Self>;

    // SECTION: Syntax sugars
    type Import: Ast<Self> = !;
    type Group: Ast<Self> = !;

    // SECTION: Meta location
    /// The meta type, it's the type of the location of the syntax tree.
    type Meta: Debug + Clone = Location;
}

impl<S: state::State> Element<S> for ! {
    fn meta(&self) -> &S::Meta {
        unreachable!()
    }
}

impl<S: state::State, T: Element<S>> Element<S> for Rc<T> {
    fn meta(&self) -> &S::Meta {
        self.as_ref().meta()
    }
}

impl<S: state::State, T: Element<S>> Element<S> for Box<T> {
    fn meta(&self) -> &S::Meta {
        self.as_ref().meta()
    }
}

#[derive(Debug)]
pub enum DefinitionKind {
    Constructor,
    Inductive,
    Binding,
}

/// A definition. It has a text, and a location.
#[derive(Default, Debug, Clone, Hash)]
pub struct Definition<S: state::State> {
    pub text: String,
    pub meta: S::Meta,
}

impl<S: state::State> Node<S> for Definition<S> {}

impl<S: state::State> Definition<S>
where
    S::Meta: Default,
{
    /// Creates a new instance of [`Definition`].
    pub fn new(text: String) -> Self {
        Self {
            text,
            meta: S::Meta::default(),
        }
    }
}

impl<S: state::State> Element<S> for Definition<S> {
    fn meta(&self) -> &S::Meta {
        &self.meta
    }
}

#[derive(Default, Hash, PartialEq, Eq, Clone)]
pub struct Location {
    pub start: usize,
    pub end: usize,
    pub filename: String,
}

impl Location {
    /// Creates a new instance of [`Location`].
    pub fn new(start: usize, end: usize, filename: &str) -> Self {
        Self {
            start,
            end,
            filename: filename.into(),
        }
    }
}

impl Debug for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Location")
    }
}

impl From<Location> for miette::SourceSpan {
    fn from(value: Location) -> Self {
        Self::from(value.start..value.end)
    }
}

/// An element. It can be a declaration, or a term.
pub trait Element<S: state::State> {
    fn meta(&self) -> &S::Meta;
}

/// Error node, it does contains an error.
#[derive(Debug, Clone)]
pub struct Error<S: state::State> {
    /// The error message.
    pub message: String,

    /// The original text that originated the error.
    pub full_text: String,

    /// The location of the error.
    pub meta: S::Meta,
}

impl<S: state::State> Node<S> for Error<S> {}

/// Represents a recovery from an error.
pub trait Recovery<S: state::State> {
    /// Creates a new instance of [`Self`] when it's an error, it's
    /// useful for enums
    fn recover_from_error(error: Error<S>) -> Self;
}

impl<S: state::State> Element<S> for Error<S> {
    fn meta(&self) -> &S::Meta {
        &self.meta
    }
}

#[derive(Debug, Clone)]
pub struct Identifier<S: state::State> {
    pub text: String,
    pub meta: S::Meta,
}

impl<S: state::State> Node<S> for Identifier<S> {}

impl<S: state::State> Element<S> for Identifier<S> {
    fn meta(&self) -> &S::Meta {
        &self.meta
    }
}

/// Int is a integer value like `0`, `1`, `2`, etc.
#[derive(Default, Debug, Clone)]
pub struct Str<S: state::State> {
    pub value: String,

    /// The location of the source in the source code.
    pub meta: S::Meta,
}

impl<S: state::State> Node<S> for Str<S> {}

impl<S: state::State> Element<S> for Str<S> {
    fn meta(&self) -> &S::Meta {
        &self.meta
    }
}

/// Int is a integer value like `0`, `1`, `2`, etc.
#[derive(Default, Debug, Clone)]
pub struct Int<S: state::State> {
    /// The value of the integer.
    pub value: isize,

    /// The location of the integer in the source code.
    pub meta: S::Meta,
}

impl<S: state::State> Node<S> for Int<S> {}

impl<S: state::State> Element<S> for Int<S> {
    fn meta(&self) -> &S::Meta {
        &self.meta
    }
}

/// A pattern. It has a definition, a list of arguments, and a location.
///
/// It's a simple pattern for eliminator.
#[derive(Default, Debug, Clone)]
pub struct Pattern<S: state::State> {
    pub constructor: S::Reference,
    pub arguments: Vec<S::Definition>,
    pub meta: S::Meta,
}

impl<S: state::State> Node<S> for Pattern<S> {}

impl<S: state::State> Element<S> for Pattern<S> {
    fn meta(&self) -> &S::Meta {
        &self.meta
    }
}

/// A case for eliminator.
#[derive(Debug, Clone)]
pub struct Case<S: state::State> {
    pub patterns: Vec<Pattern<S>>,
    pub value: Box<Term<S>>,
    pub meta: S::Meta,
}

impl<S: state::State> Node<S> for Case<S> {}

impl<S: state::State> Element<S> for Case<S> {
    fn meta(&self) -> &S::Meta {
        &self.meta
    }
}

/// An eliminator. It has a list of patterns, and a location.
///
/// It's a simple eliminator for inductive types.
#[derive(Debug, Clone)]
pub struct Elim<S: state::State> {
    pub patterns: Vec<Case<S>>,
    pub meta: S::Meta,
}

impl<S: state::State> Node<S> for Elim<S> {}

impl<S: state::State> Element<S> for Elim<S> {
    fn meta(&self) -> &S::Meta {
        &self.meta
    }
}

/// An application term. It has a list of arguments, and a function.
///
/// ## Examples
///
/// ```haskell
/// (a b c)
/// ```
#[derive(Debug, Clone)]
pub struct Apply<S: state::State> {
    pub callee: Box<Term<S>>,
    pub arguments: S::Arguments,
    pub meta: S::Meta,
}

impl<S: state::State> Node<S> for Apply<S> {}

impl<S: state::State> Element<S> for Apply<S> {
    fn meta(&self) -> &S::Meta {
        &self.meta
    }
}

/// A function term. It has a list of arguments, and a value.
///
/// ## Examples
///
/// ```haskell
/// -- | Defines the succ constructor
/// Succ = \fun n, N, succ, _
///   (n N succ zero).
/// ```
#[derive(Debug, Clone)]
pub struct Fun<S: state::State> {
    pub arguments: S::Parameters,
    pub value: Box<Term<S>>,
    pub meta: S::Meta,
}

impl<S: state::State> Node<S> for Fun<S> {}

impl<S: state::State> Element<S> for Fun<S> {
    fn meta(&self) -> &S::Meta {
        &self.meta
    }
}

/// A term. It can be an integer, a variable, an application, or a pi type.
///
/// It's the base of the abstract syntax tree.
pub enum Term<S: state::State> {
    Pi(Pi<S>),
    Int(Int<S>),
    Str(Str<S>),
    Hole(Hole<S>),
    Apply(Apply<S>),
    Error(Error<S>),
    Universe(Universe<S>),
    Fun(S::Closure),
    Elim(S::Elim),
    Group(S::Group),
    Reference(S::Reference),
}

impl<S: state::State> Node<S> for Term<S> {}

/// Necessary due to this [issue here](https://github.com/rust-lang/rust/issues/39959).
/// 
/// The bounds would be generated like the following:
/// ```rust,norun
/// impl <S: state::State> Clone for Term<S> where
///     S::Arguments: Clone,
///     S::Reference: Clone,
///     S::Parameters: Clone,
///     S::Definition: Clone,
///     S::Group: Clone,
///     S::Meta: Clone,
///     S::Closure: Clone,
///     ...
/// ```
/// 
/// And since we have a lot of bounds, it's better to just use the `Clone` trait
/// directly. Because it would cause the compiler to crash over an infinite loop, just like
/// when we have types like following:
/// 
/// ```rust,norun
/// enum Ast {
///     Const(isize),
///     App(Ast, Ast)
/// }
/// ```
/// 
/// The compiler would try this, but on the GATs.
impl<S: state::State> Clone for Term<S> {
    fn clone(&self) -> Self {
        match self {
            Self::Error(arg0) => Self::Error(arg0.clone()),
            Self::Universe(arg0) => Self::Universe(arg0.clone()),
            Self::Int(arg0) => Self::Int(arg0.clone()),
            Self::Str(arg0) => Self::Str(arg0.clone()),
            Self::Group(arg0) => Self::Group(arg0.clone()),
            Self::Elim(arg0) => Self::Elim(arg0.clone()),
            Self::Fun(arg0) => Self::Fun(arg0.clone()),
            Self::Apply(arg0) => Self::Apply(arg0.clone()),
            Self::Pi(arg0) => Self::Pi(arg0.clone()),
            Self::Reference(arg0) => Self::Reference(arg0.clone()),
            Self::Hole(arg0) => Self::Hole(arg0.clone()),
        }
    }
}

impl<S: state::State> Default for Term<S>
where
    S::Meta: Default,
{
    fn default() -> Self {
        Self::Hole(Hole::default())
    }
}

impl<S: state::State> Debug for Term<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Error(arg0) => arg0.fmt(f),
            Self::Universe(arg0) => arg0.fmt(f),
            Self::Int(arg0) => arg0.fmt(f),
            Self::Str(arg0) => arg0.fmt(f),
            Self::Group(arg0) => arg0.fmt(f),
            Self::Elim(arg0) => arg0.fmt(f),
            Self::Fun(arg0) => arg0.fmt(f),
            Self::Apply(arg0) => arg0.fmt(f),
            Self::Pi(arg0) => arg0.fmt(f),
            Self::Reference(arg0) => arg0.fmt(f),
            Self::Hole(arg0) => arg0.fmt(f),
        }
    }
}

impl<S: state::State> Recovery<S> for Term<S> {
    fn recover_from_error(error: Error<S>) -> Self {
        Term::Error(error)
    }
}

impl<S: state::State> Element<S> for Term<S> {
    fn meta(&self) -> &S::Meta {
        match self {
            Term::Error(error) => error.meta(),
            Term::Universe(universe) => universe.meta(),
            Term::Int(int) => int.meta(),
            Term::Str(str) => str.meta(),
            Term::Elim(elim) => elim.meta(),
            Term::Group(group) => group.meta(),
            Term::Fun(fun) => fun.meta(),
            Term::Apply(apply) => apply.meta(),
            Term::Pi(pi) => pi.meta(),
            Term::Hole(hole) => hole.meta(),
            Term::Reference(atom) => atom.meta(),
        }
    }
}

// SECTION: Modules
pub use decl::*;
pub mod decl;

pub use stmt::*;
mod stmt;

pub use type_repr::*;
pub mod type_repr;
