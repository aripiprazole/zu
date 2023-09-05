use std::cell::Cell;
use std::fmt::Debug;
use std::marker::PhantomData;
use std::rc::Rc;

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

pub trait Ast<S: State> = Element<S> + Debug + Clone + PartialEq;

/// Represents the syntax state, if it's resolved, or just parsed, it's useful for not
/// having to redeclare the same types.
pub trait State: Default + Debug + Clone + PartialEq {
  // SECTION: Auxiliary
  type Parameters: Debug + Clone + PartialEq = Self::Definition;
  type Arguments: Debug + Clone + PartialEq = Vec<Term<Self>>;
  type NameSet: Debug + Clone + PartialEq = Self::Definition;

  // SECTION: Elements
  type Definition: Ast<Self> = Rc<crate::passes::resolver::Definition<Self>>;
  type Closure: Ast<Self> = Fun<Self>;
  type Elim: Ast<Self> = Elim<Self>;
  type Reference: Ast<Self>;

  // SECTION: Syntax sugars
  type Import: Ast<Self> = !;
  type Group: Ast<Self> = !;
  type Siganture: Ast<Self> = !;
  type Anno: Ast<Self> = !;

  // SECTION: Meta location
  /// The meta type, it's the type of the location of the syntax tree.
  type Meta: Debug + Clone + PartialEq = Location;
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

/// A span in the source code.
#[derive(Hash, PartialEq, Eq, Clone)]
pub struct Location {
  pub id: u64,
  pub start: usize,
  pub end: usize,
  pub filename: String,

  /// If the location is synthesized, it means that it's not a real location
  pub is_synthesized: bool,
}

impl Location {
  /// Returns the location if it's not synthesized.
  pub fn or_none(self) -> Option<Location> {
    if self.is_synthesized {
      None
    } else {
      Some(self)
    }
  }
}

impl Default for Location {
  fn default() -> Self {
    Self {
      id: Default::default(),
      start: Default::default(),
      end: Default::default(),
      filename: Default::default(),
      is_synthesized: true,
    }
  }
}

/// The default constant for [`Location`].
pub static SYNTHESIZED: Location = Location {
  id: 0,
  start: 0,
  end: 0,
  filename: String::new(),
  is_synthesized: true,
};

impl Location {
  /// Creates a new instance of [`Location`].
  pub fn new(start: usize, end: usize, filename: &str, unique: &Cell<u64>) -> Self {
    Self {
      id: unique.update(|x| x + 1),
      start,
      end,
      filename: filename.into(),
      is_synthesized: false,
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
#[derive(Debug, Clone, PartialEq)]
pub struct Error<S: state::State> {
  /// The error message.
  pub message: String,

  /// The original text that originated the error.
  pub full_text: String,

  /// The location of the error.
  pub meta: S::Meta,
}

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

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier<S: state::State> {
  pub text: String,
  pub meta: S::Meta,
}

impl<S: state::State> Element<S> for Identifier<S> {
  fn meta(&self) -> &S::Meta {
    &self.meta
  }
}

/// Int is a integer value like `0`, `1`, `2`, etc.
#[derive(Default, Debug, Clone, PartialEq)]
pub struct Str<S: state::State> {
  pub value: String,

  /// The location of the source in the source code.
  pub meta: S::Meta,
}

impl<S: state::State> Element<S> for Str<S> {
  fn meta(&self) -> &S::Meta {
    &self.meta
  }
}

/// Int is a integer value like `0`, `1`, `2`, etc.
#[derive(Default, Debug, Clone, PartialEq)]
pub struct Int<S: state::State> {
  /// The value of the integer.
  pub value: isize,

  /// The location of the integer in the source code.
  pub meta: S::Meta,
}

impl<S: state::State> Element<S> for Int<S> {
  fn meta(&self) -> &S::Meta {
    &self.meta
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern<S: state::State> {
  /// A variable pattern.
  Var(S::Definition, S::Meta),

  /// A constructor pattern.
  Constructor(S::Definition, Vec<Pattern<S>>, S::Meta),

  /// A wildcard pattern.
  Wildcard(S::Meta),
}

impl<S: state::State> Element<S> for Pattern<S> {
  fn meta(&self) -> &S::Meta {
    match self {
      Pattern::Var(_, meta) => meta,
      Pattern::Constructor(_, _, meta) => meta,
      Pattern::Wildcard(meta) => meta,
    }
  }
}

/// A case for eliminator.
#[derive(Debug, Clone, PartialEq)]
pub struct Case<S: state::State> {
  /// The pattern has a list of patterns, so we can pattern match a bunch of
  /// patterns at the same time.
  ///
  /// It's just like: `Cons x xs`, `Cons x' xs'`. But never this place is empty.
  pub pattern: NonEmpty<Box<Pattern<S>>>,
  pub value: Box<Term<S>>,
  pub meta: S::Meta,
}

impl<S: state::State> Element<S> for Case<S> {
  fn meta(&self) -> &S::Meta {
    &self.meta
  }
}

/// An eliminator. It has a list of patterns, and a location.
///
/// It's a simple eliminator for inductive types.
#[derive(Debug, Clone, PartialEq)]
pub struct Elim<S: state::State> {
  /// The scrutinee can be a list, it's useful for pattern matching with
  /// tuples, but when we don't have dependent pattern matching implemented,
  /// this is our way to pattern match tuples.
  ///
  /// It's just like: `Cons x xs`, `Cons x' xs'`. But never this place is empty.
  pub scrutinee: NonEmpty<Box<Term<S>>>,
  pub patterns: Vec<Case<S>>,
  pub meta: S::Meta,
}

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
#[derive(Debug, Clone, PartialEq)]
pub struct Apply<S: state::State> {
  pub callee: Box<Term<S>>,
  pub arguments: S::Arguments,
  pub meta: S::Meta,
}

impl<S: state::State> Element<S> for Apply<S> {
  fn meta(&self) -> &S::Meta {
    &self.meta
  }
}

/// An annotation expression term. It has a list of value and a type
/// representation.
#[derive(Debug, Clone, PartialEq)]
pub struct Anno<S: state::State> {
  pub value: Box<Term<S>>,
  pub type_repr: Box<Term<S>>,
  pub meta: S::Meta,
}

impl<S: state::State> Element<S> for Anno<S> {
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
#[derive(Debug, Clone, PartialEq)]
pub struct Fun<S: state::State> {
  pub arguments: S::Parameters,
  pub value: Box<Term<S>>,
  pub meta: S::Meta,
}

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
  Prim(Prim<S>),
  Anno(S::Anno),
  Fun(S::Closure),
  Elim(S::Elim),
  Group(S::Group),
  Reference(S::Reference),
}

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
mod impls {
  use super::*;

  impl<S: state::State> PartialEq for Term<S> {
    fn eq(&self, other: &Self) -> bool {
      match (self, other) {
        (Self::Pi(l0), Self::Pi(r0)) => l0 == r0,
        (Self::Int(l0), Self::Int(r0)) => l0 == r0,
        (Self::Str(l0), Self::Str(r0)) => l0 == r0,
        (Self::Hole(l0), Self::Hole(r0)) => l0 == r0,
        (Self::Apply(l0), Self::Apply(r0)) => l0 == r0,
        (Self::Error(l0), Self::Error(r0)) => l0 == r0,
        (Self::Prim(l0), Self::Prim(r0)) => l0 == r0,
        (Self::Anno(l0), Self::Anno(r0)) => l0 == r0,
        (Self::Fun(l0), Self::Fun(r0)) => l0 == r0,
        (Self::Elim(l0), Self::Elim(r0)) => l0 == r0,
        (Self::Group(l0), Self::Group(r0)) => l0 == r0,
        (Self::Reference(l0), Self::Reference(r0)) => l0 == r0,
        _ => false,
      }
    }
  }

  impl<S: state::State> Clone for Term<S> {
    fn clone(&self) -> Self {
      match self {
        Self::Error(arg0) => Self::Error(arg0.clone()),
        Self::Prim(arg0) => Self::Prim(arg0.clone()),
        Self::Int(arg0) => Self::Int(arg0.clone()),
        Self::Str(arg0) => Self::Str(arg0.clone()),
        Self::Group(arg0) => Self::Group(arg0.clone()),
        Self::Elim(arg0) => Self::Elim(arg0.clone()),
        Self::Anno(arg0) => Self::Anno(arg0.clone()),
        Self::Fun(arg0) => Self::Fun(arg0.clone()),
        Self::Apply(arg0) => Self::Apply(arg0.clone()),
        Self::Pi(arg0) => Self::Pi(arg0.clone()),
        Self::Reference(arg0) => Self::Reference(arg0.clone()),
        Self::Hole(arg0) => Self::Hole(arg0.clone()),
      }
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
      Self::Prim(arg0) => arg0.fmt(f),
      Self::Int(arg0) => arg0.fmt(f),
      Self::Str(arg0) => arg0.fmt(f),
      Self::Group(arg0) => arg0.fmt(f),
      Self::Elim(arg0) => arg0.fmt(f),
      Self::Fun(arg0) => arg0.fmt(f),
      Self::Apply(arg0) => arg0.fmt(f),
      Self::Pi(arg0) => arg0.fmt(f),
      Self::Anno(arg0) => arg0.fmt(f),
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
      Term::Prim(universe) => universe.meta(),
      Term::Int(int) => int.meta(),
      Term::Str(str) => str.meta(),
      Term::Elim(elim) => elim.meta(),
      Term::Group(group) => group.meta(),
      Term::Fun(fun) => fun.meta(),
      Term::Apply(apply) => apply.meta(),
      Term::Anno(anno) => anno.meta(),
      Term::Pi(pi) => pi.meta(),
      Term::Hole(hole) => hole.meta(),
      Term::Reference(atom) => atom.meta(),
    }
  }
}

// SECTION: Modules
pub use decl::*;
pub mod decl;

use nonempty::NonEmpty;
pub use stmt::*;
mod stmt;

pub use type_repr::*;
pub mod type_repr;
