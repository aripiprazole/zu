use super::*;

/// Simple attribute to the AST. It can hold a lot of things, and it's
/// defined primarily for the compiler.
#[derive(Debug, Clone)]
pub enum Attribute<S: state::State> {
    _TODO(PhantomData<S>),
}

/// A constructor for an inductive type. It has a name and a type.
///
/// ## Examples
///
/// For example, the constructors:
/// - `Zero : Nat`, and
/// - `Succ : Nat -> Nat`
///
/// Are constructors for the inductive type `nat`.
#[derive(Debug, Clone)]
pub struct Constructor<S: state::State> {
    pub name: S::Definition,
    pub type_repr: Term<S>,
    pub meta: S::Meta,
}

impl<S: state::State> Element<S> for Constructor<S> {
    fn meta(&self) -> &S::Meta {
        &self.meta
    }
}

/// A documentation string. It has a list of strings.
///
/// It's used to document declarations.
#[derive(Debug, Clone)]
pub struct DocString<S: state::State> {
    pub full_text: String,
    pub text: String,
    pub meta: S::Meta,
}

impl<S: state::State> Element<S> for DocString<S> {
    fn meta(&self) -> &S::Meta {
        &self.meta
    }
}

/// A declaration. It can be an inductive type, or a downgrade.
///
/// It's an wrapper for different types of declarations.
pub trait Declaration<S: state::State> {
    /// The documentation of the declaration.
    fn doc_strings(&self) -> &[DocString<S>];

    /// The attributes of the declaration.
    fn attributes(&self) -> &[Attribute<S>];

    /// The name of the declaration.
    fn name(&self) -> &<S as state::State>::Definition;
}

/// An inductive type. It has a name, a list of parameters, and a list of
/// constructors.
///
/// It's the same as algebraic data types, or generalized algebraic data
/// types.
///
/// ## Examples
///
/// For example, the inductive type `Nat`:
///
/// ```haskell
/// inductive data Nat where
///   Zero : Nat,
///   Succ : Nat -> Nat.
/// ```
#[derive(Debug, Clone)]
pub struct Inductive<S: state::State> {
    pub doc_strings: Vec<DocString<S>>,
    pub attributes: Vec<Attribute<S>>,
    pub name: S::Definition,
    pub parameters: Vec<Domain<S>>,
    pub constructors: Vec<Constructor<S>>,
    pub meta: S::Meta,
}

impl<S: state::State> Element<S> for Inductive<S> {
    fn meta(&self) -> &S::Meta {
        &self.meta
    }
}

impl<S: state::State> Declaration<S> for Inductive<S> {
    fn doc_strings(&self) -> &[DocString<S>] {
        &self.doc_strings
    }

    fn attributes(&self) -> &[Attribute<S>] {
        &self.attributes
    }

    fn name(&self) -> &<S as state::State>::Definition {
        &self.name
    }
}

/// Defines a binding. It has a name, a list of doc strings, and a value.
///
/// ## Examples
///
/// ```haskell
/// // | Defines natural numbers without induction feature, it's
/// // like functions in dependent langauges.
/// Nat := N : Type -> (N -> N) -> N -> N.
///
/// // | Defines the zero constructor
/// Zero : _ := \_, _, zero ->
///   zero.
///
/// // | Defines the succ constructor
/// Succ : _ := \n, N, succ, _ ->
///   (n N succ zero).
/// ```
#[derive(Debug, Clone)]
pub struct Binding<S: state::State> {
    pub doc_strings: Vec<DocString<S>>,
    pub attributes: Vec<Attribute<S>>,
    pub name: S::Definition,
    pub type_repr: Term<S>,
    pub value: Term<S>,
    pub meta: S::Meta,
}

impl<S: state::State> Element<S> for Binding<S> {
    fn meta(&self) -> &S::Meta {
        &self.meta
    }
}

impl<S: state::State> Declaration<S> for Binding<S> {
    fn doc_strings(&self) -> &[DocString<S>] {
        &self.doc_strings
    }

    fn attributes(&self) -> &[Attribute<S>] {
        &self.attributes
    }

    fn name(&self) -> &<S as state::State>::Definition {
        &self.name
    }
}
