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
    use std::rc::Rc;

    use super::*;

    /// Represents the syntax state, if it's resolved, or just parsed, it's useful for not
    /// having to redeclare the same types.
    pub trait State: Default + Debug + Clone {
        type Parameters: Debug + Clone;
        type Arguments: Debug + Clone;
        type Meta: Debug + Clone;
        type NameSet: Debug + Clone;
        type Import: Element<Self>;
        type Reference: Element<Self>;
        type Definition: Element<Self>;
    }

    /// Represents the parsed state, it's the state of the syntax tree when it's just parsed.
    #[derive(Default, Debug, Clone)]
    pub struct Syntax;

    impl State for Syntax {
        type NameSet = Vec<Option<Self::Definition>>;
        type Arguments = Vec<Term<Self>>;
        type Parameters = Vec<Self::Definition>;
        type Definition = syntax::Reference;
        type Reference = syntax::Reference;
        type Import = syntax::Import;
        type Meta = Location;
    }

    /// Represents the resolved state, it's the state of the syntax tree when it's resolved.
    #[derive(Default, Debug, Clone)]
    pub struct Resolved;

    impl State for Resolved {
        type NameSet = Self::Definition;
        type Arguments = Vec<Term<Resolved>>;
        type Parameters = Self::Definition;
        type Definition = Rc<Definition<Resolved>>;
        type Reference = resolved::Reference;
        type Meta = Location;
        type Import = !;
    }

    /// Represents the resolved state, it's the state of the syntax tree when it's resolved.
    #[derive(Default, Debug, Clone)]
    pub struct Typed;

    impl State for Typed {
        type NameSet = Self::Definition;
        type Arguments = Vec<Term<Resolved>>;
        type Parameters = Self::Definition;
        type Definition = Rc<Definition<Typed>>;
        type Reference = typed::Reference;
        type Meta = typed::TypedMeta;
        type Import = !;
    }

    /// Represents the resolved state, it's the state of the syntax tree when it's resolved.
    #[derive(Default, Debug, Clone)]
    pub struct Quoted;

    impl State for Quoted {
        type NameSet = Self::Definition;
        type Parameters = Self::Definition;
        type Arguments = Box<Term<Self>>;
        type Definition = Definition<Quoted>;
        type Reference = quoted::Reference;
        type Import = !;
        type Meta = ();
    }
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

#[derive(Debug)]
pub enum DefinitionKind {
    Constructor,
    Inductive,
    Binding,
}

/// A definition. It has a text, and a location.
pub mod syntax {
    use super::*;

    /// A name access.
    #[derive(Debug, Clone)]
    pub struct Reference {
        pub text: String,
        pub meta: Location,
    }

    impl<S: state::State<Meta = Location>> Element<S> for Reference {
        fn meta(&self) -> &Location {
            &self.meta
        }
    }

    /// Imports a name temporally until it's
    /// propertly resolved
    #[derive(Debug, Clone)]
    pub struct Import {
        pub text: String,
        pub meta: Location,
    }

    impl<S: state::State<Meta = Location>> Element<S> for Import {
        fn meta(&self) -> &Location {
            &self.meta
        }
    }
}

/// Resolved state, it's the state of the syntax tree when it's resolved.
pub mod resolved {
    use super::*;

    /// A name access.
    #[derive(Debug, Clone)]
    pub struct Reference {
        pub definition: Rc<Definition<state::Resolved>>,
        pub meta: Location,
    }

    impl<S: state::State<Meta = Location>> Element<S> for Reference {
        fn meta(&self) -> &Location {
            &self.meta
        }
    }
}

/// Quoted state, it's the state of the syntax tree when it's quoted.
pub mod quoted {
    use super::*;

    #[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
    pub enum BD {
        Bound,
        Defined,
    }

    #[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
    pub struct MetaVar(pub usize);

    #[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
    pub enum Reference {
        Var(Ix),
        MetaVar(MetaVar),
        InsertedMeta(MetaVar, im_rc::Vector<BD>),
    }

    impl<S: state::State<Meta = ()>> Element<S> for Reference {
        fn meta(&self) -> &S::Meta {
            &()
        }
    }

    /// Defines a debruijin index.
    #[derive(Debug, Clone, Hash, Copy, PartialEq, Eq, PartialOrd, Ord)]
    pub struct Lvl(usize);

    impl Lvl {
        /// Transforms a level into a debruijin index.
        pub fn into_ix(&self, Lvl(lvl0): Lvl) -> Ix {
            let Lvl(lvl1) = self;

            Ix(lvl1 - lvl0 - 1)
        }
    }

    impl std::ops::Add<usize> for Lvl {
        type Output = Self;

        fn add(self, rhs: usize) -> Self::Output {
            Self(self.0 + rhs)
        }
    }

    impl std::ops::AddAssign<usize> for Lvl {
        fn add_assign(&mut self, rhs: usize) {
            self.0 += rhs
        }
    }

    impl<S: state::State<Meta = ()>> Element<S> for Lvl {
        fn meta(&self) -> &S::Meta {
            &()
        }
    }

    /// Defines a debruijin index.
    #[derive(Debug, Clone, Hash, Copy, PartialEq, Eq, PartialOrd, Ord)]
    pub struct Ix(pub usize);

    impl std::ops::Add<usize> for Ix {
        type Output = Self;

        fn add(self, rhs: usize) -> Self::Output {
            Self(self.0 + rhs)
        }
    }

    impl std::ops::AddAssign<usize> for Ix {
        fn add_assign(&mut self, rhs: usize) {
            self.0 += rhs
        }
    }

    impl<S: state::State<Meta = ()>> Element<S> for Ix {
        fn meta(&self) -> &S::Meta {
            &()
        }
    }
}

/// Typed state, it's the state of the syntax tree when it's typed.
pub mod typed {
    use super::*;

    /// A type info. It contains if the type is an enum or a struct, or maybe
    /// a function type.
    #[derive(Debug, Clone)]
    pub enum TypeInfo {}

    #[derive(Debug, Clone)]
    pub struct TypedMeta {
        pub type_info: TypeInfo,
        pub type_term: Option<Term<state::Quoted>>,
        pub type_value: crate::elab::Value,
        pub location: Location,
    }

    /// A name access.
    #[derive(Debug, Clone)]
    pub struct Reference {
        pub definition: Rc<Definition<state::Resolved>>,
        pub meta: TypedMeta,
    }

    impl<S: state::State<Meta = TypedMeta>> Element<S> for Reference {
        fn meta(&self) -> &TypedMeta {
            &self.meta
        }
    }
}

/// A definition. It has a text, and a location.
#[derive(Default, Debug, Clone, Hash)]
pub struct Definition<S: state::State> {
    pub text: String,
    pub meta: S::Meta,
}

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
pub trait Element<S: state::State>: Debug + Clone {
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

impl<S: state::State> Element<S> for Fun<S> {
    fn meta(&self) -> &S::Meta {
        &self.meta
    }
}

/// A term. It can be an integer, a variable, an application, or a pi type.
///
/// It's the base of the abstract syntax tree.
#[derive(Clone)]
pub enum Term<S: state::State> {
    Error(Error<S>),
    Universe(Universe<S>),
    Int(Int<S>),
    Str(Str<S>),
    Group(Box<Term<S>>),
    Elim(Elim<S>),
    Fun(Fun<S>),
    Apply(Apply<S>),
    Pi(Pi<S>),
    Reference(S::Reference),
    Hole(Hole<S>),
}

impl<S: state::State> Default for Term<S>
where
    S::Meta: Default,
{
    fn default() -> Self {
        Self::Hole(Hole::default())
    }
}

impl<S: state::State> Term<S> {
    /// Removes the group from the term. It's useful to pattern
    /// match agains't group.
    pub fn unwrap(self) -> Self {
        match self {
            Self::Group(arg0) => *arg0.clone(),
            _ => self,
        }
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
