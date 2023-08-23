use std::{fmt::Debug, marker::PhantomData, rc::Rc};

/// File definition, it contains all the statements,
/// the module name, and a base location for it as anchor
/// for the statements.
#[derive(Default, Debug, Clone)]
pub struct File<S: state::State> {
    pub name: String,
    pub stmts: Vec<Stmt<S>>,
    pub location: S::Location,
}

/// The ast GAT state. It's more likelly a Tree That Grow, with the
/// rust features, but that's it.
pub mod state {
    use std::rc::Rc;

    use super::*;

    /// Represents the syntax state, if it's resolved, or just parsed, it's useful for not
    /// having to redeclare the same types.
    pub trait State: Default + Debug + Clone {
        type NameSet: Debug + Clone;
        type Arguments: Debug + Clone;
        type Parameters: Debug + Clone;
        type Import: Element<Self>;
        type Reference: Element<Self>;
        type Definition: Element<Self>;
        type Location: Debug + Clone;
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
        type Location = Location;
    }

    /// Represents the resolved state, it's the state of the syntax tree when it's resolved.
    #[derive(Default, Debug, Clone)]
    pub struct Resolved;

    impl State for Resolved {
        type NameSet = Self::Definition;
        type Arguments = Vec<Term<Resolved>>;
        type Parameters = Vec<Self::Definition>;
        type Definition = Rc<resolved::Definition<Resolved>>;
        type Reference = resolved::Reference;
        type Import = !;
        type Location = Location;
    }

    /// Represents the resolved state, it's the state of the syntax tree when it's resolved.
    #[derive(Default, Debug, Clone)]
    pub struct Quoted;

    impl State for Quoted {
        type NameSet = Self::Definition;
        type Parameters = Self::Definition;
        type Arguments = Box<Term<Quoted>>;
        type Definition = resolved::Definition<Quoted>;
        type Reference = quoted::Reference;
        type Import = !;
        type Location = ();
    }
}

impl<S: state::State> Element<S> for ! {
    fn location(&self) -> &S::Location {
        unreachable!()
    }
}

impl<S: state::State, T: Element<S>> Element<S> for Rc<T> {
    fn location(&self) -> &S::Location {
        self.as_ref().location()
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
        pub location: Location,
    }

    impl<S: state::State<Location = Location>> Element<S> for Reference {
        fn location(&self) -> &Location {
            &self.location
        }
    }

    /// Imports a name temporally until it's
    /// propertly resolved
    #[derive(Debug, Clone)]
    pub struct Import {
        pub text: String,
        pub location: Location,
    }

    impl<S: state::State<Location = Location>> Element<S> for Import {
        fn location(&self) -> &Location {
            &self.location
        }
    }
}

/// Resolved state, it's the state of the syntax tree when it's resolved.
pub mod resolved {
    use std::rc::Rc;

    use super::*;

    /// A definition. It has a text, and a location.
    #[derive(Default, Debug, Clone, Hash)]
    pub struct Definition<S: state::State> {
        pub text: String,
        pub location: S::Location,
    }

    impl<S: state::State> Definition<S>
    where
        S::Location: Default,
    {
        /// Creates a new instance of [`Definition`].
        pub fn new(text: String) -> Self {
            Self {
                text,
                location: S::Location::default(),
            }
        }
    }

    impl<S: state::State> Element<S> for Definition<S> {
        fn location(&self) -> &S::Location {
            &self.location
        }
    }

    /// A name access.
    #[derive(Debug, Clone)]
    pub struct Reference {
        pub definition: Rc<Definition<state::Resolved>>,
        pub location: Location,
    }

    impl<S: state::State<Location = Location>> Element<S> for Reference {
        fn location(&self) -> &Location {
            &self.location
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

    impl<S: state::State<Location = ()>> Element<S> for Reference {
        fn location(&self) -> &S::Location {
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

    impl<S: state::State<Location = ()>> Element<S> for Lvl {
        fn location(&self) -> &S::Location {
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

    impl<S: state::State<Location = ()>> Element<S> for Ix {
        fn location(&self) -> &S::Location {
            &()
        }
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
    fn location(&self) -> &S::Location;
}

/// Error node, it does contains an error.
#[derive(Debug, Clone)]
pub struct Error<S: state::State> {
    /// The error message.
    pub message: String,

    /// The original text that originated the error.
    pub full_text: String,

    /// The location of the error.
    pub location: S::Location,
}

/// Represents a recovery from an error.
pub trait Recovery<S: state::State> {
    /// Creates a new instance of [`Self`] when it's an error, it's
    /// useful for enums
    fn recover_from_error(error: Error<S>) -> Self;
}

impl<S: state::State> Element<S> for Error<S> {
    fn location(&self) -> &S::Location {
        &self.location
    }
}

#[derive(Debug, Clone)]
pub struct Identifier<S: state::State> {
    pub text: String,
    pub location: S::Location,
}

impl<S: state::State> Element<S> for Identifier<S> {
    fn location(&self) -> &S::Location {
        &self.location
    }
}

/// Int is a integer value like `0`, `1`, `2`, etc.
#[derive(Default, Debug, Clone)]
pub struct Str<S: state::State> {
    pub value: String,

    /// The location of the source in the source code.
    pub location: S::Location,
}

impl<S: state::State> Element<S> for Str<S> {
    fn location(&self) -> &S::Location {
        &self.location
    }
}

/// Int is a integer value like `0`, `1`, `2`, etc.
#[derive(Default, Debug, Clone)]
pub struct Int<S: state::State> {
    /// The value of the integer.
    pub value: isize,

    /// The location of the integer in the source code.
    pub location: S::Location,
}

impl<S: state::State> Element<S> for Int<S> {
    fn location(&self) -> &S::Location {
        &self.location
    }
}

/// A pattern. It has a definition, a list of arguments, and a location.
///
/// It's a simple pattern for eliminator.
#[derive(Default, Debug, Clone)]
pub struct Pattern<S: state::State> {
    pub definition: S::Reference,
    pub arguments: Vec<S::Definition>,
    pub location: S::Location,
}

impl<S: state::State> Element<S> for Pattern<S> {
    fn location(&self) -> &S::Location {
        &self.location
    }
}

/// A case for eliminator.
#[derive(Debug, Clone)]
pub struct Case<S: state::State> {
    pub patterns: Vec<Pattern<S>>,
    pub value: Box<Term<S>>,
    pub location: S::Location,
}

impl<S: state::State> Element<S> for Case<S> {
    fn location(&self) -> &S::Location {
        &self.location
    }
}

/// An eliminator. It has a list of patterns, and a location.
///
/// It's a simple eliminator for inductive types.
#[derive(Debug, Clone)]
pub struct Elim<S: state::State> {
    pub patterns: Vec<Pattern<S>>,
    pub location: S::Location,
}

impl<S: state::State> Element<S> for Elim<S> {
    fn location(&self) -> &S::Location {
        &self.location
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
    pub location: S::Location,
}

impl<S: state::State> Element<S> for Apply<S> {
    fn location(&self) -> &S::Location {
        &self.location
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
    pub location: S::Location,
}

impl<S: state::State> Element<S> for Fun<S> {
    fn location(&self) -> &S::Location {
        &self.location
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
    S::Location: Default,
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
    fn location(&self) -> &S::Location {
        match self {
            Term::Error(error) => error.location(),
            Term::Universe(universe) => universe.location(),
            Term::Int(int) => int.location(),
            Term::Str(str) => str.location(),
            Term::Elim(elim) => elim.location(),
            Term::Group(group) => group.location(),
            Term::Fun(fun) => fun.location(),
            Term::Apply(apply) => apply.location(),
            Term::Pi(pi) => pi.location(),
            Term::Hole(hole) => hole.location(),
            Term::Reference(atom) => atom.location(),
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
