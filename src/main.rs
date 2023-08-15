#![feature(box_patterns)]

/// The abstract syntax tree for the language.
pub mod ast {
    use std::fmt::Debug;

    /// The ast GAT state.
    pub mod state {
        /// Represents the syntax state, if it's resolved, or just parsed, it's useful for not
        /// having to redeclare the same types.
        pub trait State {}

        /// Represents the parsed state, it's the state of the syntax tree when it's just parsed.
        #[derive(Debug)]
        pub struct Quoted;

        impl State for Quoted {}

        /// Represents the resolved state, it's the state of the syntax tree when it's resolved.
        #[derive(Debug)]
        pub struct Resolved;

        impl State for Resolved {}
    }

    #[derive(Debug)]
    pub struct Location {
        pub from: usize,
        pub to: usize,
        pub file: String,
    }

    /// An element. It can be a declaration, or a term.
    pub trait Element: Debug {
        fn location(&self) -> &Location;
    }

    /// Error node, it does contains an error.
    #[derive(Debug)]
    pub struct Error {
        /// The error message.
        pub message: String,

        /// The original text that originated the error.
        pub full_text: String,

        /// The location of the error.
        pub location: Location,
    }

    impl Element for Error {
        fn location(&self) -> &Location {
            &self.location
        }
    }

    #[derive(Debug)]
    pub enum DefinitionKind {
        Constructor,
        Inductive,
        Binding,
    }

    /// A definition. It has a text, and a location.
    #[derive(Debug)]
    pub struct Definition {
        pub text: String,
        pub location: Location,
    }

    impl Element for Definition {
        fn location(&self) -> &Location {
            &self.location
        }
    }

    /// A constructor for an inductive type. It has a name and a type.
    ///
    /// ## Examples
    ///
    /// For example, the constructors:
    /// - `Zero : nat`, and
    /// - `Succ : nat -> nat`
    ///
    /// Are constructors for the inductive type `nat`.
    #[derive(Debug)]
    pub struct Constructor {
        pub name: Definition,
        pub type_rep: Term,
        pub location: Location,
    }

    impl Element for Constructor {
        fn location(&self) -> &Location {
            &self.location
        }
    }

    /// A documentation string. It has a list of strings.
    ///
    /// It's used to document declarations.
    #[derive(Debug)]
    pub struct DocString {
        pub full_text: String,
        pub text: String,
        pub location: Location,
    }

    impl Element for DocString {
        fn location(&self) -> &Location {
            &self.location
        }
    }

    /// A declaration. It can be an inductive type, or a downgrade.
    ///
    /// It's an wrapper for different types of declarations.
    pub trait Declaration: Element {
        /// The documentation of the declaration.
        fn doc_strings(&self) -> &[DocString];
    }

    /// An inductive type. It has a name, a list of parameters, and a list of
    /// constructors.
    ///
    /// It's the same as algebraic data types, or generalized algebraic data
    /// types.
    ///
    /// ## Examples
    ///
    /// For example, the inductive type `nat`:
    ///
    /// ```haskell
    /// \inductive nat  
    ///   Zero : nat,
    ///   Succ : nat -> nat.
    /// ```
    #[derive(Debug)]
    pub struct Inductive {
        pub doc_strings: Vec<DocString>,
        pub name: Definition,
        pub parameters: Vec<Variable>,
        pub constructors: Vec<Constructor>,
        pub location: Location,
    }

    impl Element for Inductive {
        fn location(&self) -> &Location {
            &self.location
        }
    }

    impl Declaration for Inductive {
        fn doc_strings(&self) -> &[DocString] {
            &self.doc_strings
        }
    }

    /// Defines a binding. It has a name, a list of doc strings, and a value.
    ///
    /// ## Examples
    /// 
    /// ```haskell
    /// -- | Defines natural numbers without induction feature, it's
    /// -- like functions in dependent langauges.
    /// nat = %n: type -> (n -> n) -> n -> n.
    ///
    /// -- | Defines the zero constructor
    /// Zero = \fun _, _, zero
    ///   zero.
    ///
    /// -- | Defines the succ constructor
    /// Succ = \fun n, N, succ, _
    ///   (n N succ zero).
    /// ```
    #[derive(Debug)]
    pub struct Binding {
        pub doc_strings: Vec<DocString>,
        pub name: Definition,
        pub value: Vec<Term>,
        pub location: Location,
    }

    impl Element for Binding {
        fn location(&self) -> &Location {
            &self.location
        }
    }

    impl Declaration for Binding {
        fn doc_strings(&self) -> &[DocString] {
            &self.doc_strings
        }
    }

    #[derive(Debug)]
    pub struct Identifier {
        pub text: String,
        pub location: Location,
    }

    impl Element for Identifier {
        fn location(&self) -> &Location {
            &self.location
        }
    }

    /// Represents a command downgrade from statement, just like @eval and @type.
    #[derive(Debug)]
    pub enum CommandKind {
        Eval(Term),
        Type(Term),
        Import(Identifier),
        Elim(Definition, Vec<Definition>),
    }

    /// Represents a command downgrade from statement, just like @eval and @type.
    ///
    /// ## Examples
    ///
    /// ```haskell
    /// @eval 10
    /// ```
    #[derive(Debug)]
    pub struct Downgrade {
        pub kind: CommandKind,
        pub location: Location,
    }

    impl Element for Downgrade {
        fn location(&self) -> &Location {
            &self.location
        }
    }

    /// A statement. It can be an inductive type, or a downgrade.
    #[derive(Debug)]
    pub enum Stmt {
        Error(Error),

        /// An inductive type is a statement that introduces a new inductive
        /// type.
        Inductive(Inductive),

        /// A binding is a statement that introduces a new binding.
        Binding(Binding),

        /// A downgrade is a statement that downgrades a type to a term.
        ///
        /// For example, `nat` is a type, but `nat` is also a term.
        Downgrade(Downgrade),
    }

    impl Element for Stmt {
        fn location(&self) -> &Location {
            match self {
                Stmt::Error(error) => &error.location,
                Stmt::Inductive(inductive) => &inductive.location,
                Stmt::Binding(binding) => &binding.location,
                Stmt::Downgrade(downgrade) => downgrade.location(),
            }
        }
    }

    /// Type of a type. It has a location.
    #[derive(Debug)]
    pub struct Universe {
        /// The location of the integer in the source code.
        pub location: Location,
    }

    impl Element for Universe {
        fn location(&self) -> &Location {
            &self.location
        }
    }

    /// A name access.
    #[derive(Debug)]
    pub struct Atom {
        pub text: String,

        /// The location of the integer in the source code.
        pub location: Location,
    }

    impl Element for Atom {
        fn location(&self) -> &Location {
            &self.location
        }
    }

    /// A hole. It has a location.
    #[derive(Debug)]
    pub struct Hole {
        /// The location of the integer in the source code.
        pub location: Location,
    }

    impl Element for Hole {
        fn location(&self) -> &Location {
            &self.location
        }
    }

    /// Int is a integer value like `0`, `1`, `2`, etc.
    #[derive(Debug)]
    pub struct Int {
        /// The value of the integer.
        pub value: isize,

        /// The location of the integer in the source code.
        pub location: Location,
    }

    impl Element for Int {
        fn location(&self) -> &Location {
            &self.location
        }
    }

    /// A pattern. It has a definition, a list of arguments, and a location.
    ///
    /// It's a simple pattern for eliminator.
    #[derive(Debug)]
    pub struct Pattern {
        pub definition: Definition,
        pub arguments: Vec<Identifier>,
        pub location: Location,
    }

    impl Element for Pattern {
        fn location(&self) -> &Location {
            &self.location
        }
    }

    /// A case for eliminator.
    #[derive(Debug)]
    pub struct Case {
        pub patterns: Vec<Pattern>,
        pub value: Box<Term>,
        pub location: Location,
    }

    impl Element for Case {
        fn location(&self) -> &Location {
            &self.location
        }
    }

    /// An eliminator. It has a list of patterns, and a location.
    ///
    /// It's a simple eliminator for inductive types.
    #[derive(Debug)]
    pub struct Elim {
        pub patterns: Vec<Pattern>,
        pub location: Location,
    }

    impl Element for Elim {
        fn location(&self) -> &Location {
            &self.location
        }
    }

    /// A variable. It has a name and a location.
    #[derive(Debug)]
    pub struct Variable {
        /// The name of the variable. The idea of the [`Option`] type, is when
        /// we have a binder like `_`, which is a placeholder for a variable for
        /// which we don't care about the name.
        pub text: Option<String>,

        /// The type of the variable. If it's in an implicit argument position,
        /// it will fallback to the type `type`.
        pub type_repr: Option<Box<Term>>,

        /// If the variable binds something implicitly or explicitly.
        pub icit: Icit,

        /// The location of the variable in the source code.
        pub location: Location,
    }

    impl Element for Variable {
        fn location(&self) -> &Location {
            &self.location
        }
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub enum Icit {
        /// Explicit binder `{%x : A}`.
        Expl,

        /// Implicit binder `%x : A`.
        Impl,
    }

    /// An application term. It has a list of arguments, and a function.
    ///
    /// ## Examples
    ///
    /// ```haskell
    /// (a b c)
    /// ```
    #[derive(Debug)]
    pub struct Apply {
        pub callee: Box<Term>,
        pub arguments: Vec<Variable>,
        pub location: Location,
    }

    impl Element for Apply {
        fn location(&self) -> &Location {
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
    #[derive(Debug)]
    pub struct Fun {
        pub arguments: Vec<Variable>,
        pub value: Box<Term>,
        pub location: Location,
    }

    impl Element for Fun {
        fn location(&self) -> &Location {
            &self.location
        }
    }

    /// A pi type. It has a name, a domain, and a codomain.
    ///
    /// ## Examples
    ///
    /// For example, the pi type `%x : A -> B`, where `x` is the name, `A` is the
    /// domain, and `B` is the codomain.
    ///
    /// The pi type can be an arrow type too, like `A -> B`
    ///
    /// ## Implicit pi types
    ///
    /// Pi types can be implicit too, like `{%x : A} -> B`, where `x` is the name,
    /// `A` is the domain, and `B` is the codomain.
    #[derive(Debug)]
    pub struct Pi {
        pub icit: Icit,
        pub name: Variable,
        pub domain: Box<Term>,
        pub codomain: Box<Term>,
        pub location: Location,
    }

    impl Element for Pi {
        fn location(&self) -> &Location {
            &self.location
        }
    }

    /// A term. It can be an integer, a variable, an application, or a pi type.
    ///
    /// It's the base of the abstract syntax tree.
    #[derive(Debug)]
    pub enum Term {
        Error(Error),
        Universe(Universe),
        Int(Int),
        Elim(Elim),
        Fun(Fun),
        Variable(Variable),
        Apply(Apply),
        Pi(Pi),
        Atom(Atom),
        Hole(Hole),
    }

    impl Element for Term {
        fn location(&self) -> &Location {
            match self {
                Term::Error(error) => error.location(),
                Term::Universe(universe) => universe.location(),
                Term::Int(int) => int.location(),
                Term::Elim(elim) => elim.location(),
                Term::Fun(fun) => fun.location(),
                Term::Variable(variable) => variable.location(),
                Term::Apply(apply) => apply.location(),
                Term::Pi(pi) => pi.location(),
                Term::Hole(hole) => hole.location(),
                Term::Atom(atom) => atom.location(),
            }
        }
    }
}

fn main() {
    println!("Hello, world!");
}
