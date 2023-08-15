#![feature(box_patterns)]

/// The abstract syntax tree for the language.
pub mod ast {
    use std::fmt::Debug;

    /// The ast GAT state.
    pub mod state {
        use super::*;

        /// Represents the syntax state, if it's resolved, or just parsed, it's useful for not
        /// having to redeclare the same types.
        pub trait State: Debug {
            type Reference: Element;
            type Definition: Debug;
        }

        /// Represents the parsed state, it's the state of the syntax tree when it's just parsed.
        #[derive(Debug)]
        pub struct Quoted;

        impl State for Quoted {
            type Definition = parsed::Definition;
            type Reference = parsed::Reference;
        }

        /// Represents the resolved state, it's the state of the syntax tree when it's resolved.
        #[derive(Debug)]
        pub struct Resolved;

        impl State for Resolved {
            type Definition = resolved::Definition;
            type Reference = resolved::Reference;
        }
    }

    #[derive(Debug)]
    pub enum DefinitionKind {
        Constructor,
        Inductive,
        Binding,
    }

    pub mod parsed {
        use super::*;

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

        /// A name access.
        #[derive(Debug)]
        pub struct Reference {
            pub text: String,
            pub location: Location,
        }

        impl Element for Reference {
            fn location(&self) -> &Location {
                &self.location
            }
        }
    }

    pub mod resolved {
        use super::*;

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

        /// A name access.
        #[derive(Debug)]
        pub struct Reference {
            pub text: String,
            pub location: Location,
        }

        impl Element for Reference {
            fn location(&self) -> &Location {
                &self.location
            }
        }
    }

    #[derive(Debug, Hash, PartialEq, Eq, Clone)]
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
    pub struct Constructor<S: state::State> {
        pub name: S::Definition,
        pub type_rep: Term<S>,
        pub location: Location,
    }

    impl<S: state::State> Element for Constructor<S> {
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
    pub struct Inductive<S: state::State> {
        pub doc_strings: Vec<DocString>,
        pub name: S::Definition,
        pub parameters: Vec<Variable<S>>,
        pub constructors: Vec<Constructor<S>>,
        pub location: Location,
    }

    impl<S: state::State> Element for Inductive<S> {
        fn location(&self) -> &Location {
            &self.location
        }
    }

    impl<S: state::State> Declaration for Inductive<S> {
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
    pub struct Binding<S: state::State> {
        pub doc_strings: Vec<DocString>,
        pub name: S::Definition,
        pub value: Vec<Term<S>>,
        pub location: Location,
    }

    impl<S: state::State> Element for Binding<S> {
        fn location(&self) -> &Location {
            &self.location
        }
    }

    impl<S: state::State> Declaration for Binding<S> {
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
    pub enum CommandKind<S: state::State> {
        Eval(Term<S>),
        Type(Term<S>),
        Import(Identifier),
        Elim(S::Definition, Vec<S::Definition>),
    }

    /// Represents a command downgrade from statement, just like @eval and @type.
    ///
    /// ## Examples
    ///
    /// ```haskell
    /// @eval 10
    /// ```
    #[derive(Debug)]
    pub struct Downgrade<S: state::State> {
        pub kind: CommandKind<S>,
        pub location: Location,
    }

    impl<S: state::State> Element for Downgrade<S> {
        fn location(&self) -> &Location {
            &self.location
        }
    }

    /// A statement. It can be an inductive type, or a downgrade.
    #[derive(Debug)]
    pub enum Stmt<S: state::State> {
        Error(Error),

        /// An inductive type is a statement that introduces a new inductive
        /// type.
        Inductive(Inductive<S>),

        /// A binding is a statement that introduces a new binding.
        Binding(Binding<S>),

        /// A downgrade is a statement that downgrades a type to a term.
        ///
        /// For example, `nat` is a type, but `nat` is also a term.
        Downgrade(Downgrade<S>),
    }

    impl<S: state::State> Element for Stmt<S> {
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
    pub struct Pattern<S: state::State> {
        pub definition: S::Reference,
        pub arguments: Vec<S::Definition>,
        pub location: Location,
    }

    impl<S: state::State> Element for Pattern<S> {
        fn location(&self) -> &Location {
            &self.location
        }
    }

    /// A case for eliminator.
    #[derive(Debug)]
    pub struct Case<S: state::State> {
        pub patterns: Vec<Pattern<S>>,
        pub value: Box<Term<S>>,
        pub location: Location,
    }

    impl<S: state::State> Element for Case<S> {
        fn location(&self) -> &Location {
            &self.location
        }
    }

    /// An eliminator. It has a list of patterns, and a location.
    ///
    /// It's a simple eliminator for inductive types.
    #[derive(Debug)]
    pub struct Elim<S: state::State> {
        pub patterns: Vec<Pattern<S>>,
        pub location: Location,
    }

    impl<S: state::State> Element for Elim<S> {
        fn location(&self) -> &Location {
            &self.location
        }
    }

    /// A variable. It has a name and a location.
    #[derive(Debug)]
    pub struct Variable<S: state::State> {
        /// The name of the variable. The idea of the [`Option`] type, is when
        /// we have a binder like `_`, which is a placeholder for a variable for
        /// which we don't care about the name.
        pub text: Option<S::Definition>,

        /// The type of the variable. If it's in an implicit argument position,
        /// it will fallback to the type `type`.
        pub type_repr: Option<Box<Term<S>>>,

        /// If the variable binds something implicitly or explicitly.
        pub icit: Icit,

        /// The location of the variable in the source code.
        pub location: Location,
    }

    impl<S: state::State> Element for Variable<S> {
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
    pub struct Apply<S: state::State> {
        pub callee: Box<Term<S>>,
        pub arguments: Vec<Variable<S>>,
        pub location: Location,
    }

    impl<S: state::State> Element for Apply<S> {
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
    pub struct Fun<S: state::State> {
        pub arguments: Vec<Variable<S>>,
        pub value: Box<Term<S>>,
        pub location: Location,
    }

    impl<S: state::State> Element for Fun<S> {
        fn location(&self) -> &Location {
            &self.location
        }
    }

    /// A pi type. It has a name, a domain, and a codomain.
    ///
    /// ## Examples
    ///
    /// For example, the pi type `\pi (x : A) -> B`, where `x` is the name, `A` is the
    /// domain, and `B` is the codomain.
    ///
    /// The pi type can be an arrow type too, like `A -> B`
    ///
    /// ## Implicit pi types
    ///
    /// Pi types can be implicit too, like `{%x : A} -> B`, where `x` is the name,
    /// `A` is the domain, and `B` is the codomain.
    #[derive(Debug)]
    pub struct Pi<S: state::State> {
        pub icit: Icit,
        pub name: Variable<S>,
        pub domain: Box<Term<S>>,
        pub codomain: Box<Term<S>>,
        pub location: Location,
    }

    impl<S: state::State> Element for Pi<S> {
        fn location(&self) -> &Location {
            &self.location
        }
    }

    /// A term. It can be an integer, a variable, an application, or a pi type.
    ///
    /// It's the base of the abstract syntax tree.
    #[derive(Debug)]
    pub enum Term<S: state::State> {
        Error(Error),
        Universe(Universe),
        Int(Int),
        Elim(Elim<S>),
        Fun(Fun<S>),
        Variable(Variable<S>),
        Apply(Apply<S>),
        Pi(Pi<S>),
        Reference(S::Reference),
        Hole(Hole),
    }

    impl<S: state::State> Element for Term<S> {
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
                Term::Reference(atom) => atom.location(),
            }
        }
    }
}

/// Error reporting module to the language.
pub mod failure {
    use super::*;

    pub const UNRESOLVED_REFERENCE: &str = "unresolved-reference";
    pub const DUPLICATED_DEFINITION: &str = "duplicated-definition";

    /// The identification of the diagnostic, it's a list of strings.
    ///
    /// ## Examples
    ///
    /// For example, if we have, "unresolved-variable", "name-of-var", it will group all the
    /// errors with the same diagnostic id.
    #[derive(Debug, Hash, PartialEq, Eq, Clone)]
    pub struct DiagId(pub Vec<String>);

    /// The diagnostic level, it's the severity of the diagnostic.
    #[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
    pub enum DiagLevel {
        /// The diagnostic is a warning.
        Warning,

        /// The diagnostic is an error.
        Error,
    }

    /// The diagnostic kind, it's the severity of the diagnostic.
    #[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
    pub enum DiagKind {
        Parser,
        Resolver,
        Typer,
    }

    /// The diagnostic message to be presented to the user.
    #[derive(Debug, Hash, PartialEq, Eq, Clone)]
    pub struct Failure {
        pub kind: DiagKind,
        pub level: DiagLevel,
        pub id: DiagId,
        pub message: String,
        pub location: ast::Location,
    }

    /// Creates a new diagnostic id for types with `Display`.
    #[macro_export]
    macro_rules! diag_id {
        ($($x:expr),+ $(,)?) => {
            $crate::failure::DiagId(vec![$($x.to_string()),+])
        };
    }
}

/// The tokenization part of the language.
pub mod lexer {
    use logos::Logos;

    /// The base structure of the lexer.
    #[derive(Logos, Debug, PartialEq, Eq)]
    #[logos(skip r"[ \t\n\f]+")]
    pub enum Token {
        // SECTION: Commands
        #[token("@import")]
        CmdImport,

        #[token("@eval")]
        CmdEval,

        #[token("@type")]
        CmdType,

        #[token("@elim")]
        CmdElim,

        // SECTION: Keywords
        #[token("\\module")]
        KwModule,

        #[token("\\inductive")]
        KwInductive,

        #[token("\\type")]
        KwType,

        #[token("\\elim")]
        KwElim,

        #[token("\\of")]
        KwOf,

        #[token("\\fun")]
        KwFun,

        // SECTION: Control
        #[token("(")]
        LParen,

        #[token(")")]
        RParen,

        #[token("{")]
        LBrace,

        #[token("}")]
        RBrace,

        // SECTION: Symbols
        #[token(".")]
        SmDot,

        #[token(",")]
        SmComma,

        #[token(":")]
        SmColon,

        #[token("=")]
        SmEq,

        #[token("->")]
        SmArr,

        #[token("=>")]
        SmDoubleArr,

        // SECTION: Values
        #[regex("[a-zA-Z*/+-_^&$@!][a-zA-Z0-9_*/+-^&$@!]*")]
        Constructor,

        #[regex("\"\\.*\"")]
        String,

        #[regex("\\d", priority = 2)]
        Int,
    }
}

fn main() {
    println!("Hello, world!");
}
