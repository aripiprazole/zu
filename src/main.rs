#![feature(box_patterns)]

/// The abstract syntax tree for the language.
pub mod ast {
    use std::fmt::Debug;

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
        pub name: Variable,
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
        fn doc_strings(&self) -> &Vec<String>;
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
    /// nat = \inductive
    /// | Zero : nat
    /// | Succ : nat -> nat.
    /// ```
    #[derive(Debug)]
    pub struct Inductive {
        pub doc_strings: Vec<DocString>,
        pub name: Variable,
        pub parameters: Vec<Variable>,
        pub constructors: Vec<Constructor>,
        pub location: Location,
    }

    impl Element for Inductive {
        fn location(&self) -> &Location {
            &self.location
        }
    }

    /// A statement. It can be an inductive type, or a downgrade.
    #[derive(Debug)]
    pub enum Stmt {
        /// An inductive type is a statement that introduces a new inductive
        /// type.
        Inductive(Inductive),

        /// A downgrade is a statement that downgrades a type to a term.
        ///
        /// For example, `nat` is a type, but `nat` is also a term.
        Downgrade(Term),
    }

    impl Element for Stmt {
        fn location(&self) -> &Location {
            match self {
                Stmt::Inductive(inductive) => &inductive.location,
                Stmt::Downgrade(downgrade) => downgrade.location(),
            }
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

    /// A variable. It has a name and a location.
    #[derive(Debug)]
    pub struct Variable {
        /// The name of the variable. The idea of the [`Option`] type, is when
        /// we have a binder like `_`, which is a placeholder for a variable for
        /// which we don't care about the name.
        pub text: Option<String>,

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
        Int(Int),
        Variable(Variable),
        Apply(Box<Term>, Vec<Term>),
        Pi(Pi),
    }

    impl Element for Term {
        fn location(&self) -> &Location {
            match self {
                Term::Int(int) => int.location(),
                Term::Variable(variable) => variable.location(),
                Term::Apply(apply, _) => apply.location(),
                Term::Pi(pi) => pi.location(),
            }
        }
    }
}

fn main() {
    println!("Hello, world!");
}
