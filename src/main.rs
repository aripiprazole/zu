#![feature(never_type)]
#![feature(box_patterns)]
#![feature(exhaustive_patterns)]

use clap::Parser;

/// The abstract syntax tree for the language.
pub mod ast {
    use std::{fmt::Debug, rc::Rc};

    /// File definition, it contains all the statements,
    /// the module name, and a base location for it as anchor
    /// for the statements.
    #[derive(Default, Debug, Clone)]
    pub struct File<S: state::State> {
        pub name: String,
        pub stmts: Vec<Stmt<S>>,
        pub location: Location,
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
            type Import: Element;
            type Reference: Element;
            type Definition: Element;
        }

        /// Represents the parsed state, it's the state of the syntax tree when it's just parsed.
        #[derive(Default, Debug, Clone)]
        pub struct Quoted;

        impl State for Quoted {
            type NameSet = Vec<Option<Self::Definition>>;
            type Definition = parsed::Reference;
            type Reference = parsed::Reference;
            type Import = parsed::Import;
        }

        /// Represents the resolved state, it's the state of the syntax tree when it's resolved.
        #[derive(Default, Debug, Clone)]
        pub struct Resolved;

        impl State for Resolved {
            type NameSet = Self::Definition;
            type Definition = Rc<resolved::Definition>;
            type Reference = resolved::Reference;
            type Import = !;
        }
    }

    impl Element for ! {
        fn location(&self) -> &Location {
            unreachable!()
        }
    }

    impl<T: Element> Element for Rc<T> {
        fn location(&self) -> &Location {
            self.as_ref().location()
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

        /// A name access.
        #[derive(Debug, Clone)]
        pub struct Reference {
            pub text: String,
            pub location: Location,
        }

        impl Element for Reference {
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

        impl Element for Import {
            fn location(&self) -> &Location {
                &self.location
            }
        }
    }

    pub mod resolved {
        use std::rc::Rc;

        use super::*;

        /// A definition. It has a text, and a location.
        #[derive(Debug, Clone)]
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
        #[derive(Debug, Clone)]
        pub struct Reference {
            pub definition: Rc<Definition>,
            pub location: Location,
        }

        impl Element for Reference {
            fn location(&self) -> &Location {
                &self.location
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
    pub trait Element: Debug + Clone {
        fn location(&self) -> &Location;
    }

    /// Error node, it does contains an error.
    #[derive(Debug, Clone)]
    pub struct Error {
        /// The error message.
        pub message: String,

        /// The original text that originated the error.
        pub full_text: String,

        /// The location of the error.
        pub location: Location,
    }

    /// Represents a recovery from an error.
    pub trait Recovery {
        /// Creates a new instance of [`Self`] when it's an error, it's
        /// useful for enums
        fn recover_from_error(error: Error) -> Self;
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
    #[derive(Debug, Clone)]
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
    #[derive(Debug, Clone)]
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
    pub trait Declaration<S: state::State> {
        /// The documentation of the declaration.
        fn doc_strings(&self) -> &[DocString];

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
    /// For example, the inductive type `nat`:
    ///
    /// ```haskell
    /// \inductive nat  
    ///   Zero : nat,
    ///   Succ : nat -> nat.
    /// ```
    #[derive(Debug, Clone)]
    pub struct Inductive<S: state::State> {
        pub doc_strings: Vec<DocString>,
        pub name: S::Definition,
        pub parameters: Vec<Domain<S>>,
        pub constructors: Vec<Constructor<S>>,
        pub location: Location,
    }

    impl<S: state::State> Element for Inductive<S> {
        fn location(&self) -> &Location {
            &self.location
        }
    }

    impl<S: state::State> Declaration<S> for Inductive<S> {
        fn doc_strings(&self) -> &[DocString] {
            &self.doc_strings
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
    /// -- | Defines natural numbers without induction feature, it's
    /// -- like functions in dependent langauges.
    /// nat = %n: type -> (n -> n) -> n -> n.
    ///
    /// -- | Defines the zero constructor
    /// Zero : _ = \fun _, _, zero
    ///   zero.
    ///
    /// -- | Defines the succ constructor
    /// Succ : _ = \fun n, N, succ, _
    ///   (n N succ zero).
    /// ```
    #[derive(Debug, Clone)]
    pub struct Binding<S: state::State> {
        pub doc_strings: Vec<DocString>,
        pub name: S::Definition,
        pub type_repr: Term<S>,
        pub value: Term<S>,
        pub location: Location,
    }

    impl<S: state::State> Element for Binding<S> {
        fn location(&self) -> &Location {
            &self.location
        }
    }

    impl<S: state::State> Declaration<S> for Binding<S> {
        fn doc_strings(&self) -> &[DocString] {
            &self.doc_strings
        }

        fn name(&self) -> &<S as state::State>::Definition {
            &self.name
        }
    }

    #[derive(Debug, Clone)]
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
    ///
    /// ## Examples
    ///
    /// ```haskell
    /// @eval 10
    /// ```
    #[derive(Debug, Clone)]
    pub struct ElimDef<S: state::State> {
        pub inductive: S::Definition,
        pub constructors: Vec<S::Definition>,
        pub location: Location,
    }

    impl<S: state::State> Element for ElimDef<S> {
        fn location(&self) -> &Location {
            &self.location
        }
    }

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
        pub location: Location,
    }

    impl<S: state::State> Element for Type<S> {
        fn location(&self) -> &Location {
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
        pub location: Location,
    }

    impl<S: state::State> Element for Eval<S> {
        fn location(&self) -> &Location {
            &self.location
        }
    }

    /// A statement. It can be an inductive type, or a downgrade.
    #[derive(Clone)]
    pub enum Stmt<S: state::State> {
        Error(Error),

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

        /// Defines the eliminator for an inductive type.
        ElimDef(ElimDef<S>),
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
                Self::ElimDef(arg0) => arg0.fmt(f),
                Self::Import(arg0) => arg0.fmt(f),
            }
        }
    }

    impl<S: state::State> Recovery for Stmt<S> {
        fn recover_from_error(error: Error) -> Self {
            Stmt::Error(error)
        }
    }

    impl<S: state::State> Element for Stmt<S> {
        fn location(&self) -> &Location {
            match self {
                Stmt::Error(error) => &error.location,
                Stmt::Inductive(inductive) => &inductive.location,
                Stmt::Binding(binding) => &binding.location,
                Stmt::Eval(downgrade) => &downgrade.location,
                Stmt::Type(downgrade) => &downgrade.location,
                Stmt::ElimDef(downgrade) => &downgrade.location,
                Stmt::Import(downgrade) => downgrade.location(),
            }
        }
    }

    /// Type of a type. It has a location.
    #[derive(Debug, Clone)]
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
    #[derive(Debug, Clone)]
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
    #[derive(Debug, Clone)]
    pub struct Str {
        pub value: String,

        /// The location of the source in the source code.
        pub location: Location,
    }

    impl Element for Str {
        fn location(&self) -> &Location {
            &self.location
        }
    }

    /// Int is a integer value like `0`, `1`, `2`, etc.
    #[derive(Debug, Clone)]
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
    #[derive(Debug, Clone)]
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
    #[derive(Debug, Clone)]
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
    #[derive(Debug, Clone)]
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
    #[derive(Debug, Clone)]
    pub struct Domain<S: state::State> {
        /// The name of the variable. The idea of the [`Option`] type, is when
        /// we have a binder like `_`, which is a placeholder for a variable for
        /// which we don't care about the name.
        pub text: S::NameSet,

        /// The type of the variable. If it's in an implicit argument position,
        /// it will fallback to the type `type`.
        pub type_repr: Box<Term<S>>,

        /// If the variable binds something implicitly or explicitly.
        pub icit: Icit,

        /// The location of the variable in the source code.
        pub location: Location,
    }

    impl<S: state::State> Element for Domain<S> {
        fn location(&self) -> &Location {
            &self.location
        }
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub enum Icit {
        /// Explicit binder `(x : A)`.
        Expl,

        /// Implicit binder `{x : A}`.
        Impl,
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
        pub arguments: Vec<Term<S>>,
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
    #[derive(Debug, Clone)]
    pub struct Fun<S: state::State> {
        pub arguments: Vec<S::Definition>,
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
    #[derive(Debug, Clone)]
    pub struct Pi<S: state::State> {
        pub icit: Icit,
        pub domain: Domain<S>,
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
    #[derive(Clone)]
    pub enum Term<S: state::State> {
        Error(Error),
        Universe(Universe),
        Int(Int),
        Str(Str),
        Group(Box<Term<S>>),
        Elim(Elim<S>),
        Fun(Fun<S>),
        Apply(Apply<S>),
        Pi(Pi<S>),
        Reference(S::Reference),
        Hole(Hole),
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

    impl<S: state::State> Recovery for Term<S> {
        fn recover_from_error(error: Error) -> Self {
            Term::Error(error)
        }
    }

    impl<S: state::State> Element for Term<S> {
        fn location(&self) -> &Location {
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
}

/// Parser LALRPOP mod.
pub mod parser {
    use lalrpop_util::lalrpop_mod;

    use miette::{NamedSource, SourceSpan};
    pub use zu::*;

    lalrpop_mod! {
        #[allow(warnings)]
        /// The parsing module
        pub zu
    }

    /// The parsed file type.
    type FileQt = crate::ast::File<crate::ast::state::Quoted>;

    #[derive(miette::Diagnostic, thiserror::Error, Debug)]
    #[error("could not parse due the following errors")]
    #[diagnostic()]
    pub struct ParseError {
        // Note source code by no labels
        #[source_code]
        source_code: NamedSource,

        // The source code above is used for these errors
        #[related]
        related: Vec<InnerError>,
    }

    /// Inner specified error for the parser. It's useful
    /// to debug the parser.
    #[derive(miette::Diagnostic, thiserror::Error, Debug)]
    #[error("can't parse the file")]
    #[diagnostic()]
    pub enum InnerError {
        #[error("invalid token")]
        #[diagnostic(code(zu::invalid_token))]
        InvalidToken {
            #[label = "here"]
            err_span: SourceSpan,
        },

        #[error("unrecognized token, {}", fmt_expected(expected))]
        #[diagnostic(code(zu::unrecognized_token))]
        UnrecoginzedToken {
            #[label = "here"]
            err_span: SourceSpan,
            expected: Vec<String>,
        },

        #[error("expected token, but got eof, {}", fmt_expected(expected))]
        #[diagnostic(code(zu::expected_token))]
        ExpectedToken {
            #[label = "here"]
            err_span: SourceSpan,
            expected: Vec<String>,
        },

        #[error("extra token, {}", token)]
        #[diagnostic(code(zu::extra_token))]
        ExtraToken {
            #[label = "here"]
            err_span: SourceSpan,
            token: String,
        },
    }

    /// Format an expected token message, it's
    /// useful for the error message.
    fn fmt_expected(expected: &[String]) -> String {
        let mut f = String::new();
        if !expected.is_empty() {
            for (i, e) in expected.iter().enumerate() {
                let sep = match i {
                    0 => "expected one of",
                    _ if i < expected.len() - 1 => ",",
                    // Last expected message to be written
                    _ => " or",
                };
                f.push_str(&format!("{sep} {e}"));
            }
        }
        f
    }

    /// Parses or report the error.
    pub fn parse_or_report(filename: &str, text: &str) -> Result<FileQt, ParseError> {
        let mut errors = vec![];
        let ast = match FileParser::new().parse(&mut errors, filename, text) {
            Ok(ast) => ast,
            Err(error) => {
                // Build up the list with at least one recovery error.
                errors.push(lalrpop_util::ErrorRecovery {
                    dropped_tokens: vec![],
                    error,
                });
                Default::default()
            }
        };

        // If there's no error, so return normally the AST as nothing
        // had happened.
        if errors.is_empty() {
            return Ok(ast);
        }

        Err(ParseError {
            related: errors
                .into_iter()
                .map(|recovery| {
                    use lalrpop_util::ParseError::*;

                    match recovery.error {
                        InvalidToken { location } => InnerError::InvalidToken {
                            err_span: SourceSpan::from(location..location),
                        },
                        UnrecognizedEof { location, expected } => InnerError::ExpectedToken {
                            err_span: SourceSpan::from(location..location),
                            expected,
                        },
                        UnrecognizedToken { token, expected } => InnerError::UnrecoginzedToken {
                            err_span: SourceSpan::from(token.0..token.2),
                            expected,
                        },
                        ExtraToken { ref token } => InnerError::ExtraToken {
                            err_span: SourceSpan::from(token.0..token.2),
                            token: token.1.to_string(),
                        },
                        User { .. } => todo!(),
                    }
                })
                .collect(),
            source_code: NamedSource::new(filename, text.to_string()),
        })
    }
}

/// Resolver module. It does handles the imports and the references.
///
/// It's the second phase of the compiler.
pub mod resolver {
    use std::{collections::HashMap, rc::Rc};

    use fxhash::FxBuildHasher;
    use miette::{Context, IntoDiagnostic, NamedSource, SourceSpan};

    use crate::ast::{
        resolved::{Definition, Reference},
        state, Apply, Binding, Domain, Element, ElimDef, Eval, File, Fun, Hole, Location, Pi, Stmt,
        Term, Type,
    };

    type FileMap = HashMap<String, String>;

    #[derive(thiserror::Error, miette::Diagnostic, Debug)]
    pub enum InnerError {
        #[error(transparent)]
        #[diagnostic(transparent)]
        Import(#[from] UnresolvedImport),

        #[error(transparent)]
        #[diagnostic(transparent)]
        Definition(#[from] UnresolvedDefinition),

        #[error(transparent)]
        #[diagnostic(transparent)]
        LaterDefinition(#[from] LaterUnresolvedDefinition),
    }

    #[derive(thiserror::Error, miette::Diagnostic, Debug)]
    #[diagnostic(code(zu::resolution_failure))]
    #[error("can't resolve the files")]
    pub struct ResolutionFailure {
        // The source code above is used for these errors
        #[related]
        related: Vec<InnerError>,
    }

    #[derive(thiserror::Error, miette::Diagnostic, Debug)]
    #[diagnostic(code(zu::unresolved_definition), help("maybe add an import for it?"))]
    #[error("unresolved definition: {module}")]
    pub struct UnresolvedDefinition {
        /// The name of the import.
        pub module: String,

        /// The source code of the import.
        #[source_code]
        source_code: NamedSource,

        /// The location of the unresolved import.
        #[label = "here"]
        span: SourceSpan,
    }

    #[derive(thiserror::Error, miette::Diagnostic, Debug)]
    #[diagnostic(
        code(zu::later_unresolved_declaration),
        help("maybe move this declaration")
    )]
    #[error("unresolved definition: {module} in the current scope")]
    pub struct LaterUnresolvedDefinition {
        /// The name of the import.
        pub module: String,

        /// The source code of the import.
        #[source_code]
        source_code: NamedSource,

        #[label = "here is the reference"]
        span: SourceSpan,

        #[label = "here is the declaration"]
        declaration_span: SourceSpan,
    }

    #[derive(thiserror::Error, miette::Diagnostic, Debug)]
    #[diagnostic(code(zu::unresolved_import), help("try to import the file in the cli"))]
    #[error("unresolved import: {module}")]
    pub struct UnresolvedImport {
        /// The name of the import.
        pub module: String,

        /// The source code of the import.
        #[source_code]
        source_code: NamedSource,

        /// The location of the unresolved import.
        #[label = "this import"]
        span: SourceSpan,
    }

    pub struct Resolver {
        pub files: FileMap,
        pub inputs: im_rc::HashMap<String, crate::ast::File<state::Quoted>, FxBuildHasher>,
        pub errors: Vec<InnerError>,
        pub scope: im_rc::HashMap<String, Rc<crate::ast::resolved::Definition>, FxBuildHasher>,
        pub file_scope: Scope,
        pub main: crate::ast::File<state::Quoted>,
    }

    /// Current file scope for the resolver.
    #[derive(Default)]
    pub struct Scope {
        /// Possible names for the current scope. It's useful for the error messages,
        /// for example:
        ///
        /// ```haskell
        /// @eval Sim.
        ///
        /// Sim := 10.
        /// ```
        ///
        /// The declaration `Sim` is not defined, but the resolver can suggest the
        /// possible names for the user, in this case `Sim` is the possible name.
        all_possible_names: im_rc::HashMap<String, Rc<Definition>, FxBuildHasher>,
    }

    /// Read file and parse it. Associating the file name with the file.
    ///
    /// It's useful for the resolver.
    fn read_file(path: String, files: &mut FileMap) -> miette::Result<File<state::Quoted>> {
        let text = std::fs::read_to_string(&path)
            .into_diagnostic()
            .wrap_err_with(|| format!("can't read file `{}`", path))?;

        files.insert(path.clone(), text.clone()); // Insert for error handling

        let ast = crate::parser::parse_or_report(&path, &text)?;
        Ok(ast)
    }

    impl Resolver {
        /// Creates and parses a new resolver.
        pub fn new(file: String, inputs: Vec<String>) -> miette::Result<Resolver> {
            let mut files = HashMap::new();
            let file = read_file(file, &mut files)?;
            let mut inputs = inputs
                .into_iter()
                .map(|path| read_file(path, &mut files))
                .collect::<miette::Result<Vec<_>>>()?;

            Ok(Resolver {
                files,
                inputs: inputs
                    .drain(..)
                    .map(|file| (file.name.clone(), file))
                    .collect(),
                errors: vec![],
                scope: im_rc::HashMap::default(),
                file_scope: Default::default(),
                main: file,
            })
        }

        /// Resolves and imports the files.
        pub fn resolve_and_import(mut self) -> miette::Result<File<state::Resolved>> {
            let file = std::mem::take(&mut self.main);
            let file = self.file(file);

            if !self.errors.is_empty() {
                return Err(ResolutionFailure {
                    related: self.errors,
                }
                .into());
            }

            Ok(file)
        }

        // Iterates the statements of the file and collects the errors.
        fn file(&mut self, file: File<state::Quoted>) -> File<state::Resolved> {
            // Create a default scope for the file.
            let mut scope = Scope::default();

            // Define all the statements into the scope
            for stmt in file.stmts.iter() {
                self.define(&mut scope, stmt);
            }

            // Replace the current scope with the new one, so
            // we can handle this scope in the future.
            let old_scope = std::mem::take(&mut self.file_scope);
            self.file_scope = scope;
            let stmts = file
                .stmts
                .into_iter()
                .flat_map(|stmt| self.resolve(stmt))
                .collect();
            self.file_scope = old_scope;

            File {
                name: file.name,
                stmts,
                location: file.location,
            }
        }

        /// Defines a statement. It's useful to define the references.
        fn define(&mut self, scope: &mut Scope, stmt: &Stmt<state::Quoted>) {
            let Some(declaration) = stmt.as_declaration() else {
                return;
            };

            // Adds the definition to the scope.
            scope.all_possible_names.insert(
                declaration.name().text.clone(),
                Rc::new(Definition {
                    location: stmt.location().clone(),
                    text: declaration.name().text.clone(),
                }),
            );
        }

        /// Evaluates a statement, resolving the references.
        fn resolve(&mut self, stmt: Stmt<state::Quoted>) -> Vec<Stmt<state::Resolved>> {
            vec![match stmt {
                Stmt::Error(error) => Stmt::Error(error),
                Stmt::Eval(stmt) => Stmt::Eval(Eval {
                    value: self.term(stmt.value),
                    location: stmt.location,
                }),
                Stmt::Type(stmt) => Stmt::Type(Type {
                    value: self.term(stmt.value),
                    location: stmt.location,
                }),
                Stmt::Binding(stmt) => {
                    let name = stmt.name.text.clone();
                    let definition = Rc::new(Definition {
                        location: stmt.location.clone(),
                        text: stmt.name.text.clone(),
                    });

                    // Adds the definition to the scope.
                    self.scope.insert(name, definition.clone());

                    // Resolve the type and the value of the binding.
                    Stmt::Binding(Binding {
                        doc_strings: stmt.doc_strings,
                        name: definition,
                        location: stmt.location,
                        type_repr: self.term(stmt.type_repr),
                        value: self.term(stmt.value),
                    })
                }
                Stmt::Inductive(_) => todo!(),
                Stmt::Import(import) => {
                    let Some(file) = self.inputs.get(&import.text).cloned() else {
                        let error = InnerError::Import(UnresolvedImport {
                            module: import.text.clone(),
                            source_code: NamedSource::new(
                                &import.location.filename,
                                self.files.get(&import.location.filename).unwrap().clone(),
                            ),
                            span: import.location.clone().into(),
                        });
                        self.errors.push(error);

                        return vec![];
                    };

                    // Resolve the file and concatenate the statements.
                    return self.file(file).stmts;
                }
                Stmt::ElimDef(elim_def) => Stmt::ElimDef(ElimDef {
                    // Resolve the inductive type and the constructors.
                    inductive: self
                        .find_reference(elim_def.inductive.clone())
                        .unwrap_or_else(|| {
                            Rc::new(Definition {
                                location: elim_def.location.clone(),
                                text: elim_def.inductive.text.clone(),
                            })
                        }),
                    // Resolve the constructors.
                    constructors: elim_def
                        .constructors
                        .into_iter()
                        .map(|constructor| {
                            self.find_reference(constructor.clone()).unwrap_or_else(|| {
                                Rc::new(Definition {
                                    location: constructor.location.clone(),
                                    text: constructor.text.clone(),
                                })
                            })
                        })
                        .collect(),
                    location: elim_def.location,
                }),
            }]
        }

        /// Resolves a term. It's useful to resolve the references.
        ///
        /// It's the main function of the resolver.
        pub fn term(&mut self, term: Term<state::Quoted>) -> Term<state::Resolved> {
            match term {
                Term::Elim(_) => todo!(),
                Term::Error(error) => Term::Error(error),
                Term::Universe(universe) => Term::Universe(universe),
                Term::Int(int) => Term::Int(int),
                Term::Str(str) => Term::Str(str),
                Term::Group(group) => Term::Group(self.term(*group).into()),
                Term::Hole(hole) => Term::Hole(hole),
                Term::Fun(fun) => self.fork(|local| {
                    // Resolve the arguments of the function. It's useful to
                    // define the parameters into the scope.
                    let arguments = fun
                        .arguments
                        .into_iter()
                        .map(|parameter| {
                            let definition = Rc::new(Definition {
                                location: parameter.location.clone(),
                                text: parameter.text.clone(),
                            });

                            // Adds the definition to the scope.
                            local
                                .scope
                                .insert(parameter.text.clone(), definition.clone());

                            definition
                        })
                        .collect();

                    Term::Fun(Fun {
                        arguments,
                        value: local.term(*fun.value).into(),
                        location: fun.location,
                    })
                }),
                Term::Apply(apply) => {
                    // Resolve the callee and the arguments.
                    Term::Apply(Apply {
                        callee: self.term(*apply.callee).into(),
                        arguments: apply
                            .arguments
                            .into_iter()
                            .map(|argument| self.term(argument))
                            .collect(),
                        location: apply.location,
                    })
                }
                Term::Reference(reference) => self
                    .find_reference(reference.clone())
                    .map(|definition| {
                        // Create a new reference to the definition of `reference.text`
                        // of the reference.
                        Term::Reference(Reference {
                            definition,
                            location: reference.location.clone(),
                        })
                    })
                    .unwrap_or_else(|| {
                        // If can't find the definition, it will fallback to a hole.
                        Term::Hole(Hole {
                            location: reference.location,
                        })
                    }),
                Term::Pi(pi) => {
                    self.fork(|local| {
                        // Make up the domain of the pi type. It's useful to
                        // resolve the domain.
                        let domain = local.create_domain(pi.domain);

                        // Resolve the codomain of the pi type.
                        let codomain = local.term(*pi.codomain);

                        // Fold the domain into a bunch of pi, just like `x, y : A -> B` into
                        // `x : A -> y : A -> B`.
                        domain.into_iter().fold(codomain, |acc, next| {
                            Term::Pi(Pi {
                                icit: next.icit,
                                domain: next,
                                codomain: acc.into(),
                                location: pi.location.clone(),
                            })
                        })
                    })
                }
            }
        }

        // Transform a domain into one or more domains.
        fn create_domain(&mut self, domain: Domain<state::Quoted>) -> Vec<Domain<state::Resolved>> {
            let mut parameters = vec![];
            let type_repr = self.term(*domain.type_repr);
            for reference in domain.text {
                // Tries to get the location of the reference, if it's not
                // possible, it will fallback to the location of the domain.
                let location = match reference {
                    Some(ref name) => name.location.clone(),
                    None => domain.location.clone(),
                };

                let definition = Rc::new(Definition {
                    location: location.clone(),
                    text: match reference {
                        Some(name) => name.text,
                        None => "_".into(),
                    },
                });

                // Adds the definition to the scope.
                self.scope
                    .insert(definition.text.clone(), definition.clone());

                // Adds the definition to the scope.
                parameters.push(Domain {
                    text: definition,
                    location,
                    type_repr: type_repr.clone().into(),
                    icit: domain.icit,
                });
            }

            parameters
        }

        // Find a reference and returns the definition. If it cant be found,
        // it will report an error.
        fn find_reference(
            &mut self,
            reference: crate::ast::parsed::Reference,
        ) -> Option<Rc<Definition>> {
            match self.scope.get(&reference.text) {
                Some(value) => value.clone().into(),
                None => {
                    let is_later_defined = self.file_scope.all_possible_names.get(&reference.text);

                    if let Some(is_later_defined) = is_later_defined {
                        // If the definition is later defined, it will report
                        // a possible definition.
                        self.report_possible_definition(&reference, is_later_defined.clone());
                    } else {
                        // If can't find the definition, it will fallback to a hole.
                        self.report_unresolved(&reference);
                    }

                    None
                }
            }
        }

        /// Reports a possible definition for a reference.
        fn report_possible_definition(
            &mut self,
            reference: &crate::ast::parsed::Reference,
            definition: Rc<Definition>,
        ) {
            self.errors
                .push(InnerError::LaterDefinition(LaterUnresolvedDefinition {
                    module: reference.text.clone(),
                    source_code: self.get_source_code(&reference.location),
                    span: reference.location.clone().into(),
                    declaration_span: definition.location.clone().into(),
                }))
        }

        /// Reports an error for a reference.
        fn report_unresolved(&mut self, reference: &crate::ast::parsed::Reference) {
            self.errors
                .push(InnerError::Definition(UnresolvedDefinition {
                    module: reference.text.clone(),
                    source_code: self.get_source_code(&reference.location),
                    span: reference.location.clone().into(),
                }))
        }

        fn get_source_code(&self, location: &Location) -> NamedSource {
            NamedSource::new(
                &location.filename,
                self.files.get(&location.filename).unwrap().clone(),
            )
        }

        /// Creates a new fork of the current scope, with a new
        /// scope.
        fn fork<U, F: FnOnce(&mut Self) -> U>(&mut self, f: F) -> U {
            let new_scope = self.scope.clone();
            let scope = std::mem::replace(&mut self.scope, new_scope);
            let value = f(self);
            self.scope = scope;
            value
        }
    }
}

// Type elaborator, it does the type checking stuff.
pub mod elab;

/// Simple program to run `zu` language.
#[derive(clap::Parser, Debug)]
#[command(author, version, about, long_about = None)]
#[command(propagate_version = true)]
pub struct Command {
    #[arg(short, long)]
    pub include: Vec<String>,

    /// The main file to run
    #[arg(short, long)]
    pub main: String,
}

fn main() -> miette::Result<()> {
    miette::set_hook(Box::new(|_| {
        // Build the bupropion handler options, for specific
        // error presenting.
        let handler_opts = bupropion::MietteHandlerOpts::new().build();

        Box::new(handler_opts)
    }))?;

    let command = Command::parse();
    let resolver = resolver::Resolver::new(command.main, command.include)?;
    resolver.resolve_and_import()?;

    Ok(())
}
