#![feature(box_patterns)]

use clap::Parser;

/// The abstract syntax tree for the language.
pub mod ast {
    use std::fmt::Debug;

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
            type NameSet = Option<Self::Definition>;
            type Definition = resolved::Definition;
            type Reference = resolved::Reference;
            type Import = resolved::Import;
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
        pub struct Import(std::convert::Infallible);

        impl Element for Import {
            fn location(&self) -> &Location {
                panic!("Can't construct a location for an import when it's resolved")
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
    #[derive(Debug, Clone)]
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

    impl<S: state::State> Declaration for Binding<S> {
        fn doc_strings(&self) -> &[DocString] {
            &self.doc_strings
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
    pub struct Variable<S: state::State> {
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

    impl<S: state::State> Element for Variable<S> {
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
        pub domain: Variable<S>,
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

    use miette::SourceSpan;
    pub use zu::*;

    lalrpop_mod! {
        #[allow(warnings)]
        /// The parsing module
        pub zu
    }

    /// The parsed file type.
    type FileQt = crate::ast::File<crate::ast::state::Quoted>;

    #[derive(miette::Diagnostic, thiserror::Error, Debug)]
    #[error("oops: multiple errors")]
    #[diagnostic()]
    pub struct ParseError {
        // Note source code by no labels
        #[source_code]
        source_code: String,
        // The source code above is used for these errors
        #[related]
        related: Vec<InnerError>,
    }

    #[derive(miette::Diagnostic, thiserror::Error, Debug)]
    #[error("oops")]
    #[diagnostic()]
    pub struct InnerError {
        /// Span of the error, it's basically the location of the
        /// error in the source code.
        #[label]
        err_span: SourceSpan,

        /// The help message
        #[help]
        help: String,
    }

    fn fmt_expected(expected: &[String]) -> String {
        let mut f = String::new();
        if !expected.is_empty() {
            for (i, e) in expected.iter().enumerate() {
                let sep = match i {
                    0 => "Expected one of",
                    _ if i < expected.len() - 1 => ",",
                    // Last expected message to be written
                    _ => " or",
                };
                f.push_str(&format!("{sep} {e}"));
            }
        }
        f
    }

    /// Builds an ariadne label from a parse error. It's to construct the report
    /// for the user.
    fn label(err: lalrpop_util::ParseError<usize, Token<'_>, &str>) -> InnerError {
        use lalrpop_util::ParseError::*;

        match err {
            InvalidToken { location } => InnerError {
                err_span: SourceSpan::from(location..location),
                help: "invalid token".into(),
            },
            UnrecognizedEof { location, expected } => InnerError {
                err_span: SourceSpan::from(location..location),
                help: format!("expected one of {} but found eof", fmt_expected(&expected)),
            },
            UnrecognizedToken { token, expected } => InnerError {
                err_span: SourceSpan::from(token.0..token.2),
                help: format!("expected one of {}", fmt_expected(&expected)),
            },
            ExtraToken { ref token } => InnerError {
                err_span: SourceSpan::from(token.0..token.2),
                help: format!("extra token found {}", token.1),
            },
            User { error } => todo!(),
        }
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
            related: errors.into_iter().map(|error| label(error.error)).collect(),
            source_code: text.to_string(),
        })
    }
}

/// Resolver module. It does handles the imports and the references.
///
/// It's the second phase of the compiler.
pub mod resolver {
    use std::collections::HashMap;

    use miette::{Context, IntoDiagnostic};

    use crate::ast::{state, File};

    type FileMap = HashMap<String, String>;

    #[derive(thiserror::Error, miette::Diagnostic, Debug)]
    pub enum ResolutionError {
        #[error("parse error")]
        #[diagnostic(code(zu::resolver::parse_error))]
        ParseError,
    }

    pub struct Resolver {
        pub files: FileMap,
        pub inputs: HashMap<String, crate::ast::File<state::Quoted>>,
        pub errors: Vec<ResolutionError>,
        pub file: crate::ast::File<state::Quoted>,
    }

    /// Read file and parse it. Associating the file name with the file.
    ///
    /// It's useful for the resolver.
    fn read_file(path: String, files: &mut FileMap) -> miette::Result<File<state::Quoted>> {
        let text = std::fs::read_to_string(&path)
            .into_diagnostic()
            .wrap_err_with(|| format!("Can't read file `{}`", path))?;

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
                file,
            })
        }

        pub fn resolve_and_import(self) -> miette::Result<File<state::Resolved>> {
            todo!()
        }
    }
}

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
        let handler_opts = miette::MietteHandlerOpts::new()
            .terminal_links(true)
            .unicode(true)
            .context_lines(2)
            .tab_width(2)
            .build();

        Box::new(handler_opts)
    }))?;

    let command = Command::parse();
    let resolver = resolver::Resolver::new(command.main, command.include)?;
    resolver.resolve_and_import()?;

    Ok(())
}
