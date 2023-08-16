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

    #[derive(Default, Hash, PartialEq, Eq, Clone)]
    pub struct Location {
        pub from: usize,
        pub to: usize,
        pub file: String,
    }

    impl Debug for Location {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "Location")
        }
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
    /// Zero : _ = \fun _, _, zero
    ///   zero.
    ///
    /// -- | Defines the succ constructor
    /// Succ : _ = \fun n, N, succ, _
    ///   (n N succ zero).
    /// ```
    #[derive(Debug)]
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
    #[derive(Debug)]
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
    #[derive(Debug)]
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
    #[derive(Debug)]
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
    pub enum Term<S: state::State> {
        Error(Error),
        Universe(Universe),
        Int(Int),
        Str(Str),
        Elim(Elim<S>),
        Fun(Fun<S>),
        Variable(Variable<S>),
        Apply(Apply<S>),
        Pi(Pi<S>),
        Reference(S::Reference),
        Hole(Hole),
    }

    impl<S: state::State> Debug for Term<S> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::Error(arg0) => arg0.fmt(f),
                Self::Universe(arg0) => arg0.fmt(f),
                Self::Int(arg0) => arg0.fmt(f),
                Self::Str(arg0) => arg0.fmt(f),
                Self::Elim(arg0) => arg0.fmt(f),
                Self::Fun(arg0) => arg0.fmt(f),
                Self::Variable(arg0) => arg0.fmt(f),
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
    use std::num::ParseIntError;

    use logos::Logos;

    #[derive(Default, Debug, PartialEq, Eq, Clone, Copy)]
    pub enum LexerError {
        IntegerParseError,

        #[default]
        Unknown,
    }

    impl From<ParseIntError> for LexerError {
        fn from(_: ParseIntError) -> Self {
            LexerError::IntegerParseError
        }
    }

    /// The base structure of the lexer.
    #[derive(Logos, Debug, PartialEq, Eq, Clone)]
    #[logos(error = LexerError)]
    pub enum Token {
        #[regex("[ \\t\\n\\f]+", logos::skip)]
        Ws,

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
        ModuleKw,

        #[token("\\inductive")]
        InductiveKw,

        #[token("\\type")]
        TypeKw,

        #[token("\\pi")]
        PiKw,

        #[token("\\elim")]
        ElimKw,

        #[token("\\of")]
        OfKw,

        #[token("\\fun")]
        FunKw,

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
        Dot,

        #[token(",")]
        Comma,

        #[token(":")]
        Colon,

        #[token("=")]
        Eq,

        #[token("->")]
        Arrow,

        #[token("=>")]
        DoubleArrow,

        // SECTION: Values
        #[regex("--.*\n")]
        Comment,

        #[regex("[a-zA-Z*/+-][a-zA-Z0-9_*/+-]*")]
        Constructor,

        #[regex("\"\\.*\"")]
        String,

        #[regex("\\d")]
        Int,
    }
}

/// The parsing part of the language.
pub mod parser {
    use std::cell::Cell;

    use crate::{
        failure::DiagId,
        lexer::{LexerError, Token},
    };

    use super::*;
    use logos::{Logos, Span};

    /// The parser implementation, which turns tokens into the abstract syntax tree,
    /// it's the main part of the compiler.
    pub struct Parser<'src> {
        pub lexer: Vec<(Result<lexer::Token, LexerError>, Span)>,
        pub failures: Vec<failure::Failure>,

        /// The source code.
        src: &'src str,

        /// The current index of the parser.
        index: usize,

        /// The fuel, to avoiding infinite loops.
        #[cfg(debug_assertions)]
        fuel: Cell<usize>,
    }

    /// Defines a marker for the parser. It's useful to create locations and closing it
    pub struct Marker(ast::Location);

    impl<'src> Parser<'src> {
        /// Creates a new parser instance instantiating the lexer with the source code.
        ///
        /// ## Parameters
        ///
        /// - `src`: The source code to parse.
        pub fn new(src: &'src str) -> Self {
            Self {
                lexer: lexer::Token::lexer(src).spanned().collect(),
                failures: Vec::new(),
                #[cfg(debug_assertions)]
                fuel: Cell::new(256),
                index: 0,
                src,
            }
        }

        /// Creates a location for the latest span ever found in the parser,
        /// it's useful to reporting locations in the tree.
        pub fn location(&mut self) -> ast::Location {
            if self.is_eof() {
                return Default::default();
            }

            let span = self.lexer[self.index].1.clone();

            ast::Location {
                from: span.start,
                to: span.end,
                file: self.src.to_string(),
            }
        }

        /// Opens and closes a marker, it's useful to create locations.
        #[must_use]
        #[inline(always)]
        pub fn open(&mut self) -> Marker {
            Marker(self.location())
        }

        /// Opens and closes a marker, it's useful to create locations.
        #[must_use]
        #[inline(always)]
        pub fn next_and_close(&mut self, m: Marker) -> ast::Location {
            let location = self.close(m);
            self.advance();
            location
        }

        /// Close a marker and get a location from it
        #[must_use]
        pub fn close(&mut self, Marker(latest): Marker) -> ast::Location {
            if self.is_eof() && self.lexer.last().is_some() {
                // Return latest location before EOF
                return self.create_location(self.lexer.last().unwrap().1.clone());
            } else if self.is_eof() {
                return latest; // Return latest location before EOF
            }

            let span = self.lexer[self.index].1.clone();

            ast::Location {
                from: latest.from,
                to: span.end,
                file: self.src.to_string(),
            }
        }

        /// Lookup the current token.
        pub fn lookahead(&mut self, n: usize) -> Option<lexer::Token> {
            #[cfg(debug_assertions)]
            if self.fuel.get() == 0 {
                panic!("infinite loop detected");
            } else {
                self.fuel.set(self.fuel.take() - 1);
            }

            match self.lexer.get(self.index + n)? {
                (Ok(token), _) => Some(token.clone()),
                (Err(_), span) => {
                    self.failures.push(failure::Failure {
                        kind: failure::DiagKind::Parser,
                        level: failure::DiagLevel::Error,
                        id: diag_id!("unknown-token"),
                        message: "unknown token".to_string(),
                        location: self.create_location(span.clone()),
                    });

                    None
                }
            }
        }

        /// Expects a token, if it's not the current token, it will report an error.
        pub fn expect(&mut self, message: &str, kind: Token) {
            if self.lookahead(0) == Some(kind) {
                self.advance();
            } else {
                self.fail(message, diag_id!["unexpected-token"]);
                self.advance(); // Advance anyways.
            }
        }

        /// Check if the parser has reached the end of the file.
        #[inline(always)]
        pub fn is_eof(&self) -> bool {
            self.index >= self.lexer.len()
        }

        /// Advances the parser.
        #[inline(always)]
        pub fn advance(&mut self) {
            #[cfg(debug_assertions)]
            self.fuel.set(self.fuel.take() + 1);

            self.index += 1;
        }

        /// Proceeds the parser with true to make easier pattern matching
        #[inline(always)]
        pub fn skips(&mut self) -> bool {
            if self.is_eof() {
                return false;
            }

            self.advance();
            true
        }

        /// Expects a token, if it's not the current token, it will return false.
        #[inline(always)]
        pub fn is(&mut self, kind: Token) -> bool {
            self.lookahead(0) == Some(kind)
        }

        /// Creates a new high-level location from logos' lexer span.
        ///
        /// It's useful for creating locations for diagnostics.
        #[inline(always)]
        pub fn create_location(&self, span: Span) -> ast::Location {
            ast::Location {
                from: span.start,
                to: span.end,
                file: self.src.to_string(),
            }
        }

        /// Expects a token in the parser, if it's not the current token, it will report an error.
        #[inline(always)]
        pub fn is_in(&mut self, tokens: &[Token]) -> bool {
            tokens.contains(&self.lookahead(0).unwrap_or(Token::Dot))
        }

        #[inline(always)]
        pub fn fail(&mut self, message: &str, id: DiagId) {
            let token = self
                .lexer
                .get(self.index)
                .cloned()
                .or_else(|| self.lexer.last().cloned())
                .expect("expected a token");

            // Don't report an error if it's the end of the file.
            self.failures.push(failure::Failure {
                kind: failure::DiagKind::Parser,
                level: failure::DiagLevel::Error,
                id,
                message: message.to_string(),
                location: self.create_location(token.1.clone()),
            });
        }

        /// Reads a location to a string buffer
        #[inline(always)]
        pub fn slice(&mut self, location: &ast::Location) -> String {
            self.src[location.from..location.to].to_string()
        }
    }

    /// Expects a token in the parser, if it's not the current token, it will report an error.
    #[macro_export]
    macro_rules! expect {
        ($p:expr, $t:expr, $($arg:tt)*) => {
            $p.expect(&format!($($arg)*), $t)
        };
    }

    /// Expects a token in the parser, if it's not the current token, it will return false.
    #[macro_export]
    macro_rules! delimited {
        ($p:expr, $s:expr, $e:expr, $f:expr, $($arg:tt)*) => {{
            expect!($p, $s, $($arg)*);
            let f = $f($p);
            expect!($p, $e, $($arg)*);
            f
        }};
    }

    /// Recovers from an error, it will report an error, and return a recovery value.
    #[macro_export]
    macro_rules! recover {
        ($p:expr, $m:expr, $($arg:tt)*) => {
            if $p.is_eof() {
                let l = $p.close($m);
                $crate::ast::Recovery::recover_from_error($crate::ast::Error {
                    message: "unexpected end of file".into(),
                    full_text: $p.slice(&l),
                    location: l,
                })
            } else {
                let l = $p.close($m);
                let m = format!($($arg)*);
                $p.fail(&m, diag_id!["cant-parse"]);
                $crate::ast::Recovery::recover_from_error($crate::ast::Error {
                    message: m,
                    full_text: $p.slice(&l),
                    location: l,
                })
            }
        };
    }
}

/// The grammar implementation of the language.
pub mod grammar {
    use super::*;
    use crate::{
        ast::{parsed, state},
        lexer::Token,
        parser::Parser,
    };

    /// The first character that can be parsed as a an expression.
    ///
    /// It's useful to recover from errors.
    const EXPR_FIRST: &[Token] = &[
        Token::LParen,
        Token::ElimKw,
        Token::FunKw,
        Token::PiKw,
        Token::TypeKw,
        Token::Constructor,
        Token::Int,
    ];

    /// The last character that can be parsed as a an expression.
    const EXPR_RECOVERY: &[Token] = &[Token::RParen, Token::Comma, Token::Dot];

    pub fn definition(p: &mut Parser) -> parsed::Definition {
        let m = p.open();
        let location = p.next_and_close(m);

        parsed::Definition {
            text: p.slice(&location),
            location,
        }
    }

    /// Parses a simple reference to a definition.
    pub fn reference(p: &mut Parser) -> parsed::Reference {
        let m = p.open();
        let location = p.next_and_close(m);

        parsed::Reference {
            text: p.slice(&location),
            location,
        }
    }

    /// Usually finishes a statement with a `.`.
    pub fn dot(p: &mut Parser) {
        expect!(p, Token::Dot, "expected `.` ending of statement");
    }

    /// Check if it's a comment, and if it is, then parse it.
    pub fn doc_string(p: &mut Parser) -> Option<ast::DocString> {
        if !p.is(Token::Comment) {
            return None;
        }

        let m = p.open();
        let location = p.next_and_close(m);
        let text = p.slice(&location);

        Some(ast::DocString {
            full_text: (&text[2..]).into(),
            location,
            text,
        })
    }

    /// Parses a statement. It has the following grammar:
    ///
    /// ```txt
    /// stmt =
    /// | <definition> : <expr> = <expr> . # Stmt::Binding
    pub fn stmt(p: &mut Parser) -> ast::Stmt<state::Quoted> {
        let m = p.open();
        let mut doc_strings = vec![];

        // Parse doc string whenever it's possible to add into the
        // documentation string list.
        while let Some(doc_string) = doc_string(p) {
            doc_strings.push(doc_string);
        }

        match p.lookahead(0) {
            // SECTION: Signature
            //   Parses a signature, it's a type or a value definition.
            //   Grammar: ?doc_strings <constructor> (: <expr>)? = <expr>.
            Some(Token::Constructor) => {
                let definition = definition(p);
                // Parses a type representation. It can be a
                // hole, or a type.
                let type_repr = match p.lookahead(0) {
                    // Parses type repr as a type, normally.
                    Some(Token::Colon) if p.skips() => expr(p),

                    // As the next value is an `=`, so we can set the type
                    // as a hole, with the location set to the definition's
                    // location.
                    Some(Token::Eq) => ast::Term::Hole(ast::Hole {
                        location: definition.location.clone(),
                    }),

                    // Return error and recover with hole default value for
                    // type repr.
                    _ => {
                        expect!(p, Token::Eq, "expected the value of signature");

                        // Return hole as default type. for the signature.
                        ast::Term::Hole(ast::Hole {
                            location: definition.location.clone(),
                        })
                    }
                };
                expect!(p, Token::Eq, "expected the value of signature");
                let value = expr(p);
                dot(p);

                // Constructs the binding signature.
                ast::Stmt::Binding(ast::Binding {
                    location: p.close(m),
                    name: definition,
                    doc_strings,
                    type_repr,
                    value,
                })
            }

            // SECTION: Induction
            Some(Token::InductiveKw) => recover!(p, m, r"`\inductive` types aren't supported yet"),

            // SECTION: Commands
            Some(Token::CmdElim) => recover!(p, m, "`@elim` commands aren't supported yet"),
            Some(Token::CmdEval) => recover!(p, m, "`@eval` commands aren't supported yet"),
            Some(Token::CmdType) => recover!(p, m, "`@type` commands aren't supported yet"),
            Some(Token::CmdImport) => recover!(p, m, "`@import` commands aren't supported yet"),

            // SECTION: Errors
            // Send a diagnostic to parser, and recover the tree.
            None => recover!(p, m, "eof can't be parsed into statement"),
            _ => recover!(p, m, "unknown statement token"),
        }
    }

    /// Parses a variable parameter. It has the following grammar:
    pub fn variable(p: &mut Parser, icit: ast::Icit) -> ast::Variable<state::Quoted> {
        let m = p.open();
        let name = definition(p);
        expect!(p, Token::Colon, "expected `:` for the pi type");
        let domain = expr(p);

        ast::Variable {
            icit,
            text: Some(name),
            location: p.close(m),
            type_repr: Some(domain.into()),
        }
    }

    /// Parses a primary expression. It has the following grammar:
    ///
    /// ```txt
    /// Primary =
    /// | <reference>                           # Term::Reference
    /// | <integer>                             # Term::Int
    /// | \type                                 # Term::Universe
    /// | \elim <expr> \of (<case> ,*)          # Term::Elim
    /// | \fun (<definition> ,*) -> <expr>      # Term::Fun
    /// | \pi (<definition> : <expr>) -> <expr> # Term::Pi
    pub fn primary(p: &mut Parser) -> ast::Term<state::Quoted> {
        /// Consumes a token and returns the string of the content.
        ///
        /// It's useful when parsing literals.
        fn next(p: &mut Parser) -> String {
            let location = p.location();
            let string = p.slice(&location);
            p.advance();
            string
        }

        /// Pi expression
        fn pi_expr(p: &mut Parser, m: parser::Marker) -> ast::Term<state::Quoted> {
            let domain = match p.lookahead(0) {
                // Pareses explicit bindings for Pi types.
                Some(Token::LParen) if p.skips() => {
                    let variable = variable(p, ast::Icit::Expl);
                    expect!(p, Token::RParen, "expected `)` finish for the pi type");
                    variable
                }

                // Parses implicit bindings for Pi types.
                Some(Token::LBrace) if p.skips() => {
                    let variable = variable(p, ast::Icit::Impl);
                    expect!(p, Token::RBrace, "expected `}}` finish for the pi type");
                    variable
                }

                // Returns an error and recover the default value for the
                // parameter
                _ => {
                    expect!(p, Token::LParen, "expected `(` for the pi type");

                    ast::Variable {
                        icit: ast::Icit::Expl,
                        text: None,
                        location: p.location(),
                        type_repr: None,
                    }
                }
            };
            expect!(p, Token::Arrow, "expected `->` to the codomain");
            let codomain = expr(p);

            ast::Term::Pi(ast::Pi {
                icit: ast::Icit::Expl,
                domain,
                codomain: codomain.into(),
                location: p.close(m),
            })
        }

        /// Fun expression parser
        fn fun_expr(p: &mut Parser, m: parser::Marker) -> ast::Term<state::Quoted> {
            // Parses a new argument whenever it's possible to detect a
            // new definition.
            let mut arguments = vec![definition(p)];
            while p.is(Token::Comma) {
                p.advance();

                arguments.push(definition(p));
            }

            ast::Term::Fun(ast::Fun {
                arguments,
                value: expr(p).into(), // Parses the codomain of the function.
                location: p.close(m),
            })
        }

        /// Group expression parser
        fn group_expr(p: &mut Parser) -> ast::Term<state::Quoted> {
            delimited!(p, Token::LParen, Token::RParen, expr, "expected `)`")
        }

        let m = p.open();

        match p.lookahead(0) {
            // SECTION: Primaries
            Some(Token::PiKw) if p.skips() => pi_expr(p, m),
            Some(Token::FunKw) if p.skips() => fun_expr(p, m),
            Some(Token::ElimKw) if p.skips() => todo!(),
            Some(Token::LParen) => group_expr(p),

            // SECTION: Literals
            // Parses a constructor
            Some(Token::Constructor) => ast::Term::Reference(reference(p)),
            // Parses a type universe
            Some(Token::TypeKw) => ast::Term::Universe(ast::Universe {
                location: p.next_and_close(m),
            }),
            // Parses the integer.
            Some(Token::Int) => ast::Term::Int(ast::Int {
                value: next(p).parse::<isize>().unwrap(),
                location: p.next_and_close(m),
            }),
            // Parses the string.
            Some(Token::String) => {
                let str = next(p);

                ast::Term::Str(ast::Str {
                    value: (&str[1..str.len() - 1]).into(),
                    location: p.next_and_close(m),
                })
            }

            // SECTION: Errors
            // Send a diagnostic to parser, and recover the tree.
            None => recover!(p, m, "eof can't be parsed into expression"),
            _ if p.skips() => recover!(p, m, "unknown expression token"),
            _ => recover!(p, m, "unknown expression token"),
        }
    }

    /// Parses an expression. It has the following grammar:
    ///
    /// ```txt
    /// expr =
    /// | <primary> (<primary> *) # Term::Apply
    /// | <expr> ((-> <expr>) *)  # Term::Pi
    /// ```
    pub fn expr(p: &mut Parser) -> ast::Term<state::Quoted> {
        let m = p.open();
        let callee = primary(p);
        let mut arguments = vec![];

        // Parses a new argument whenever it's possible to detect a
        // new expression.
        while p.is_in(EXPR_FIRST) {
            arguments.push(primary(p));
        }

        // If there's no argument, return directly the callee to avoid
        // redundancy.
        if arguments.is_empty() {
            callee
        } else {
            ast::Term::Apply(ast::Apply {
                callee: callee.into(),
                arguments,
                location: p.close(m),
            })
        }
    }
}

fn main() {
    let mut p = parser::Parser::new(
        "-- | Defines the succ constructor\n
         A : \\pi {a : b} -> c = \\fun a, b (a b).",
    );

    println!("AST: {:#?}", grammar::stmt(&mut p));
    println!("Failures: {:#?}", p.failures);
}
