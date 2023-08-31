use std::cell::Cell;

use crate::ast::{state::State, Anno, Element, Location};

use miette::{NamedSource, SourceSpan};

pub use crate::zu::*;

/// Represents the parsed state, it's the state of the syntax tree when it's just parsed.
#[derive(Default, Debug, Clone)]
pub struct Parsed;

impl State for Parsed {
    type NameSet = Vec<Option<Self::Definition>>;
    type Arguments = Vec<crate::ast::Term<Self>>;
    type Parameters = Vec<Self::Definition>;
    type Definition = Reference;
    type Reference = Reference;

    // SECTION: Syntax sugars
    type Group = Box<crate::ast::Term<Self>>;
    type Import = Import;
    type Siganture = Signature<Self>;
    type Anno = Anno<Self>;
}

impl crate::ast::Term<Parsed> {
    /// Removes the group from the term. It's useful to pattern
    /// match agains't group.
    pub fn unwrap(self) -> Self {
        match self {
            Self::Group(arg0) => *arg0.clone(),
            _ => self,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Signature<S: State> {
    pub name: S::Definition,
    pub type_repr: crate::ast::Term<S>,
    pub meta: S::Meta,
}

impl<S: State> crate::ast::Element<S> for Signature<S> {
    fn meta(&self) -> &<S as State>::Meta {
        &self.meta
    }
}

/// A name reference or definition in the source code.
///
/// It's useful to know the location of the name in the source code
/// and the name itself to be resolved later.
#[derive(Debug, Clone)]
pub struct Reference {
    pub text: String,
    pub meta: Location,
}

impl<S: State<Meta = Location>> Element<S> for Reference {
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

impl<S: State<Meta = Location>> Element<S> for Import {
    fn meta(&self) -> &Location {
        &self.meta
    }
}

/// The parsed file type. It's the state where the AST
/// comes to the world.
type FileQt = crate::ast::File<Parsed>;

/// The error type for the parser. It's useful to debug the parser
/// or report errors to the final end-to-end user.
///
/// The parser can report multiple errors at once, so it's useful
/// to debug the parser.
#[derive(miette::Diagnostic, thiserror::Error, Debug)]
#[error("could not parse due the following errors")]
#[diagnostic()]
pub struct ParseError {
    /// The unlabelled source code that will be pointed in the error
    /// message.
    #[source_code]
    source_code: NamedSource,

    /// The multiple-errors that the parser can report, it's useful
    /// to debug the parser.
    ///
    /// It's related to it's resilience and error recovery.
    #[related]
    related: Vec<InnerError>,
}

/// Inner specified error for the parser. It's useful
/// to debug the parser.
#[derive(miette::Diagnostic, thiserror::Error, Debug)]
#[error("can't parse the file")]
#[diagnostic()]
pub enum InnerError {
    /// The statement doesn't have a type representation or
    /// doesn't a value.
    ///
    /// ```kotlin
    /// fun A
    /// ```
    ///
    /// The function `A` doesn't declare a type representation, or
    /// a value. A well-founded statement can be declared as the
    /// following:
    ///
    /// ```kotlin
    /// fun A : Type
    ///
    /// fun A = 1
    /// ```
    ///
    /// The first statement declares a function `A` that returns
    /// a type `Type`. The second statement declares a function `A`
    /// that returns a value `1`.
    #[error("expected or binding or signature statement")]
    #[diagnostic(
        code(zu::expected_statement),
        url(docsrs),
        help("maybe assign a type representation")
    )]
    ExpectedStatement {
        /// The span of the statement that doesn't have a type
        /// representation or a value. It's the span of the
        /// statement that will be pointed in the error message.
        #[label = "here"]
        err_span: SourceSpan,
    },

    /// Record indexing isn't supported yet. It's a feature
    /// that will be implemented in the future.
    ///
    /// ```kotlin
    /// A.B
    /// ```
    ///
    /// It can be replaced by a simple function call:
    ///
    /// ```kotlin
    /// B A
    /// ```
    #[error("record index isn't supported yet")]
    #[diagnostic(
        code(zu::unsupported_record_index),
        url(docsrs),
        help("maybe open a PR")
    )]
    UnsupportedRecordIndex {
        /// The expression that is a record index. It's the span of the
        /// expression that will be pointed in the error message.
        ///
        /// It's not supported yet, but it will be implemented in the future.
        #[label = "here"]
        err_span: SourceSpan,
    },

    /// Inductive data isn't supported yet. It's a feature
    /// that will be implemented in the future.
    #[error("inductive data isn't supported yet")]
    #[diagnostic(
        code(zu::unsupported_inductive_data),
        url(docsrs),
        help("maybe open a PR")
    )]
    UnsupportedInductiveData {
        /// The name of the inductive data. It's the span of the
        /// name that will be pointed in the error message.
        #[label = "here"]
        err_span: SourceSpan,
    },

    /// Attributes isn't supported yet. It's a feature
    /// that will be implemented in the future.
    #[error("attributes aren't supported yet")]
    #[diagnostic(
        code(zu::unsupported_coinductive_data),
        url(docsrs),
        help("maybe open a PR")
    )]
    UnsupportedAttribute {
        /// The name of the attribute. It's the span of the
        /// name that will be pointed in the error message.
        #[label = "here"]
        err_span: SourceSpan,
    },

    /// Coinductive data isn't supported yet. It's a feature
    /// that will be implemented in the future.
    #[error("coinductive data isn't supported yet")]
    #[diagnostic(
        code(zu::unsupported_coinductive_data),
        url(docsrs),
        help("maybe open a PR")
    )]
    UnsupportedCoinductiveData {
        /// The name of the coinductive data. It's the span of the
        /// name that will be pointed in the error message.
        #[label = "here"]
        err_span: SourceSpan,
    },

    /// The parser found a token that it doesn't recognize as valid. The
    /// typed token won't be recognized by the parser.
    #[error("invalid token")]
    #[diagnostic(code(zu::invalid_token), url(docsrs))]
    InvalidToken {
        /// The unrecognized token. It's the span of the token
        /// that wasn't recognized that will be pointed in the
        /// error message.
        #[label = "here"]
        err_span: SourceSpan,
    },

    /// The token is not recognized by the parser. It's a
    /// token that the parser doesn't recognize, but it's
    /// a valid token that can be placed in the place.
    #[error("unrecognized token")]
    #[diagnostic(code(zu::unrecognized_token), url(docsrs))]
    UnrecoginzedToken {
        /// The unrecognized token. It's the span of the token
        /// that wasn't recognized that will be pointed in the
        /// error message.
        #[label = "here"]
        err_span: SourceSpan,

        /// The help messages pointing to the expected tokens.
        ///
        /// It's useful to know what the parser expected to
        /// parse.
        #[help]
        help: String,
    },

    /// The parser expected a token, but it got an EOF. The EOF
    /// is the end of file, it's a special token that represents
    /// the end of the file.
    #[error("expected token, but got eof")]
    #[diagnostic(code(zu::expected_token), url(docsrs))]
    ExpectedToken {
        #[label = "here"]
        err_span: SourceSpan,

        #[help]
        help: String,
    },

    /// The parser found an extra token that it doesn't expect.
    #[error("extra token, {}", token)]
    #[diagnostic(code(zu::extra_token), url(docsrs))]
    ExtraToken {
        #[label = "here"]
        err_span: SourceSpan,
        token: String,
    },
}

/// Format an expected token message, it's useful for helpful error messages.
///
/// It's useful to know what the parser expected to parse.
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

/// Parses or report the error. It takes a filename to report errors and locations
/// pointing to the file in the error message.
///
/// It does return a result of a parsed file or a parse error, that can contain a
/// lot of sub-errors.
pub fn parse_or_report(filename: &str, text: &str) -> Result<FileQt, ParseError> {
    let mut errors = vec![];
    let unique = std::rc::Rc::new(Cell::new(0));
    let ast = match crate::zu::FileParser::new().parse(&mut errors, filename, &unique, text) {
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
                        help: fmt_expected(&expected),
                    },
                    UnrecognizedToken { token, expected } => InnerError::UnrecoginzedToken {
                        err_span: SourceSpan::from(token.0..token.2),
                        help: fmt_expected(&expected),
                    },
                    ExtraToken { ref token } => InnerError::ExtraToken {
                        err_span: SourceSpan::from(token.0..token.2),
                        token: token.1.to_string(),
                    },
                    User { error } => error,
                }
            })
            .collect(),
        source_code: NamedSource::new(filename, text.to_string()),
    })
}
