use crate::ast::{Location, state::State, Element};

use miette::{NamedSource, SourceSpan};

pub use crate::zu::*;

/// Represents the parsed state, it's the state of the syntax tree when it's just parsed.
#[derive(Default, Debug, Clone)]
pub struct Syntax;

impl State for Syntax {
    type NameSet = Vec<Option<Self::Definition>>;
    type Arguments = Vec<crate::ast::Term<Self>>;
    type Parameters = Vec<Self::Definition>;
    type Definition = Reference;
    type Reference = Reference;
    type Closure = crate::ast::Fun<Self>;
    type Import = Import;
    type Meta = Location;
}

/// A name access.
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

/// The parsed file type.
type FileQt = crate::ast::File<Syntax>;

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
    let ast = match crate::zu::FileParser::new().parse(&mut errors, filename, text) {
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
