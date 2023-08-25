use std::{collections::HashMap, rc::Rc};

use fxhash::FxBuildHasher;
use miette::{Context, IntoDiagnostic, NamedSource, SourceSpan};

use crate::ast::{
    state::{self, State},
    Anno, Apply, Attribute, Binding, Definition, DocString, Domain, Element, Error, Eval, File,
    Fun, Hole, Int, Location, Pi, Stmt, Str, Term, Type, Universe,
};

use super::parser::{parse_or_report, Parsed};

/// Represents the resolved state, it's the state of the syntax tree when it's resolved.
#[derive(Default, Debug, Clone)]
pub struct Resolved;

impl State for Resolved {
    type Definition = Rc<crate::ast::Definition<Resolved>>;
    type Reference = Reference;
    type Meta = Location;
    type Anno = Anno<Self>;
}

/// A name access.
#[derive(Debug, Clone)]
pub struct Reference {
    pub definition: Rc<Definition<Resolved>>,
    pub meta: Location,
}

impl<S: state::State<Meta = Location>> Element<S> for Reference {
    fn meta(&self) -> &Location {
        &self.meta
    }
}

type FileMap = HashMap<String, String>;

#[derive(thiserror::Error, miette::Diagnostic, Debug)]
pub enum InnerError {
    #[error(transparent)]
    #[diagnostic(transparent)]
    UnresolvedImport(#[from] UnresolvedImport),

    #[error(transparent)]
    #[diagnostic(transparent)]
    UnresolvedDefinition(#[from] UnresolvedDefinition),

    #[error(transparent)]
    #[diagnostic(transparent)]
    LaterUnresolvedDefinition(#[from] LaterUnresolvedDefinition),

    #[error(transparent)]
    #[diagnostic(transparent)]
    AlreadyDefinedSignature(#[from] AlreadyDefinedSignature),
}

#[derive(thiserror::Error, miette::Diagnostic, Debug)]
#[diagnostic(code(zu::resolution_failure), url(docsrs))]
#[error("can't resolve the files")]
pub struct ResolutionFailure {
    // The source code above is used for these errors
    #[related]
    related: Vec<InnerError>,
}

#[derive(thiserror::Error, miette::Diagnostic, Debug)]
#[diagnostic(
    code(zu::unresolved_definition),
    url(docsrs),
    help("maybe add an import for it?")
)]
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
    url(docsrs),
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

    #[label("here is the declaration")]
    declaration_span: SourceSpan,
}

#[derive(thiserror::Error, miette::Diagnostic, Debug)]
#[diagnostic(
    code(zu::already_defined_signature),
    url(docsrs),
    help("remove this declaration")
)]
#[error("unresolved already defined: {module} signature in the current file")]
pub struct AlreadyDefinedSignature {
    /// The name of the import.
    pub module: String,

    /// The source code of the import.
    #[source_code]
    source_code: NamedSource,

    #[label = "here is the duplicated"]
    span: SourceSpan,

    #[label("here is the already defined signature")]
    declaration_span: SourceSpan,
}

#[derive(thiserror::Error, miette::Diagnostic, Debug)]
#[diagnostic(
    code(zu::unresolved_import),
    url(docsrs),
    help("try to import the file in the cli")
)]
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
    pub inputs: im_rc::HashMap<String, crate::ast::File<Parsed>, FxBuildHasher>,
    pub errors: Vec<InnerError>,
    pub scope: im_rc::HashMap<String, Rc<Definition<Resolved>>, FxBuildHasher>,
    pub file_scope: Scope,
    pub main: crate::ast::File<Parsed>,
}

/// Current file scope for the resolver.
#[derive(Default)]
pub struct Scope {
    /// All types names for the current file, it's useful to mutually
    /// recursive declarations:
    ///
    /// ```haskell
    /// A : String.
    /// B : String.
    /// B = A.
    /// A = B.
    /// ```
    signatures: im_rc::HashMap<String, (Term<Resolved>, Location), FxBuildHasher>,

    /// Locations for all the definitions in the current file. It's
    /// useful for presenting good-looking error messages.
    ///
    /// ```haskell
    /// A : String.
    /// A : String. // Error: `A` is already defined at line ...., column ...
    /// ```
    locations: im_rc::HashMap<String, Location, FxBuildHasher>,

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
    all_possible_names: im_rc::HashMap<String, Rc<Definition<Resolved>>, FxBuildHasher>,
}

/// Read file and parse it. Associating the file name with the file.
///
/// It's useful for the resolver.
fn read_file(path: String, files: &mut FileMap) -> miette::Result<File<Parsed>> {
    let text = std::fs::read_to_string(&path)
        .into_diagnostic()
        .wrap_err_with(|| format!("can't read file `{}`", path))?;

    files.insert(path.clone(), text.clone()); // Insert for error handling

    let ast = parse_or_report(&path, &text)?;
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
    pub fn resolve_and_import(mut self) -> miette::Result<File<Resolved>> {
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
    fn file(&mut self, file: File<Parsed>) -> File<Resolved> {
        log::info!("loading file `{}`", file.name);

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
            meta: file.meta,
        }
    }

    /// Defines a statement. It's useful to define the references.
    fn define(&mut self, scope: &mut Scope, stmt: &Stmt<Parsed>) {
        let Some(declaration) = stmt.as_declaration() else {
            return;
        };

        let name = declaration.name();

        // Adds the definition to the scope.
        scope.all_possible_names.insert(
            name.text.clone(),
            Rc::new(Definition {
                meta: name.meta.clone(),
                text: declaration.name().text.clone(),
            }),
        );
    }

    /// Evaluates a statement, resolving the references.
    fn resolve(&mut self, stmt: Stmt<Parsed>) -> Vec<Stmt<Resolved>> {
        vec![match stmt {
            Stmt::Inductive(_) => todo!(),
            Stmt::Error(error) => Stmt::Error(Error { ..error }),
            Stmt::Eval(stmt) => Stmt::Eval(Eval {
                value: self.term(stmt.value),
                meta: stmt.meta,
            }),
            Stmt::Type(stmt) => Stmt::Type(Type {
                value: self.term(stmt.value),
                meta: stmt.meta,
            }),
            Stmt::Signature(signature) => {
                let name = signature.name.text.clone();
                let location = signature.name.meta.clone();
                let value = self.term(signature.type_repr);
                if let Some(location) = self.file_scope.locations.get(&name) {
                    self.errors.push(InnerError::AlreadyDefinedSignature(
                        AlreadyDefinedSignature {
                            module: name.clone(),
                            source_code: self.get_source_code(&signature.name.meta),
                            span: signature.name.meta.clone().into(),
                            declaration_span: location.clone().into(),
                        },
                    ));
                }
                self.file_scope.signatures.insert(name, (value, location));

                return vec![];
            }
            Stmt::Binding(stmt) => {
                let name = stmt.name.text.clone();
                let location = stmt.name.meta.clone();
                let definition = Rc::new(Definition {
                    meta: stmt.meta.clone(),
                    text: stmt.name.text.clone(),
                });

                // Dont allow redefining a binding with a binding.
                if let Some(location) = self.file_scope.locations.get(&name) {
                    self.errors.push(InnerError::AlreadyDefinedSignature(
                        AlreadyDefinedSignature {
                            module: name.clone(),
                            source_code: self.get_source_code(&stmt.name.meta),
                            span: stmt.name.meta.clone().into(),
                            declaration_span: location.clone().into(),
                        },
                    ));
                }
                self.file_scope.locations.insert(name.clone(), location);

                // Clone type representation using A : B, where A is the name of the
                // definition, and B is the type representation.
                let type_repr = match self.file_scope.signatures.get(&name) {
                    Some((type_repr, _)) => Term::Anno(Anno {
                        meta: type_repr.meta().clone(),
                        type_repr: type_repr.clone().into(),
                        value: self.term(stmt.type_repr).into(),
                    }),
                    None => self.term(stmt.type_repr),
                };

                // Change the type of the definition.
                let doc_strings = stmt
                    .doc_strings
                    .into_iter()
                    .map(|doc| DocString { ..doc })
                    .collect();

                // Resolve the attributes.
                let attributes = stmt
                    .attributes
                    .into_iter()
                    .map(|attribute| self.attribute(attribute))
                    .collect();

                // Adds the definition to the scope.
                self.scope.insert(name, definition.clone());

                // Resolve the type and the value of the binding.
                Stmt::Binding(Binding {
                    doc_strings,
                    attributes,
                    name: definition,
                    meta: stmt.meta,
                    type_repr,
                    value: self.term(stmt.value),
                })
            }
            Stmt::Import(import) => {
                let Some(file) = self.inputs.get(&import.text).cloned() else {
                    let error = InnerError::UnresolvedImport(UnresolvedImport {
                        module: import.text.clone(),
                        source_code: NamedSource::new(
                            &import.meta.filename,
                            self.files.get(&import.meta.filename).unwrap().clone(),
                        ),
                        span: import.meta.clone().into(),
                    });
                    self.errors.push(error);

                    return vec![];
                };

                // Resolve the file and concatenate the statements.
                return self.file(file).stmts;
            }
        }]
    }

    /// Resolves a term. It's useful to resolve the references.
    ///
    /// It's the main function of the resolver.
    fn term(&mut self, term: Term<Parsed>) -> Term<Resolved> {
        match term {
            Term::Elim(_) => todo!(),
            Term::Error(error) => Term::Error(Error { ..error }),
            Term::Universe(universe) => Term::Universe(Universe { ..universe }),
            Term::Int(int) => Term::Int(Int { ..int }),
            Term::Str(str) => Term::Str(Str { ..str }),
            Term::Group(group) => self.term(*group),
            Term::Hole(hole) => Term::Hole(Hole { ..hole }),
            Term::Anno(anno) => Term::Anno(Anno {
                type_repr: self.term(*anno.type_repr).into(),
                value: self.term(*anno.value).into(),
                meta: anno.meta,
            }),
            Term::Fun(fun) => self.fork(|local| {
                // Resolve the arguments of the function. It's useful to
                // define the parameters into the scope.
                fun.arguments
                    .into_iter()
                    .map(|argument| {
                        // This is needed so we can access the names in the context.
                        let name = argument.text.clone();
                        let parameter = Rc::new(Definition {
                            text: argument.text.clone(),
                            meta: argument.meta.clone(),
                        });

                        local.scope.insert(name, parameter.clone());

                        parameter
                    })
                    .collect::<Vec<_>>()
                    .into_iter()
                    .fold(local.term(*fun.value), |callee, parameter| {
                        Term::Fun(Fun {
                            arguments: parameter,
                            value: callee.into(),
                            meta: fun.meta.clone(),
                        })
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
                    meta: apply.meta,
                })
            }
            Term::Reference(reference) => self
                .find_reference(reference.clone())
                .map(|definition| {
                    // Create a new reference to the definition of `reference.text`
                    // of the reference.
                    Term::Reference(Reference {
                        definition,
                        meta: reference.meta.clone(),
                    })
                })
                .unwrap_or_else(|| {
                    // If can't find the definition, it will fallback to a hole.
                    Term::Hole(Hole {
                        meta: reference.meta,
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
                            meta: pi.meta.clone(),
                        })
                    })
                })
            }
        }
    }

    /// Resolves an attribute. It's useful to resolve the references.
    fn attribute(&mut self, attribute: Attribute<Parsed>) -> Attribute<Resolved> {
        let _ = attribute;
        todo!()
    }

    // Transform a domain into one or more domains.
    fn create_domain(&mut self, domain: Domain<Parsed>) -> Vec<Domain<Resolved>> {
        let mut parameters = vec![];
        let type_repr = self.term(*domain.type_repr);
        for reference in domain.name {
            // Tries to get the location of the reference, if it's not
            // possible, it will fallback to the location of the domain.
            let location = match reference {
                Some(ref name) => name.meta.clone(),
                None => domain.meta.clone(),
            };

            let definition = Rc::new(Definition {
                meta: location.clone(),
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
                name: definition,
                meta: location,
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
        reference: crate::passes::parser::Reference,
    ) -> Option<Rc<Definition<Resolved>>> {
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
        reference: &crate::passes::parser::Reference,
        definition: Rc<Definition<Resolved>>,
    ) {
        self.errors.push(InnerError::LaterUnresolvedDefinition(
            LaterUnresolvedDefinition {
                module: reference.text.clone(),
                source_code: self.get_source_code(&reference.meta),
                span: reference.meta.clone().into(),
                declaration_span: definition.meta.clone().into(),
            },
        ))
    }

    /// Reports an error for a reference.
    fn report_unresolved(&mut self, reference: &crate::passes::parser::Reference) {
        self.errors
            .push(InnerError::UnresolvedDefinition(UnresolvedDefinition {
                module: reference.text.clone(),
                source_code: self.get_source_code(&reference.meta),
                span: reference.meta.clone().into(),
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
