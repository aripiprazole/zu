use std::{collections::HashMap, rc::Rc};

use fxhash::FxBuildHasher;
use miette::{Context, IntoDiagnostic, NamedSource, SourceSpan};

use crate::ast::{
    resolved::{Definition, Reference},
    state::{self, Resolved},
    syntax, Apply, Attribute, Binding, DocString, Domain, Error, Eval, File, Fun, Hole, Int,
    Location, Pi, Stmt, Str, Term, Type, Universe,
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

    #[label("here is the declaration")]
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
    pub inputs: im_rc::HashMap<String, crate::ast::File<state::Syntax>, FxBuildHasher>,
    pub errors: Vec<InnerError>,
    pub scope: im_rc::HashMap<String, Rc<Definition<Resolved>>, FxBuildHasher>,
    pub file_scope: Scope,
    pub main: crate::ast::File<state::Syntax>,
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
    all_possible_names: im_rc::HashMap<String, Rc<Definition<Resolved>>, FxBuildHasher>,
}

/// Read file and parse it. Associating the file name with the file.
///
/// It's useful for the resolver.
fn read_file(path: String, files: &mut FileMap) -> miette::Result<File<state::Syntax>> {
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
    fn file(&mut self, file: File<state::Syntax>) -> File<state::Resolved> {
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
    fn define(&mut self, scope: &mut Scope, stmt: &Stmt<state::Syntax>) {
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
    fn resolve(&mut self, stmt: Stmt<state::Syntax>) -> Vec<Stmt<state::Resolved>> {
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
            Stmt::Binding(stmt) => {
                let name = stmt.name.text.clone();
                let definition = Rc::new(Definition {
                    meta: stmt.meta.clone(),
                    text: stmt.name.text.clone(),
                });

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
                    type_repr: self.term(stmt.type_repr),
                    value: self.term(stmt.value),
                })
            }
            Stmt::Import(import) => {
                let Some(file) = self.inputs.get(&import.text).cloned() else {
                    let error = InnerError::Import(UnresolvedImport {
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
    fn term(&mut self, term: Term<state::Syntax>) -> Term<state::Resolved> {
        match term {
            Term::Elim(_) => todo!(),
            Term::Error(error) => Term::Error(Error { ..error }),
            Term::Universe(universe) => Term::Universe(Universe { ..universe }),
            Term::Int(int) => Term::Int(Int { ..int }),
            Term::Str(str) => Term::Str(Str { ..str }),
            Term::Group(group) => Term::Group(self.term(*group).into()),
            Term::Hole(hole) => Term::Hole(Hole { ..hole }),
            Term::Fun(fun) => self.fork(|local| {
                // Resolve the arguments of the function. It's useful to
                // define the parameters into the scope.
                let arguments = fun
                    .arguments
                    .into_iter()
                    .map(|parameter| {
                        let definition = Rc::new(Definition {
                            meta: parameter.meta.clone(),
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
                    meta: fun.meta,
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
    fn attribute(&mut self, attribute: Attribute<state::Syntax>) -> Attribute<state::Resolved> {
        let _ = attribute;
        todo!()
    }

    // Transform a domain into one or more domains.
    fn create_domain(&mut self, domain: Domain<state::Syntax>) -> Vec<Domain<state::Resolved>> {
        let mut parameters = vec![];
        let type_repr = self.term(*domain.type_repr);
        for reference in domain.text {
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
                text: definition,
                meta: location,
                type_repr: type_repr.clone().into(),
                icit: domain.icit,
            });
        }

        parameters
    }

    // Find a reference and returns the definition. If it cant be found,
    // it will report an error.
    fn find_reference(&mut self, reference: syntax::Reference) -> Option<Rc<Definition<Resolved>>> {
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
        reference: &syntax::Reference,
        definition: Rc<Definition<Resolved>>,
    ) {
        self.errors
            .push(InnerError::LaterDefinition(LaterUnresolvedDefinition {
                module: reference.text.clone(),
                source_code: self.get_source_code(&reference.meta),
                span: reference.meta.clone().into(),
                declaration_span: definition.meta.clone().into(),
            }))
    }

    /// Reports an error for a reference.
    fn report_unresolved(&mut self, reference: &syntax::Reference) {
        self.errors
            .push(InnerError::Definition(UnresolvedDefinition {
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
