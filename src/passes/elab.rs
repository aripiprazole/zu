use std::cell::Cell;
use std::cell::RefCell;
use std::fmt::Debug;
use std::ops::Deref;
use std::rc::Rc;

use miette::NamedSource;
pub use value::*;

use self::unification::UnifyError;
use super::resolver::Definition;
use super::resolver::FileMap;
use super::resolver::Resolved;
use crate::ast::*;
use crate::nfe::Nfe;
use crate::passes::elab::quote::Quote;
use crate::quoting::*;

pub type DefinitionRs = Definition<Resolved>;

pub type Tm = Term<Resolved>;

/// The quoted version of [`Value`], but without locations, and closures
///
/// It's used to debug and build values.
pub type Expr = crate::ast::Term<Quoted>;

pub mod check;
pub mod eval;
pub mod globals;
pub mod infer;
pub mod quote;
pub mod unification;
pub mod value;

/// Module representation with type table and typed values.
pub struct Module {
  pub name: String,
  pub declarations: Vec<globals::Declaration>,
}

/// A couple of type errors, within a source code, this represents all the errors
/// per file.
#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("elaboration error")]
#[diagnostic(code(elaboration_error), url(docsrs))]
pub struct ElaborationError {
  #[source_code]
  source_code: miette::NamedSource,

  /// All related errors to the file
  #[related]
  related: Vec<TypeError>,
}

/// Type error that can be reported to the user
#[derive(thiserror::Error, miette::Diagnostic, Debug, Clone)]
#[error("{message}")]
#[diagnostic(code(type_error), url(docsrs))]
pub struct TypeError {
  message: UnifyError,

  /// The location that the error happened
  #[label("here")]
  span: miette::SourceSpan,

  /// The location that may be have written the type.
  #[label("the type")]
  type_span: Option<miette::SourceSpan>,
}

#[derive(Default, Debug, Clone, PartialEq)]
pub struct Environment {
  pub data: im_rc::Vector<Type>,
  pub globals: globals::GlobalEnvironment,
}

impl Environment {
  /// Creates a new entry in the environment
  pub fn create_new_value(&self, value: Type) -> Self {
    let mut data = self.data.clone();
    data.push_front(value);
    Self {
      data,
      globals: self.globals.clone(),
    }
  }
}

/// Logger to the context, it can be either implemented
/// as a logger, or as a presenter for UI like a LSP.
pub trait Reporter: Debug {
  fn evaluate(&self, value: Nfe, location: Location) -> miette::Result<()>;
  fn check(&self, value: Nfe, location: Location) -> miette::Result<()>;
}

/// The context of the elaborator
#[derive(Debug, Clone)]
pub struct Elab {
  pub lvl: Lvl,
  pub env: Environment,
  pub types: im_rc::Vector<(String, Type)>,
  pub reporter: Rc<dyn Reporter>,
  pub position: RefCell<crate::ast::Location>,
  pub unique: Cell<usize>,
  pub files: FileMap,
  pub errors: RefCell<Vec<TypeError>>,

  /// A map from the name of the declaration to the type of the declaration
  ///
  /// It's a simple type table, so we don't rewrite everything.
  pub tt: RefCell<intmap::IntMap<Type>>,
}

impl Elab {
  pub fn new<T: Reporter + 'static>(files: FileMap, reporter: T) -> Self {
    Self {
      files,
      env: Default::default(),
      lvl: Default::default(),
      types: Default::default(),
      reporter: Rc::new(reporter),
      position: Default::default(),
      tt: Default::default(),
      unique: Default::default(),
      errors: Default::default(),
    }
  }

  /// Lifts the debruijin level of the context, and increases
  /// the level of the context by 1.
  pub fn lift(&self) -> Elab {
    let mut new_ctx = self.clone();
    new_ctx.lvl += 1;
    new_ctx
  }

  /// Elaborates a file into a new file
  /// with types.
  ///
  /// It does elaborates the types of the file.
  pub fn elaborate(&mut self, file: File<Resolved>) -> miette::Result<Module> {
    let declarations = vec![];

    for top_level in file.top_levels {
      match top_level {
        // Sentinel values
        TopLevel::Error(_) => {}

        // Type checking and evaluation of bindings and inductive types.
        //
        // Also should be used for structures, classes, modules.
        TopLevel::Inductive(_) => todo!(),
        TopLevel::Binding(s) => {
          let type_repr = self.check(&s.type_repr, Type::universe()).eval(&self.env);
          let value = self.check(&s.value, type_repr.clone()).eval(&self.env);

          // Create a binding
          self.env.globals.insert(s.name.text.clone(), globals::Declaration {
            name: s.name,
            type_repr,
            value,
          });
        }

        // Inline evaluation and checking functions. `@eval` and `@check`
        TopLevel::Eval(s) => {
          let location = s.value.meta().clone();
          let _ = self.infer(&s.value); // Infers the type
          let value = s.value.erase(self).eval(&self.env);
          let expr = value.show(self);

          self.reporter.evaluate(expr, location)?;
        }
        TopLevel::Check(s) => {
          let location = s.value.meta().clone();
          let value = self.infer(&s.value);
          let expr = value.show(self);

          self.reporter.check(expr, location)?;
        }

        // Removed
        TopLevel::Signature(_) => unreachable!(),
        TopLevel::Import(_) => unreachable!(),
      }
    }

    // Get the current file errors to report
    let mut errors = self.errors.borrow_mut();
    if !errors.is_empty() {
      let position = file.meta;
      let source_code = self.files.get(&position.filename).unwrap().clone();
      return Err(ElaborationError {
        source_code: NamedSource::new(position.filename, source_code),
        related: errors.drain(..).collect(),
      })?;
    }

    Ok(Module {
      name: file.name,
      declarations,
    })
  }

  // SECTION: Insertion
  /// Inserts a new type in implicit argument position.
  pub fn insert(&self, term: Tm, mut type_repr: Type) -> (Tm, Type) {
    let mut acc = term;
    loop {
      match type_repr.value() {
        Value::Pi(_, Icit::Impl, _, cod) => {
          let meta = self.fresh_meta().eval(&self.env);
          acc = Term::Apply(Apply {
            meta: acc.meta().clone(),
            arguments: Box::new(Term::Hole(Hole {
              meta: acc.meta().clone(),
            })),
            callee: Box::new(acc),
          });
          type_repr = cod.apply(meta);
        }
        _ => return (acc, type_repr)
      }
    }
  }

  // ENDSECTION: Insertion

  /// Performs unification between two values, its a
  /// equality relation between two values.
  ///
  /// It does closes some holes.
  #[inline(always)]
  pub fn unify(&self, lhs: Type, rhs: Type) {
    let location = lhs.location();

    if let Err(err) = lhs.unify(rhs, self) {
      let position = self.position();
      self.errors.borrow_mut().push(TypeError {
        message: err.clone(),
        span: position.into(),
        type_span: location.or_none().map(Into::into),
      });
    }
  }

  /// Clones the position
  pub fn position(&self) -> Location {
    self.position.borrow().clone()
  }

  /// Creates a new fresh meta variable that hasn't been evaluated yet.
  ///
  /// It does creates a new meta variable with a unique id.
  pub fn fresh_meta(&self) -> Expr {
    Expr::Reference(Reference::MetaVar(MetaVar::default()))
  }

  /// Create a new binder that has a fresh meta variable.
  pub fn create_new_binder(&self, name: &str) -> Self {
    self.create_new_value(name, self.fresh_meta().eval(&self.env))
  }

  /// Binds a new value in the context.
  pub fn create_new_value(&self, name: &str, value: Type) -> Self {
    let mut data = self.clone();
    data.env = data.env.create_new_value(Type::rigid(data.lvl));
    data.types.push_front((name.to_string(), value));
    data.lvl += 1;
    data
  }
}
