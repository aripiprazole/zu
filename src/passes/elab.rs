use std::cell::Cell;
use std::cell::RefCell;
use std::fmt::Debug;
use std::ops::Deref;
use std::rc::Rc;

use miette::NamedSource;

use super::resolver::FileMap;
use super::resolver::Resolved;
use crate::ast::*;
use crate::erase::*;
use crate::nfe::Nfe;
use crate::passes::elab::quote::Quote;

pub type DefinitionRs = Definition<Resolved>;

pub type Tm = Term<Resolved>;

/// The quoted version of [`Value`], but without locations, and closures
///
/// It's used to debug and build values.
pub type Expr = crate::ast::Term<Quoted>;

pub mod check;
pub mod eval;
pub mod infer;
pub mod quote;
pub mod unification;

/// Module representation with type table and typed values
pub struct Mod {
  pub name: String,
  pub declarations: Vec<Declaration>,
}

pub struct Declaration {
  pub name: String,
  pub type_repr: Type,
  pub value: Type,
}

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("elaboration error")]
#[diagnostic(code(elaboration_error), url(docsrs))]
pub struct ElaborationError {
  #[source_code]
  source_code: miette::NamedSource,

  #[related]
  related: Vec<TypeError>,
}

#[derive(thiserror::Error, miette::Diagnostic, Debug, Clone)]
pub enum TypeError {
  #[error("{message}")]
  #[diagnostic(code(type_error), url(docsrs))]
  TypeMismatch {
    message: String,

    #[label("here")]
    span: miette::SourceSpan,
  },

  #[error("handwritten {message}")]
  #[diagnostic(code(type_error), url(docsrs))]
  HandwrittenTypeMismatch {
    message: String,

    #[label("here")]
    span: miette::SourceSpan,

    #[label("the type")]
    type_span: Option<miette::SourceSpan>,
  },
}

#[derive(Default, Debug, Clone, PartialEq)]
pub struct Environment {
  pub data: im_rc::Vector<Type>,
}

impl Environment {
  /// Creates a new entry in the environment
  pub fn create_new_value(&self, value: Type) -> Self {
    let mut data = self.data.clone();
    data.push_front(value);
    Self { data }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
  pub env: Environment,
  pub term: Expr,
}

impl Closure {
  /// Binds a closure with a new environment with the
  /// given value.
  pub fn apply(self, argument: Type) -> Type {
    self.term.eval(&self.env.create_new_value(argument))
  }
}

/// Logger to the context, it can be either implemented
/// as a logger, or as a presenter for UI like a LSP.
pub trait Reporter: Debug {
  fn evaluate(&self, value: Nfe, location: Location) -> miette::Result<()>;
  fn check(&self, value: Nfe, location: Location) -> miette::Result<()>;
}

/// Defines the type of a term, elaborated to a value
///
/// The type of a term is a value, but the type of a value is a type.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
  Prim(PrimKind),
  Flexible(MetaVar, Spine),
  Rigid(Lvl, Spine),
  Lam(DefinitionRs, Closure),
  Pi(DefinitionRs, Icit, Box<Type>, Closure),
  Int(isize),
  Anno(Box<Type>, Box<Type>),
  Str(String),
}

/// Type that holds all the information about a type, just like if
/// it's handwritten, synthesized, etc.
#[derive(Debug, Clone, PartialEq)]
pub struct Type(pub Location, pub Value);

impl Type {
  /// Gets the location of type
  #[inline(always)]
  pub fn location(&self) -> Location {
    self.0.clone()
  }

  /// Gets the value of the type
  #[inline(always)]
  pub fn value(&self) -> Value {
    self.1.clone()
  }
}

pub type Spine = im_rc::Vector<Type>;

/// Create a new spine normal form
///
/// It does creates a new spine normal form from a value.
pub fn unspine(value: Type, spine: Spine) -> Type {
  spine.into_iter().fold(value, |value, argument| value.apply(argument))
}

impl Deref for Type {
  type Target = Value;

  fn deref(&self) -> &Self::Target {
    &self.1
  }
}

impl Type {
  /// Creates a type without location
  pub fn synthesized(value: Value) -> Self {
    Self(SYNTHESIZED.clone(), value)
  }
}

impl Type {
  /// Creates a rigid variable without applications and a
  /// spine to it
  pub fn rigid(lvl: Lvl) -> Self {
    Type::synthesized(Value::Rigid(lvl, Default::default()))
  }

  /// Creates a flexible variable without applications and a
  /// spine to it
  pub fn flexible(meta: MetaVar) -> Self {
    Type::synthesized(Value::Flexible(meta, Default::default()))
  }

  /// Function apply, it does applies a value to a value
  /// creating a new value.
  pub fn apply(self, argument: Type) -> Self {
    match self {
      Type(_, Value::Lam(_, closure)) => closure.apply(argument),
      Type(location, Value::Flexible(meta, mut spine)) => {
        spine.push_back(argument);
        Type(location, Value::Flexible(meta, spine))
      }
      Type(location, Value::Rigid(lvl, mut spine)) => {
        spine.push_back(argument);
        Type(location, Value::Rigid(lvl, spine))
      }
      _ => panic!("expected a function, got another value"),
    }
  }

  /// Creates a new pi type
  pub fn pi(name: &str, domain: Type, codomain: Closure) -> Self {
    let name = Definition::new(name.to_string());

    Type::synthesized(Value::Pi(name, Icit::Expl, domain.into(), codomain))
  }
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
  pub fn elaborate(&mut self, _: Environment, file: File<Resolved>) -> miette::Result<Mod> {
    let declarations = vec![];

    for stmt in file.stmts {
      match stmt {
        // Sentinel values
        Stmt::Error(_) => {}
        Stmt::Inductive(_) => todo!(),
        Stmt::Binding(_) => todo!(),
        Stmt::Eval(s) => {
          let location = s.value.meta().clone();
          let value = s.value.erase(self).eval(&self.env);
          let expr = value.show(self);

          self.reporter.evaluate(expr, location)?;
        }
        Stmt::Type(s) => {
          let location = s.value.meta().clone();
          let value = self.infer(&s.value);
          let expr = value.show(self);

          self.reporter.check(expr, location)?;
        }

        // Erased values, the types with `!`
        Stmt::Signature(_) => unreachable!(),
        Stmt::Import(_) => unreachable!(),
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

    Ok(Mod {
      name: file.name,
      declarations,
    })
  }

  /// Performs unification between two values, its a
  /// equality relation between two values.
  ///
  /// It does closes some holes.
  ///
  /// NOTE: I disabled the formatter so I can align values
  /// and it looks cutier.
  #[inline(always)]
  pub fn unify(&self, lhs: Type, rhs: Type) {
    use unification::UnifyError::*;

    if let Err(err) = lhs.unify(rhs, self) {
      let position = self.position();
      let message = err.to_string();
      self.errors.borrow_mut().push(match err {
        CantUnify(_, _, lhs, _) => TypeError::HandwrittenTypeMismatch {
          message,
          span: position.into(),
          type_span: lhs.or_none().map(Into::into),
        },
        _ => TypeError::TypeMismatch {
          message,
          span: position.into(),
        },
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
    Expr::Reference(Reference::MetaVar(MetaVar::new_unique(self)))
  }

  /// Binds a new value in the context.
  pub fn create_new_value(&self, name: &str, value: Type) -> Self {
    let mut data = self.clone();
    data.env = data.env.create_new_value(Type::rigid(data.lvl));
    data.types = data
      .types
      .clone()
      .into_iter()
      .chain(std::iter::once((name.into(), value)))
      .collect();
    data.lvl += 1;
    data
  }
}
