use std::cell::Cell;
use std::cell::RefCell;
use std::fmt::Debug;
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
pub type Expr = crate::ast::Term<Erased>;

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
  pub type_repr: Value,
  pub value: Value,
}

#[derive(thiserror::Error, miette::Diagnostic, Debug)]
#[error("type error: {message}")]
#[diagnostic(code(type_error), help("this type error is caused by a type mismatch"), url(docsrs))]
pub struct TypeError {
  message: String,

  #[source_code]
  source_code: miette::NamedSource,

  #[label("this type")]
  lhs_span: Option<miette::SourceSpan>,

  #[label("with this")]
  rhs_span: Option<miette::SourceSpan>,
}

pub type Spine = im_rc::Vector<Value>;

/// Create a new spine normal form
///
/// It does creates a new spine normal form from a value.
pub fn unspine(value: Value, spine: Spine) -> Value {
  spine.into_iter().fold(value, |value, argument| value.apply(argument))
}

#[derive(Default, Debug, Clone, PartialEq)]
pub struct Environment {
  pub data: im_rc::Vector<Value>,
}

impl Environment {
  /// Creates a new entry in the environment
  pub fn create_new_value(&self, value: Value) -> Self {
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
  pub fn apply(self, argument: Value) -> Value {
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
  Pi(DefinitionRs, Icit, Box<Value>, Closure),
  Int(isize),
  Anno(Box<Value>, Box<Value>),
  Str(String),
  SrcPos(crate::ast::Location, Box<Value>),
}

impl Value {
  /// Gets a source span to improve error handling
  pub fn source_span(&self) -> Option<Location> {
    match self.clone() {
      Value::Flexible(ref m, ..) => match m.take() {
        Some(value) => value.source_span(),
        None => None,
      },
      Value::SrcPos(span, _) => Some(span),
      _ => None,
    }
  }

  /// Creates a rigid variable without applications and a
  /// spine to it
  pub fn rigid(lvl: Lvl) -> Self {
    Value::Rigid(lvl, Default::default())
  }

  /// Creates a flexible variable without applications and a
  /// spine to it
  pub fn flexible(meta: MetaVar) -> Self {
    Value::Flexible(meta, Default::default())
  }

  /// Function apply, it does applies a value to a value
  /// creating a new value.
  pub fn apply(self, argument: Value) -> Self {
    match self {
      Value::Lam(_, closure) => closure.apply(argument),
      Value::Flexible(meta, mut spine) => {
        spine.push_back(argument);
        Value::Flexible(meta, spine)
      }
      Value::Rigid(lvl, mut spine) => {
        spine.push_back(argument);
        Value::Rigid(lvl, spine)
      }
      _ => panic!("expected a function, got another value"),
    }
  }

  /// Creates a new pi type
  pub fn pi(name: &str, domain: Value, codomain: Closure) -> Self {
    let name = Definition::new(name.to_string());
    Value::Pi(name, Icit::Expl, domain.into(), codomain)
  }
}

/// The context of the elaborator
#[derive(Debug, Clone)]
pub struct Elab {
  pub lvl: Lvl,
  pub env: Environment,
  pub types: im_rc::Vector<(String, Value)>,
  pub reporter: Rc<dyn Reporter>,
  pub position: RefCell<crate::ast::Location>,
  pub unique: Cell<usize>,
  pub files: FileMap,

  /// A map from the name of the declaration to the type of the declaration
  ///
  /// It's a simple type table, so we don't rewrite everything.
  pub tt: RefCell<intmap::IntMap<Value>>,
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
  pub fn unify(&self, lhs: Value, rhs: Value) {
    if let Err(err) = lhs.unify(rhs, self) {
      let message = err.to_string();
      let position = self.position();
      let code = self.files.get(&position.filename).unwrap().clone();
      let (lhs_span, rhs_span) = match err {
        unification::UnifyError::MismatchBetweenInts(_, _) => (None, None),
        unification::UnifyError::MismatchBetweenStrs(_, _) => (None, None),
        unification::UnifyError::CantUnify(_, _, lhs, rhs) => (lhs.map(Into::into), rhs.map(Into::into)),
      };

      panic!("{}", TypeError {
        message,
        source_code: NamedSource::new(position.filename, code),
        lhs_span,
        rhs_span,
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
  pub fn create_new_value(&self, name: &str, value: Value) -> Self {
    let mut data = self.clone();
    data.env = data.env.create_new_value(Value::rigid(data.lvl));
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
