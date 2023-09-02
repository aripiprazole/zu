use std::cell::Cell;
use std::cell::RefCell;
use std::fmt::Debug;
use std::rc::Rc;

use super::resolver::Resolved;
use crate::ast::*;
use crate::erase::*;
use crate::nfe::Nfe;

type DefinitionRs = Definition<Resolved>;

type Tm = Term<Resolved>;

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

#[derive(miette::Diagnostic, thiserror::Error, Debug, Clone, PartialEq, Eq, Hash)]
#[diagnostic()]
pub enum UnifyError {
  /// Int value mismatch between two values,
  #[error("expected int value {0} and got {1}")]
  #[diagnostic(url(docsrs), code(unify::int_mismatch))]
  MismatchBetweenInts(isize, isize),

  /// String value mismatch between two values,
  #[error("expected string value {0} and got {1}")]
  #[diagnostic(url(docsrs), code(unify::str_mismatch))]
  MismatchBetweenStrs(String, String),

  /// Unification error between two types
  #[error("expected type, got another")]
  #[diagnostic(url(docsrs), code(unify::str_mismatch))]
  CantUnify,
}

#[derive(thiserror::Error, Debug)]
#[error("type error: {}", error)]
pub struct UnknownTypeError {
  error: String,
}

pub type Spine = im_rc::Vector<Value>;

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

/// The context of the elaborator
#[derive(Debug, Clone)]
pub struct Elab {
  pub lvl: Lvl,
  pub env: Environment,
  pub types: im_rc::Vector<(String, Value)>,
  pub reporter: Rc<dyn Reporter>,
  pub position: RefCell<crate::ast::Location>,
  pub unique: Cell<usize>,

  /// A map from the name of the declaration to the type of the declaration
  ///
  /// It's a simple type table, so we don't rewrite everything.
  pub tt: RefCell<intmap::IntMap<Value>>,
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
  Meta(MetaVar),
  Int(isize),
  Anno(Box<Value>, Box<Value>),
  Str(String),
  SrcPos(crate::ast::Location, Box<Value>),
}

impl Value {
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

  /// Forcing is important because it does removes the holes created
  /// by the elaborator.
  ///
  /// It does returns a value without holes.
  pub fn force(self) -> Self {
    match self {
      Value::Meta(ref m) => match m.take() {
        Some(value) => value,
        None => self.clone(),
      },
      Value::SrcPos(_, box value) => value,
      _ => self,
    }
  }

  /// Creates a new pi type
  pub fn pi(name: &str, domain: Value, codomain: Closure) -> Self {
    let name = Definition::new(name.to_string());
    Value::Pi(name, Icit::Expl, domain.into(), codomain)
  }

  /// Performs unification between two values, its a
  /// equality relation between two values.
  ///
  /// It does closes some holes.
  ///
  /// # Parameters
  ///
  /// - `self` - The left hand side of the unification
  /// - `rhs`  - The right hand side of the unification
  /// - `ctx`  - The context where the unification is happening
  ///            right now
  ///
  /// # Returns
  ///
  /// It does returns an error if the unification fails. And a
  /// unit if the unification succeeds.
  ///
  /// # Another stuff
  ///
  /// NOTE: I disabled the formatter so I can align values
  /// and it looks cutier.
  #[rustfmt::skip]
  pub fn unify(self, rhs: Value, ctx: &Elab) -> miette::Result<()> {
    /// Imports every stuff so we can't have a lot of
    /// `::` in the code blowing or mind.
    use Value::*;
    use UnifyError::*;

    // Forcing here is important because it does removes the holes created
    // by the elaborator, and it does returns a value without holes.
    //
    // It's important to do this because we don't want to unify holes
    // with values, because it will cause a lot of problems, and it will
    // increase the pattern matching complexity.
    match (self.force(), rhs.force()) {
      // Type universe unification is always true, because
      // we don't have universe polymorphism.
      (Prim(k_a)   , Prim(k_b)) if k_a == k_b => Ok(()),

      // Unification of literal values, it does checks if the values are equal
      // directly. If they are not, it does returns an error.
      (Int(v_a)            , Int(v_b)) if v_a == v_b => Ok(()), // 1 = 1, 2 = 2, etc...
      (Str(v_a)            , Str(v_b)) if v_a == v_b => Ok(()), // "a" = "a", "b" = "b", etc...
      (Int(v_a)            , Int(v_b))               => Err(MismatchBetweenInts(v_a, v_b))?,
      (Str(v_a)            , Str(v_b))               => Err(MismatchBetweenStrs(v_a, v_b))?,

      // Lambda unification, that applies closures and pi types
      // using the spine of applications.
      (Lam(_, v_a)         , Lam(_, v_b)) => {
        v_a.apply(Value::rigid(ctx.lvl))
           .unify(v_b.apply(Value::rigid(ctx.lvl)), &ctx.increase_level())
      }
      (t                   ,   Lam(_, v)) => {
        t.apply(Value::rigid(ctx.lvl))
         .unify(v.apply(Value::rigid(ctx.lvl)), &ctx.increase_level())
      }
      (Lam(_, v)           ,           t) => {
        v.apply(Value::rigid(ctx.lvl))
         .unify(t.apply(Value::rigid(ctx.lvl)), &ctx.increase_level())
      }

      // Unification of application spines or meta variables, it does unifies
      // flexibles, rigids and meta variable's spines.
      //
      // It does unifies the spines of the applications.
      (Flexible(m_a, sp_a) , Flexible(m_b, sp_b)) if m_a == m_b => {
        let _ = (m_a, m_b, sp_a, sp_b);
        Ok(())
      }
      (Rigid(m_a, sp_a)    ,    Rigid(m_b, sp_b)) if m_a == m_b => {
        let _ = (m_a, m_b, sp_a, sp_b);
        Ok(())
      }
      (Flexible(m_a, sp_a) ,                  _t) |
      (_t                  , Flexible(m_a, sp_a))               => {
        // TODO: Solve
        let _ = (m_a, sp_a);
        Ok(())
      }

      // Fallback case which will cause an error if we can't unify
      // the values.
      //
      // It's the fallback of the fallbacks cases, the last error message
      // and the least meaningful.
      (_ , _) => Err(CantUnify)?,
    }
  }
}

impl Elab {
  pub fn new<T: Reporter + 'static>(reporter: T) -> Self {
    Self {
      env: Default::default(),
      lvl: Default::default(),
      types: Default::default(),
      reporter: Rc::new(reporter),
      position: Default::default(),
      tt: Default::default(),
      unique: Default::default(),
    }
  }

  /// Increases the debruijin level of the context
  pub fn increase_level(&self) -> Elab {
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

  /// Creates a new type elaborating it into a new
  /// value.
  pub fn infer(&self, term: &Tm) -> Value {
    /// Infers the type of a term in the context of the environment
    ///
    /// It does returns the type of the term.
    #[inline(always)]
    fn imp(ctx: &Elab, term: &Tm) -> Value {
      match term {
        // Removed
        Term::Group(_) => unreachable!(),

        // Values
        Term::Int(_) => Value::Prim(PrimKind::Int),
        Term::Str(_) => Value::Prim(PrimKind::String),
        Term::Prim(_) => Value::Prim(PrimKind::Universe), // Type of type
        Term::Hole(_) | Term::Error(_) => ctx.fresh_meta().eval(&ctx.env),
        Term::Fun(e) => {
          let name = Definition::new(e.arguments.text.clone());
          let domain = ctx.fresh_meta().eval(&ctx.env);
          let codomain = ctx.create_new_value(&name.text, domain.clone()).infer(&e.value);

          Value::Pi(name, Icit::Expl, domain.clone().into(), Closure {
            env: ctx.env.clone(),

            // Here we need to increase the level, because we are binding
            // in a new environment.
            term: codomain.quote(ctx.lvl + 1),
          })
        }
        Term::Elim(_) => todo!(),

        // Infers the type of a function application, it can apply
        // either a pi type, or a closure that is a lambda.
        Term::Apply(apply) => {
          let callee = ctx.infer(&apply.callee);

          apply
            .arguments
            .iter()
            .cloned()
            // Creates a spine of applications to the callee
            // and then applies the spine to the callee.
            .fold(callee, |callee, argument| {
              let (domain, codomain) = match callee.force() {
                Value::Pi(_, _, box tt, closure) => (tt, closure),
                value => {
                  let tt = ctx.fresh_meta().eval(&ctx.env);
                  let closure = Closure {
                    env: ctx.env.clone(),
                    term: ctx.create_new_value("x", tt.clone()).fresh_meta(),
                  };

                  ctx.unify(Value::pi("x", tt.clone(), closure.clone()), value);

                  (tt, closure)
                }
              };

              let u = ctx.check(&argument, domain);
              codomain.apply(u.eval(&ctx.env))
            })
        }

        // Resolves and infers the type of a reference to a variable
        // in the environment. It does uses debruijin, and `erase` function
        // works very well with it.
        Term::Reference(_) => {
          let Term::Reference(Reference::Var(Ix(ix))) = term.clone().erase(ctx) else {
            // We already check at the beginning of the function with the
            // pattern matching that the term is a reference.
            //
            // So this is unreachable.
            unreachable!()
          };

          // Gets the value from the environment and clones it to avoid
          // borrowing the environment.
          ctx.env.data[ix].clone()
        }

        // Type check annotation, it does only checks the type of the
        // annotation, and returns the value of the annotation.
        //
        // It changes the mode of type checking, from inferring to checking.
        Term::Anno(anno) => ctx
          .check(&anno.value, anno.type_repr.clone().erase(ctx).eval(&ctx.env))
          .eval(&ctx.env),

        // Infers the type of a lambda, it does creates a new closure
        // and returns the type of the closure.
        Term::Pi(pi) => {
          let name = Definition::new(pi.domain.name.text.clone());
          let domain = ctx.infer(&pi.domain.type_repr);
          let codomain = Closure {
            env: ctx.env.create_new_value(domain.clone()),
            term: pi.codomain.clone().erase(ctx),
          };

          Value::Pi(name, pi.domain.icit, domain.into(), codomain)
        }
      }
    }

    let meta = term.meta();

    // Sets the position of the elaborator to the error diagnostics
    // goes to the right place.
    self.position.replace(meta.clone());

    // Infers the type of the term
    let value = imp(self, term);

    // Insert the type associating it with it's id,
    // so we can use it later.
    self.tt.borrow_mut().insert(meta.id, value.clone());

    value
  }

  /// Checks a term against a type
  pub fn check(&self, term: &Tm, type_repr: Value) -> Expr {
    let _ = term;
    let _ = type_repr;
    todo!()
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
      // TODO: add to error lists
      panic!("{err}");
    }
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

pub trait Quote {
  /// Quote a value to an expression
  fn quote(self, nth: Lvl) -> Expr;
}

/// The quoted version of [`Value`], but without locations, and closures
///
/// It's used to debug and build values.
pub type Expr = crate::ast::Term<Erased>;

impl Quote for Value {
  fn quote(self, nth: Lvl) -> Expr {
    /// Applies quoting for a spine of applications in
    /// a term.
    fn quote_sp(sp: Spine, term: Expr, nth: Lvl) -> Expr {
      if sp.is_empty() {
        return term;
      }

      let u = sp.last().cloned().unwrap();
      let len = sp.len();
      let sp = sp.into_iter().skip(len - 1).collect();

      Expr::Apply(Apply {
        callee: quote_sp(sp, term, nth).into(),
        arguments: u.quote(nth).into(),
        meta: Default::default(),
      })
    }

    match self {
      Value::Meta(meta_var) => Expr::Reference(crate::erase::Reference::MetaVar(meta_var)),
      Value::Flexible(meta, sp) => quote_sp(sp, Expr::Reference(crate::erase::Reference::MetaVar(meta)), nth),
      Value::Rigid(lvl, sp) => quote_sp(sp, Expr::Reference(crate::erase::Reference::Var(nth.into_ix(lvl))), nth),
      Value::Prim(kind) => Expr::Prim(Prim {
        kind,
        meta: Default::default(),
      }),
      Value::Int(value) => Expr::Int(Int {
        value,
        meta: Default::default(),
      }),
      Value::Str(value) => Expr::Str(Str {
        value,
        meta: Default::default(),
      }),
      Value::Lam(name, closure) => Expr::Fun(Fun {
        arguments: Definition::new(name.text),
        value: closure.apply(Value::rigid(nth)).quote(nth + 1).into(),
        meta: Default::default(),
      }),
      Value::Pi(name, icit, domain, codomain) => Expr::Pi(Pi {
        icit,
        domain: Domain {
          name: Definition::new(name.text),
          type_repr: domain.quote(nth).into(),
          icit,
          meta: Default::default(),
        },
        codomain: codomain.apply(Value::rigid(nth)).quote(nth + 1).into(),
        meta: Default::default(),
      }),
      Value::Anno(value, type_repr) => Expr::Anno(Anno {
        value: value.quote(nth).into(),
        type_repr: type_repr.quote(nth).into(),
        meta: Default::default(),
      }),
      Value::SrcPos(_, box value) => value.quote(nth),
    }
  }
}

impl Expr {
  /// Evaluates a value to a value in the WHNF.
  ///
  /// It does performs reductions.
  pub fn eval(self, env: &Environment) -> Value {
    use crate::ast::Term::*;

    /// Evaluates an application
    fn app(callee: Value, value: Value) -> Value {
      match callee {
        Value::Lam(_, lam) => lam.apply(value),
        Value::Flexible(meta, mut spine) => {
          spine.push_back(value);

          Value::Flexible(meta, spine)
        }
        Value::Rigid(lvl, mut spine) => {
          spine.push_back(value);

          Value::Rigid(lvl, spine)
        }
        _ => unreachable!(),
      }
    }

    match self {
      // Removed
      Error(_) => unreachable!(),
      Hole(_) => unreachable!(),
      Group(_) => unreachable!(),

      // Values
      Prim(_) => Value::Prim(PrimKind::Universe),
      Int(data) => Value::Int(data.value),
      Str(data) => Value::Str(data.value),
      Elim(_) => todo!("elim expr"),
      Fun(e) => Value::Lam(Definition::new(e.arguments.text), Closure {
        env: env.clone(),
        term: *e.value,
      }),
      Apply(e) => app(e.callee.eval(env), e.arguments.eval(env)),
      Reference(crate::erase::Reference::Var(Ix(ix))) => env.data[ix].clone(), /* TODO: HANDLE ERROR */
      Reference(crate::erase::Reference::MetaVar(meta)) => match meta.take() {
        Some(value) => value,
        None => Value::Meta(meta),
      },
      Anno(anno) => {
        let value = anno.value.eval(env);
        let type_repr = anno.type_repr.eval(env);

        Value::Anno(value.into(), type_repr.into())
      }
      Pi(pi) => {
        let name = Definition::new(pi.domain.name.text);
        let domain = pi.domain.type_repr.eval(env);
        let codomain = Closure {
          env: env.clone(),
          term: *pi.codomain,
        };

        Value::Pi(name, pi.domain.icit, domain.into(), codomain)
      }
    }
  }
}
