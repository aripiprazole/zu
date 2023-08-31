use std::fmt::Debug;
use std::rc::Rc;

use crate::ast::*;
use crate::erase::*;

use super::resolver::Resolved;

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

#[derive(thiserror::Error, Debug)]
#[error("type error: {}", error)]
pub struct UnknownTypeError {
    error: String,
}

pub type Spine = im_rc::Vector<Value>;

#[derive(Default, Debug, Clone)]
pub struct Environment {
    pub data: Vec<Value>,
}

impl Environment {
    /// Creates a new entry in the environment
    pub fn create_new_value(&self, value: Value) -> Self {
        let mut data = self.data.clone();
        data.push(value);
        Self { data }
    }
}

#[derive(Debug, Clone)]
pub struct Closure {
    pub env: Environment,
    pub term: Expr,
}

/// Logger to the context, it can be either implemented
/// as a logger, or as a presenter for UI like a LSP.
pub trait Reporter: Debug {
    fn evaluate(&self, value: Value, location: Location) -> miette::Result<()>;
    fn check(&self, value: Value, location: Location) -> miette::Result<()>;
}

/// The context of the elaborator
#[derive(Debug, Clone)]
pub struct Elab {
    pub level: Lvl,
    pub types: im_rc::HashMap<String, Value>,
    pub reporter: Rc<dyn Reporter>,
    pub position: crate::ast::Location,
    pub unique: usize,

    /// A map from the name of the declaration to the type of the declaration
    ///
    /// It's a simple type table, so we don't rewrite everything.
    pub tt: intmap::IntMap<Value>,
}

impl Elab {
    pub fn new<T: Reporter + 'static>(reporter: T) -> Self {
        Self {
            level: Default::default(),
            types: Default::default(),
            reporter: Rc::new(reporter),
            position: Default::default(),
            tt: Default::default(),
            unique: 0
        }
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
                Stmt::Eval(_) => todo!(),
                Stmt::Type(_) => todo!(),

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
    pub fn infer(&mut self, env: Environment, term: &Tm) -> Value {
        /// Infers the type of a term in the context of the environment
        ///
        /// It does returns the type of the term.
        #[inline(always)]
        fn imp(elab: &mut Elab, env: Environment, term: &Tm) -> Value {
            match term {
                Term::Int(_) => Value::constructor("Int"),
                Term::Str(_) => Value::constructor("String"),
                Term::Hole(_) => elab.fresh_meta(),
                Term::Apply(_) => todo!(),
                Term::Error(_) => todo!(),
                Term::Universe(_) => todo!(),
                Term::Anno(_) => todo!(),
                Term::Fun(_) => todo!(),
                Term::Elim(_) => todo!(),
                Term::Group(_) => todo!(),
                Term::Reference(_) => todo!(),
                Term::Pi(pi) => {
                    let name = Definition::new(pi.domain.name.text.clone());
                    let domain = elab.infer(env.clone(), &pi.domain.type_repr);
                    let codomain = Closure {
                        env: env.create_new_value(domain.clone()),
                        term: pi.codomain.clone().erase(),
                    };

                    Value::Pi(name, pi.domain.icit, domain.into(), codomain)
                }
            }
        }

        let meta = term.meta();

        // Sets the position of the elaborator to the error diagnostics
        // goes to the right place.
        self.position = meta.clone();

        // Infers the type of the term
        let value = imp(self, env, term);

        // Insert the type associating it with it's id,
        // so we can use it later.
        self.tt.insert(meta.id, value.clone());

        value
    }

    /// Checks a term against a type
    pub fn check(&mut self, _: Environment, term: Tm, type_repr: Value) -> Expr {
        let _ = term;
        let _ = type_repr;
        todo!()
    }

    /// Creates a new fresh meta variable
    pub fn fresh_meta(&self) -> Value {
        todo!()
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
            Value::Constructor(_) => todo!(),
            Value::Universe => Expr::Universe(Universe::default()),
            Value::Meta(meta_var) => Expr::Reference(crate::erase::Reference::MetaVar(meta_var)),
            Value::Flexible(meta, sp) => quote_sp(
                sp,
                Expr::Reference(crate::erase::Reference::MetaVar(meta)),
                nth,
            ),
            Value::Rigid(lvl, sp) => quote_sp(
                sp,
                Expr::Reference(crate::erase::Reference::Var(lvl.into_ix(nth))),
                nth,
            ),
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
        }
    }
}

impl Expr {
    /// Evaluates a value to a value in the WHNF.
    ///
    /// It does performs reductions.
    pub fn eval(self, env: Environment) -> Value {
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
            Universe(_) => Value::Universe,
            Int(data) => Value::Int(data.value),
            Str(data) => Value::Str(data.value),
            Elim(_) => todo!("elim expr"),
            Fun(_) => todo!("fun expr"),
            Apply(e) => app(e.callee.eval(env.clone()), e.arguments.eval(env)),
            Reference(crate::erase::Reference::Var(Ix(ix))) => env.data[ix].clone(), // TODO: HANDLE ERROR
            Reference(crate::erase::Reference::MetaVar(meta)) => meta.take(),
            Anno(anno) => {
                let value = anno.value.eval(env.clone());
                let type_repr = anno.type_repr.eval(env);

                Value::Anno(value.into(), type_repr.into())
            }
            Pi(pi) => {
                let name = Definition::new(pi.domain.name.text);
                let domain = pi.domain.type_repr.eval(env.clone());
                let codomain = Closure {
                    env,
                    term: *pi.codomain,
                };

                Value::Pi(name, pi.domain.icit, domain.into(), codomain)
            }
        }
    }
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

    /// Creates a constructor
    pub fn constructor(name: &str) -> Self {
        Value::Constructor(name.to_string())
    }
}

impl Closure {
    /// Binds a closure with a new environment with the
    /// given value.
    pub fn apply(self, argument: Value) -> Value {
        self.term.eval(self.env.create_new_value(argument))
    }
}

type DefinitionRs = Definition<Resolved>;

type Tm = Term<Resolved>;

/// Defines the type of a term, elaborated to a value
///
/// The type of a term is a value, but the type of a value is a type.
#[derive(Debug, Clone)]
pub enum Value {
    Universe,
    Constructor(String),
    Flexible(MetaVar, Spine),
    Rigid(Lvl, Spine),
    Lam(DefinitionRs, Closure),
    Pi(DefinitionRs, Icit, Box<Value>, Closure),
    Meta(MetaVar),
    Int(isize),
    Anno(Box<Value>, Box<Value>),
    Str(String),
}
