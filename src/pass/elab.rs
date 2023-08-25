use std::{cell::RefCell, rc::Rc};

use intmap::IntMap;

use crate::ast::state::State;
use crate::ast::{
    Apply, Definition, Domain, Element, Fun, Icit, Int, Location, Pi, Str, Term, Universe, Anno,
};
use crate::erase::{Ix, Lvl, MetaVar, Erased, BD};

use super::resolver::Resolved;

/// Represents the resolved state, it's the state of the syntax tree when it's resolved.
#[derive(Default, Debug, Clone)]
pub struct Typed;

/// Represents a node that has a type.
pub trait TypedNode<S: State>: crate::ast::Element<S> {
    fn type_value(&self) -> &crate::pass::elab::Value;
}

impl State for Typed {
    type Reference = Reference;
    type Meta = TypedMeta;
}

impl<S: State<Meta = TypedMeta>, T: crate::ast::Element<S>> TypedNode<S> for T {
    fn type_value(&self) -> &crate::pass::elab::Value {
        &self.meta().type_value
    }
}

/// A type info. It contains if the type is an enum or a struct, or maybe
/// a function type.
#[derive(Debug, Clone)]
pub enum TypeInfo {}

#[derive(Debug, Clone)]
pub struct TypedMeta {
    pub type_info: TypeInfo,
    pub type_term: Option<Term<Erased>>,
    pub type_value: crate::pass::elab::Value,
    pub location: Location,
}

/// A name access.
#[derive(Debug, Clone)]
pub struct Reference {
    pub definition: Rc<Definition<Resolved>>,
    pub meta: TypedMeta,
}

impl<S: State<Meta = TypedMeta>> Element<S> for Reference {
    fn meta(&self) -> &TypedMeta {
        &self.meta
    }
}

#[derive(Debug, Clone)]
pub enum MetaEntry {
    Solved(Value),
    Unsolved,
}

pub type Spine = im_rc::Vector<Value>;

#[derive(Debug, Clone)]
pub struct Environment {
    pub data: Vec<Value>,
    pub metas: MetaCtx,
}

impl Environment {
    /// Creates a new entry in the environment
    pub fn create_new_value(&self, value: Value) -> Self {
        let mut data = self.data.clone();
        data.push(value);
        Self {
            data,
            metas: self.metas.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Closure {
    pub env: Environment,
    pub term: Expr,
}

/// Partial renaming from Γ to Δ
pub struct PartialRenaming {
    /// Size of Γ
    pub dom: Lvl,

    /// Size of Δ
    pub cod: Lvl,

    /// Mapping from Δ vars to Γ vars
    pub ren: IntMap<Lvl>,
}

impl PartialRenaming {
    /// Lifting a partial renaming over an extra bound variable.
    ///
    /// Given (σ : PartialRenaming Γ Δ), (lift σ : PartialRenaming (Γ, x : A[σ]) (Δ, x : A))
    pub fn lift(self) -> Self {
        todo!()
    }
}

#[derive(Default, Debug, Clone)]
pub struct MetaCtx {
    pub next_meta: Rc<RefCell<usize>>,
    pub ctx: Rc<RefCell<IntMap<MetaEntry>>>,
}

impl MetaCtx {
    /// Lookup a meta variable in the context
    pub fn lookup(&self, MetaVar(m): MetaVar) -> MetaEntry {
        self.ctx.borrow().get(m as u64).cloned().unwrap()
    }
}

/// The context of the elaborator
#[derive(Debug, Clone)]
pub struct Elab {
    pub env: Environment,
    pub level: Lvl,
    pub types: im_rc::HashMap<String, Value>,
    pub bds: im_rc::Vector<BD>,
    pub metas: MetaCtx,
    pub position: crate::ast::Location,
}

impl Elab {
    /// Creates a new type elaborating it into a new
    /// value.
    pub fn elaborate(&self, term: Expr) -> Term<Typed> {
        let _ = term;
        todo!()
    }

    /// Checks a term against a type
    pub fn check(&mut self, term: Term<Resolved>, type_repr: Value) -> Expr {
        let _ = (term, type_repr);
        todo!()
    }

    /// Creates a new fresh meta variable
    pub fn fresh_meta(&self) -> Expr {
        // Add new meta variable
        let mut next_meta = self.metas.next_meta.borrow_mut();
        let meta = MetaVar(*next_meta);
        *next_meta += 1;

        // Add the unsolved meta to the context
        let mut ctx = self.metas.ctx.borrow_mut();
        ctx.insert(*next_meta as u64, MetaEntry::Unsolved);

        // Create the reference
        let bds = self.bds.clone();
        Expr::Reference(crate::erase::Reference::InsertedMeta(meta, bds))
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

        /// Apply a mask of entries to a value
        fn app_bd(env: Environment, value: Value, bds: im_rc::Vector<BD>) -> Value {
            let _ = (env, value, bds);
            todo!()
        }

        /// Evaluates meta
        fn value_meta(env: Environment, meta: MetaVar) -> Value {
            match env.metas.lookup(meta) {
                MetaEntry::Solved(value) => value,
                MetaEntry::Unsolved => Value::Meta(meta),
            }
        }

        match self {
            Error(_) => unreachable!(),
            Hole(_) => unreachable!(),
            Group(_) => unreachable!(),
            Universe(_) => Value::Universe,
            Int(data) => Value::Int(data.value),
            Str(data) => Value::Str(data.value),
            Elim(_) => todo!("elim expr"),
            Fun(_) => todo!("fun expr"),
            Apply(e) => app(e.callee.eval(env.clone()), e.arguments.eval(env)),
            Reference(crate::erase::Reference::Var(Ix(ix))) => env.data[ix].clone(), // TODO: HANDLE ERROR
            Reference(crate::erase::Reference::MetaVar(meta)) => value_meta(env, meta),
            Reference(crate::erase::Reference::InsertedMeta(meta, bds)) => {
                app_bd(env.clone(), value_meta(env, meta), bds)
            }
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
}

impl Closure {
    /// Binds a closure with a new environment with the
    /// given value.
    pub fn apply(self, argument: Value) -> Value {
        self.term.eval(self.env.create_new_value(argument))
    }
}

type DefinitionRs = Definition<Resolved>;

/// Defines the type of a term, elaborated to a value
///
/// The type of a term is a value, but the type of a value is a type.
#[derive(Debug, Clone)]
pub enum Value {
    Universe,
    Flexible(MetaVar, Spine),
    Rigid(Lvl, Spine),
    Lam(DefinitionRs, Closure),
    Pi(DefinitionRs, Icit, Box<Value>, Closure),
    Meta(MetaVar),
    Int(isize),
    Anno(Box<Value>, Box<Value>),
    Str(String),
}
