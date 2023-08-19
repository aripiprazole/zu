use intmap::IntMap;

use crate::ast::resolved::{Definition, Reference};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct MetaVar(usize);

#[derive(Debug, Clone, Copy, Hash)]
pub enum MetaEntry {
    Solved(MetaVar),
    Unsolved,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum BD {
    Bound,
    Defined,
}

#[derive(Debug, Clone, Hash)]
pub struct Spine {
    pub value: Vec<Value>,
}

pub type Level = usize;

#[derive(Debug, Clone)]
pub struct Environment {
    pub value: Vec<Value>,
}

#[derive(Debug, Clone)]
pub struct Closure {
    pub environment: Environment,
    pub term: Expr,
}

/// Partial renaming from Γ to Δ
pub struct PartialRenaming {
    /// Size of Γ
    pub dom: Level,

    /// Size of Δ
    pub cod: Level,

    /// Mapping from Δ vars to Γ vars
    pub ren: IntMap<Level>,
}

impl PartialRenaming {
    /// Lifting a partial renaming over an extra bound variable.
    ///
    /// Given (σ : PartialRenaming Γ Δ), (lift σ : PartialRenaming (Γ, x : A[σ]) (Δ, x : A))
    pub fn lift(self) -> Self {
        todo!()
    }
}

/// The context of the elaborator
#[derive(Debug, Clone)]
pub struct Elab {
    pub env: Environment,
    pub level: Level,
    pub types: im_rc::HashMap<String, Value>,
    pub bds: im_rc::Vector<BD>,
    pub position: crate::ast::Location,
}

pub trait Quote {
    /// Quote a value to an expression
    fn quote(self, nth: Level) -> Expr;
}

/// The quoted version of [`Value`], but without locations, and closures
///
/// It's used to debug and build values.
pub type Expr = crate::ast::Term<crate::ast::state::Quoted>;

impl Quote for Value {
    fn quote(self, nth: Level) -> Expr {
        todo!()
    }
}

/// Defines the type of a term, elaborated to a value
///
/// The type of a term is a value, but the type of a value is a type.
#[derive(Debug, Clone)]
pub enum Value {
    Flexible(MetaVar, Spine),
    Rigid(Level, Spine),
    Lam(Closure),
    Pi(Definition, Box<Value>, Closure),
    Universe,
    Int(isize),
    Str(String),
}
