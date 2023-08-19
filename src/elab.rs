use intmap::IntMap;

use crate::ast::resolved::Definition;

pub struct MetaVar(usize);

pub enum MetaEntry {
    Solved(MetaVar),
    Unsolved,
}

pub enum BD {
    Bound,
    Defined,
}

pub struct Spine {
    pub value: Vec<Value>,
}

pub type Level = usize;

pub struct Environment {
    pub value: Vec<Value>,
}

pub struct Closure {
    pub environment: Environment,
    pub term: Quoted,
}

/// Partial renaming from Γ to Δ
pub struct PartialRenaming {
    /// Size of Γ
    dom: Level,

    /// Size of Δ
    cod: Level,

    /// Mapping from Δ vars to Γ vars
    ren: IntMap<Level>,
}

pub struct Elab {
    pub env: Environment,
    pub level: Level,
    pub types: im_rc::HashMap<String, Value>,
    pub bds: im_rc::Vector<BD>,
    pub position: crate::ast::Location,
}

pub enum Quoted {}

pub enum Value {
    Flexible(MetaVar, Spine),
    Rigid(Level, Spine),
    Lam(Closure),
    Pi(Definition, Box<Value>, Closure),
    Universe,
    Int(isize),
    Str(String),
}
