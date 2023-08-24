use crate::ast::{state::State, Element};

/// Represents the resolved state, it's the state of the syntax tree when it's resolved.
#[derive(Default, Debug, Clone)]
pub struct ClosureConv;

impl State for ClosureConv {
    type Reference = ClosureReference;
    type Meta = crate::pass::elab::TypedMeta;
    type Closure = Closure;
    type Import = !;
}

#[derive(Debug, Clone)]
pub enum ClosureReference {
    /// A reference to a closure.
    Closure(Closure),
    /// A reference to a global.
    Global(crate::pass::elab::Reference),
}

impl<S: State<Meta = crate::pass::elab::TypedMeta>> Element<S> for ClosureReference {
    fn meta(&self) -> &S::Meta {
        match self {
            Self::Closure(c) => &c.meta,
            Self::Global(g) => &g.meta,
        }
    }
}

/// A closure converted term, it's a term that has a closure
/// in it. It's useful for the compiler.
#[derive(Debug, Clone)]
pub struct Closure {
    /// Global index for closures.
    pub index: usize,
    pub meta: crate::pass::elab::TypedMeta,
}

impl<S: State<Meta = crate::pass::elab::TypedMeta>> Element<S> for Closure {
    fn meta(&self) -> &S::Meta {
        &self.meta
    }
}
