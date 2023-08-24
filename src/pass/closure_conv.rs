use crate::ast::{state::State, Element};

/// Represents the resolved state, it's the state of the syntax tree when it's resolved.
#[derive(Default, Debug, Clone)]
pub struct ClosureConv;

impl State for ClosureConv {
    type Reference = crate::pass::elab::Reference;
    type Meta = crate::pass::elab::TypedMeta;
    type Closure = Closure;
    type Import = !;
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
