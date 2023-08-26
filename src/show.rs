use std::fmt::Debug;

use crate::{
    ast::Term,
    erase::{Lvl, Erased},
    passes::elab::Value,
};

/// The context we need to pretty print a type right now.
///
/// It's a list of names, and the level of the last bound variable.
pub struct Show {
    pub level: Lvl,
    pub names: Vec<String>,
    pub term: Term<Erased>,
}

impl Debug for Show {
    fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl Value {
    /// Show a value as a string. As the debruijin level 0 is the last bound variable, we need to
    /// pass the size of the environment.
    pub fn show(&self) -> Show {
        todo!()
    }
}
