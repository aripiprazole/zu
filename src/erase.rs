use crate::ast::Term;

impl Term<crate::ast::state::Resolved> {
    /// Erase a term to a term in the untyped lambda calculus.
    pub fn erase(self) -> crate::ast::Term<crate::ast::state::Quoted> {
        todo!()
    }
}