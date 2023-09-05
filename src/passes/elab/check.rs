use super::Type;
use super::*;

impl Elab {
  /// Checks a term against a type
  pub fn check(&self, term: &Tm, type_repr: Type) -> Expr {
    match (term, type_repr) {
      (Term::Hole(_), _) => self.fresh_meta(),

      // Unifies the domain with the function parameter, and the codomain
      // with it's body
      (Term::Fun(ref fun), Type(_, Value::Pi(_, _, box dom, cod))) => Term::Fun(Fun {
        meta: fun.meta.clone(),
        arguments: fun.arguments.as_shift(),
        value: self
          .create_new_value(&fun.arguments.text, dom)
          .check(&fun.value, cod.apply(Type::rigid(self.lvl)))
          .into(),
      }),

      // Fallback case that will cause an error if we can't check
      // the term against the type.
      (t, expected) => {
        let inferred = self.infer(t);
        self.unify(expected.clone(), inferred.clone());
        t.clone().erase(self)
      }
    }
  }
}
