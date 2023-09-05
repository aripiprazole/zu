use super::*;
use crate::passes::elab::Type;

impl Expr {
  /// Evaluates a value to a value in the WHNF.
  ///
  /// It does performs reductions.
  pub fn eval(self, env: &Environment) -> Type {
    use crate::ast::Term::*;

    Type(self.meta().clone(), match self {
      // Removed
      Error(_) => unreachable!(),
      Hole(_) => unreachable!(),
      Group(_) => unreachable!(),

      // Values
      Prim(k) => Value::Prim(k.kind),
      Int(data) => Value::Int(data.value),
      Str(data) => Value::Str(data.value),
      Elim(_) => todo!("elim expr"),
      Fun(e) => Value::Lam(Definition::new(e.arguments.text), Closure {
        env: env.clone(),
        term: *e.value,
      }),
      Apply(e) => return e.callee.eval(env).apply(e.arguments.eval(env)),
      Reference(crate::quoting::Reference::Var(Ix(ix))) => return env.data[ix].clone(),
      Reference(crate::quoting::Reference::MetaVar(meta)) => match meta.take() {
        Some(value) => return value,
        None => return Type::flexible(meta),
      },
      Anno(anno) => return anno.value.eval(env),
      Pi(pi) => {
        let name = Definition::new(pi.domain.name.text);
        let domain = pi.domain.type_repr.eval(env);
        let codomain = Closure {
          env: env.clone(),
          term: *pi.codomain,
        };

        Value::Pi(name, pi.domain.icit, domain.into(), codomain)
      }
    })
  }
}
