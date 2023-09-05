use super::*;
use crate::passes::elab::Type;

impl Expr {
  /// Evaluates a value to a value in the WHNF.
  ///
  /// It does performs reductions.
  pub fn eval(self, env: &Environment) -> Type {
    use crate::ast::Term::*;

    /// Evaluates an application
    fn app(callee: Type, value: Type) -> Type {
      match callee {
        Type(_, Value::Lam(_, lam)) => lam.apply(value),
        Type(location, Value::Flexible(meta, mut spine)) => {
          spine.push_back(value);

          Type(location, Value::Flexible(meta, spine))
        }
        Type(location, Value::Rigid(lvl, mut spine)) => {
          spine.push_back(value);

          Type(location, Value::Rigid(lvl, spine))
        }
        _ => unreachable!(),
      }
    }

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
      Apply(e) => return app(e.callee.eval(env), e.arguments.eval(env)),
      Reference(crate::quoting::Reference::Var(Ix(ix))) => return env.data[ix].clone(),
      Reference(crate::quoting::Reference::MetaVar(meta)) => match meta.take() {
        Some(value) => return value,
        None => return Type::flexible(meta),
      },
      Anno(anno) => anno.value.eval(env).value(),
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
