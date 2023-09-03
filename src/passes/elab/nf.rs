use super::*;

impl Expr {
  /// Evaluates a value to a value in the WHNF.
  ///
  /// It does performs reductions.
  pub fn eval(self, env: &Environment) -> Value {
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

    match self {
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
      Apply(e) => app(e.callee.eval(env), e.arguments.eval(env)),
      Reference(crate::erase::Reference::Var(Ix(ix))) => env.data[ix].clone(), /* TODO: HANDLE ERROR */
      Reference(crate::erase::Reference::MetaVar(meta)) => match meta.take() {
        Some(value) => value,
        None => Value::flexible(meta),
      },
      Anno(anno) => {
        let value = anno.value.eval(env);
        let type_repr = anno.type_repr.eval(env);

        Value::Anno(value.into(), type_repr.into())
      }
      Pi(pi) => {
        let name = Definition::new(pi.domain.name.text);
        let domain = pi.domain.type_repr.eval(env);
        let codomain = Closure {
          env: env.clone(),
          term: *pi.codomain,
        };

        Value::Pi(name, pi.domain.icit, domain.into(), codomain)
      }
    }
  }
}