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
        Type(box Value::Lam(_, lam), _) => lam.apply(value),
        Type(box Value::Flexible(meta, mut spine), location) => {
          spine.push_back(value);

          Type::new(location, Value::Flexible(meta, spine))
        }
        Type(box Value::Rigid(lvl, mut spine), location) => {
          spine.push_back(value);

          Type::new(location, Value::Rigid(lvl, spine))
        }
        _ => unreachable!(),
      }
    }

    Type::new(self.meta().clone(), match self {
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
      Reference(crate::erase::Reference::Var(Ix(ix))) => return env.data[ix].clone(),
      Reference(crate::erase::Reference::MetaVar(meta)) => match meta.take() {
        Some(value) => return value,
        None => return Type::flexible(meta),
      },
      Anno(anno) => {
        let value = anno.value.eval(env);
        let type_repr = anno.type_repr.eval(env);

        Value::Anno(value, type_repr)
      }
      Pi(pi) => {
        let name = Definition::new(pi.domain.name.text);
        let domain = pi.domain.type_repr.eval(env);
        let codomain = Closure {
          env: env.clone(),
          term: *pi.codomain,
        };

        Value::Pi(name, pi.domain.icit, domain, codomain)
      }
    })
  }
}
