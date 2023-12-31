use super::*;

impl Expr {
  /// Evaluates a value to a value in the WHNF.
  ///
  /// It does performs reductions.
  #[inline(always)]
  pub fn eval(self, env: &Environment) -> Type {
    eval(self, env)
  }
}

/// Evaluates a value to a value in the WHNF.
fn eval(expr: Expr, env: &Environment) -> Type {
  use crate::ast::Term::*;
  use crate::quoting::Reference::*;

  Type(expr.meta().clone(), match expr {
    // Removed
    Error(_) => unreachable!(),
    Group(_) => unreachable!(),

    // Values
    Hole(_) => Value::Flexible(self::MetaVar::default(), im_rc::vector![]),
    Prim(k) => Value::Prim(k.kind),
    Int(data) => Value::Int(data.value),
    Str(data) => Value::Str(data.value),
    Elim(_) => todo!("elim expr"),
    Fun(e) => Value::Lam(e.arguments.shift(), Closure {
      env: env.clone(),
      term: *e.value,
    }),
    Anno(anno) => return anno.value.eval(env),
    Apply(e) => return e.callee.eval(env).apply(e.arguments.eval(env)),
    Reference(Var(Ix(ix))) => return env.data[ix].clone(),
    Reference(MetaVar(meta)) => {
      return match meta.take() {
        Some(value) => value,
        None => Type::flexible(meta),
      }
    }
    Reference(Global(name)) => return env.globals.lookup_value(name.text()),
    Pi(pi) => {
      let domain = pi.domain.type_repr.eval(env);
      let codomain = Closure {
        env: env.clone(),
        term: *pi.codomain,
      };

      Value::Pi(pi.domain.name.shift(), pi.domain.icit, domain.into(), codomain)
    }
  })
}
