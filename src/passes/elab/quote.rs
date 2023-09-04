use super::{*, Type};

pub trait Quote {
  /// Quote a value to an expression
  fn quote(&self, nth: Lvl) -> Expr;
}

impl Quote for Value {
  fn quote(&self, nth: Lvl) -> Expr {
    /// Applies quoting for a spine of applications in
    /// a term.
    fn quote_sp(sp: Spine, term: Expr, nth: Lvl) -> Expr {
      if sp.is_empty() {
        return term;
      }

      let u = sp.last().cloned().unwrap();
      let len = sp.len();
      let sp = sp.into_iter().skip(len - 1).collect();

      Expr::Apply(Apply {
        callee: quote_sp(sp, term, nth).into(),
        arguments: u.quote(nth).into(),
        meta: Default::default(),
      })
    }

    match self.clone() {
      Value::Flexible(meta, sp) => quote_sp(sp, Expr::Reference(crate::erase::Reference::MetaVar(meta)), nth),
      Value::Rigid(lvl, sp) => quote_sp(sp, Expr::Reference(crate::erase::Reference::Var(nth.into_ix(lvl))), nth),
      Value::Prim(kind) => Expr::Prim(Prim {
        kind,
        meta: Default::default(),
      }),
      Value::Int(value) => Expr::Int(Int {
        value,
        meta: Default::default(),
      }),
      Value::Str(value) => Expr::Str(Str {
        value,
        meta: Default::default(),
      }),
      Value::Lam(name, closure) => Expr::Fun(Fun {
        arguments: Definition::new(name.text),
        value: closure.apply(Type::rigid(nth)).quote(nth + 1).into(),
        meta: Default::default(),
      }),
      Value::Pi(name, icit, domain, codomain) => Expr::Pi(Pi {
        icit,
        domain: Domain {
          name: Definition::new(name.text),
          type_repr: domain.quote(nth).into(),
          icit,
          meta: Default::default(),
        },
        codomain: codomain.apply(Type::rigid(nth)).quote(nth + 1).into(),
        meta: Default::default(),
      }),
      Value::Anno(value, type_repr) => Expr::Anno(Anno {
        value: value.quote(nth).into(),
        type_repr: type_repr.quote(nth).into(),
        meta: Default::default(),
      }),
    }
  }
}