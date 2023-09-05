use nonempty::NonEmpty;

use crate::ast::*;
pub use crate::debruijin::*;
pub use crate::meta::*;
use crate::passes::elab::Elab;
use crate::passes::resolver::Definition;
use crate::passes::resolver::Resolved;

/// Represents the resolved state, it's the state of the syntax tree when it's resolved.
#[derive(Default, Debug, Clone, PartialEq)]
pub struct Quoted;

impl State for Quoted {
  type Anno = Anno<Self>;
  type Arguments = Box<Term<Self>>;
  type Definition = Definition<Quoted>;
  type Import = !;
  type Parameters = Self::Definition;
  type Reference = Reference;
}

#[derive(Debug, Clone, PartialEq)]
pub enum Reference {
  Global(crate::passes::resolver::Reference),
  Var(Ix),
  MetaVar(MetaVar),
}

impl<S: State<Meta = Location>> Element<S> for Reference {
  fn meta(&self) -> &S::Meta {
    &SYNTHESIZED
  }
}

impl Pattern<Resolved> {
  /// Erase a term to a term in the untyped lambda calculus.
  pub fn erase(self, ctx: &mut Elab) -> Box<crate::ast::Pattern<Quoted>> {
    Box::new(match self {
      Pattern::Var(n, _) => Pattern::Var(Definition::new(n.text.clone()), Default::default()),
      Pattern::Constructor(n, v, _) => Pattern::Constructor(
        Definition::new(n.text.clone()),
        v.into_iter()
          .map(|pattern| {
            ctx.lvl += 1;

            *pattern.erase(ctx)
          })
          .collect(),
        Default::default(),
      ),
      Pattern::Wildcard(_) => todo!(),
    })
  }
}

impl Term<Resolved> {
  /// Erase a term to a term in the untyped lambda calculus.
  pub fn erase(self, elab: &Elab) -> crate::ast::Term<Quoted> {
    match self {
      Term::Group(_) => unreachable!(),
      Term::Prim(u) => Term::Prim(Prim { ..u }),
      Term::Hole(h) => Term::Hole(Hole { ..h }),
      Term::Int(v) => Term::Int(Int { ..v }),
      Term::Str(v) => Term::Str(Str { ..v }),
      Term::Error(v) => Term::Error(Error { ..v }),
      Term::Anno(v) => Term::Anno(Anno {
        meta: v.meta,
        value: v.value.erase(elab).into(),
        type_repr: v.type_repr.erase(elab).into(),
      }),
      Term::Elim(elim) => Term::Elim(Elim {
        meta: elim.meta,
        scrutinee: NonEmpty::from_vec(
          elim
            .scrutinee
            .into_iter()
            .map(|value| value.erase(elab).into())
            .collect(),
        )
        .unwrap(),
        patterns: elim
          .patterns
          .into_iter()
          .map(|case| {
            // Creates a new local environment for the case, that will increase the level of the
            // environment.
            let mut local = elab.clone();

            Case {
              meta: case.meta,
              pattern: NonEmpty::from_vec(case.pattern.into_iter().map(|value| value.erase(&mut local)).collect())
                .unwrap(),
              value: case.value.erase(&local).into(),
            }
          })
          .collect(),
      }),
      Term::Fun(fun) => Term::Fun(Fun {
        meta: fun.meta,
        arguments: Definition::new(fun.arguments.text.clone()),
        value: fun
          .value
          .erase(&elab.create_new_value(&fun.arguments.text, elab.fresh_meta().eval(&elab.env)))
          .into(),
      }),
      Term::Apply(apply) => apply
        .arguments
        .into_iter()
        .fold(apply.callee.erase(elab), |acc, argument| {
          Term::Apply(Apply {
            meta: argument.meta().clone(),
            callee: acc.into(),
            arguments: argument.erase(elab).into(),
          })
        }),
      Term::Pi(pi) => Term::Pi(Pi {
        meta: pi.meta,
        icit: pi.icit,
        domain: Domain {
          icit: pi.domain.icit,
          meta: pi.domain.meta,
          type_repr: pi.domain.type_repr.erase(elab).into(),
          name: Definition {
            is_global: false,
            meta: pi.domain.name.meta.clone(),
            text: pi.domain.name.text.clone(),
          },
        },
        codomain: pi.codomain.erase(elab).into(),
      }),
      Term::Reference(reference) if reference.definition.is_global => Term::Reference(Reference::Global(reference)),
      Term::Reference(reference) => {
        let mut ix = 0;
        let mut types = elab.types.clone();
        while let Some((name, _)) = types.pop_front() {
          if name == reference.definition.text {
            return Term::Reference(Reference::Var(Ix(ix)));
          }
          ix += 1;
        }

        todo!("reference not found error handling")
      }
    }
  }
}
