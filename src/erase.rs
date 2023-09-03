use std::cell::RefCell;
use std::rc::Rc;

use nonempty::NonEmpty;

use crate::ast::state::State;
use crate::ast::Anno;
use crate::ast::Apply;
use crate::ast::Case;
use crate::ast::Definition;
use crate::ast::Domain;
use crate::ast::Element;
use crate::ast::Elim;
use crate::ast::Error;
use crate::ast::Fun;
use crate::ast::Hole;
use crate::ast::Int;
use crate::ast::Pattern;
use crate::ast::Pi;
use crate::ast::Prim;
use crate::ast::Str;
use crate::ast::Term;
use crate::passes::elab::Elab;
use crate::passes::elab::Value;
use crate::passes::resolver::Resolved;

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Empty;

/// Represents the resolved state, it's the state of the syntax tree when it's resolved.
#[derive(Default, Debug, Clone, PartialEq)]
pub struct Erased;

impl State for Erased {
  type Anno = Anno<Self>;
  type Arguments = Box<Term<Self>>;
  type Definition = Definition<Erased>;
  type Import = !;
  type Meta = Empty;
  type Parameters = Self::Definition;
  type Reference = Reference;
}

#[derive(Debug, Clone, PartialEq)]
pub enum MetaHole {
  Defined(Value),
  Nothing(Lvl),
}

#[derive(Debug, Clone)]
pub struct MetaVar(pub Rc<RefCell<MetaHole>>);

impl PartialEq for MetaVar {
  fn eq(&self, other: &Self) -> bool {
    self.take() == other.take()
  }
}

impl MetaVar {
  pub fn new_unique(elab: &Elab) -> Self {
    Self(Rc::new(RefCell::new(MetaHole::Nothing(elab.lvl))))
  }

  pub fn new(value: Value) -> Self {
    Self(Rc::new(RefCell::new(MetaHole::Defined(value))))
  }

  pub fn update(&self, value: Value) {
    *self.0.borrow_mut() = MetaHole::Defined(value)
  }

  pub fn get(&self) -> MetaHole {
    self.0.borrow().clone()
  }

  pub fn take(&self) -> Option<Value> {
    match &*self.0.borrow() {
      MetaHole::Defined(value) => value.clone().into(),
      MetaHole::Nothing(_) => None,
    }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Reference {
  Var(Ix),
  MetaVar(MetaVar),
}

impl<S: State<Meta = Empty>> Element<S> for Reference {
  fn meta(&self) -> &S::Meta {
    &Empty
  }
}

/// Defines a debruijin index.
#[derive(Default, Debug, Clone, Hash, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Lvl(usize);

impl Lvl {
  /// Transforms a level into a debruijin index.
  pub fn into_ix(&self, Lvl(lvl0): Lvl) -> Ix {
    let Lvl(lvl1) = self;

    Ix(lvl1 - lvl0 - 1)
  }
}

impl std::ops::Add<usize> for Lvl {
  type Output = Self;

  fn add(self, rhs: usize) -> Self::Output {
    Self(self.0 + rhs)
  }
}

impl std::ops::AddAssign<usize> for Lvl {
  fn add_assign(&mut self, rhs: usize) {
    self.0 += rhs
  }
}

impl<S: State<Meta = ()>> Element<S> for Lvl {
  fn meta(&self) -> &S::Meta {
    &()
  }
}

/// Defines a debruijin index.
#[derive(Debug, Clone, Hash, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Ix(pub usize);

impl std::ops::Add<usize> for Ix {
  type Output = Self;

  fn add(self, rhs: usize) -> Self::Output {
    Self(self.0 + rhs)
  }
}

impl std::ops::AddAssign<usize> for Ix {
  fn add_assign(&mut self, rhs: usize) {
    self.0 += rhs
  }
}

impl<S: State<Meta = ()>> Element<S> for Ix {
  fn meta(&self) -> &S::Meta {
    &()
  }
}

impl Pattern<Resolved> {
  /// Erase a term to a term in the untyped lambda calculus.
  pub fn erase(self, ctx: &mut Elab) -> Box<crate::ast::Pattern<Erased>> {
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
  pub fn erase(self, elab: &Elab) -> crate::ast::Term<Erased> {
    match self {
      Term::Group(_) => unreachable!(),
      Term::Prim(u) => Term::Prim(Prim { kind: u.kind, meta: Default::default() }),
      Term::Hole(_) => Term::Hole(Hole { meta: Default::default() }),
      Term::Int(v) => Term::Int(Int { meta: Default::default(), ..v }),
      Term::Str(v) => Term::Str(Str { meta: Default::default(), ..v }),
      Term::Error(v) => Term::Error(Error {
        meta: Default::default(),
        full_text: v.full_text,
        message: v.message,
      }),
      Term::Anno(v) => Term::Anno(Anno {
        meta: Default::default(),
        value: v.value.erase(elab).into(),
        type_repr: v.type_repr.erase(elab).into(),
      }),
      Term::Elim(elim) => Term::Elim(Elim {
        meta: Default::default(),
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
              meta: Default::default(),
              pattern: NonEmpty::from_vec(case.pattern.into_iter().map(|value| value.erase(&mut local)).collect())
                .unwrap(),
              value: case.value.erase(&local).into(),
            }
          })
          .collect(),
      }),
      Term::Fun(fun) => Term::Fun(Fun {
        meta: Default::default(),
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
            meta: Default::default(),
            callee: acc.into(),
            arguments: argument.erase(elab).into(),
          })
        }),
      Term::Pi(pi) => Term::Pi(Pi {
        meta: Default::default(),
        icit: pi.icit,
        domain: Domain {
          icit: pi.domain.icit,
          meta: Default::default(),
          type_repr: pi.domain.type_repr.erase(elab).into(),
          name: Definition::new(pi.domain.name.text.clone()),
        },
        codomain: pi.codomain.erase(elab).into(),
      }),
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
