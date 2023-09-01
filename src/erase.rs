use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::{
        state::State, Anno, Apply, Case, Definition, Domain, Element, Elim, Error, Fun, Hole, Int,
        Pattern, Pi, Str, Term, Universe,
    },
    passes::{
        elab::{Elab, Value},
        resolver::Resolved,
    },
};

/// Represents the resolved state, it's the state of the syntax tree when it's resolved.
#[derive(Default, Debug, Clone)]
pub struct Erased;

impl State for Erased {
    type Parameters = Self::Definition;
    type Arguments = Box<Term<Self>>;
    type Definition = Definition<Erased>;
    type Reference = Reference;
    type Import = !;
    type Meta = ();
    type Anno = Anno<Self>;
}

#[derive(Debug, Clone)]
pub enum MetaHole {
    Defined(Value),
    Nothing(usize),
}

#[derive(Debug, Clone)]
pub struct MetaVar(pub Rc<RefCell<MetaHole>>);

impl MetaVar {
    pub fn new_unique(elab: &Elab) -> Self {
        let unique = elab.unique.get();
        elab.unique.update(|x| x + 1);
        Self(Rc::new(RefCell::new(MetaHole::Nothing(unique))))
    }

    pub fn new(value: Value) -> Self {
        Self(Rc::new(RefCell::new(MetaHole::Defined(value))))
    }

    pub fn update(&self, value: Value) {
        *self.0.borrow_mut() = MetaHole::Defined(value)
    }

    pub fn take(&self) -> Value {
        match &*self.0.borrow() {
            MetaHole::Defined(value) => value.clone(),
            MetaHole::Nothing(_) => panic!("MetaVar is not defined"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Reference {
    Var(Ix),
    MetaVar(MetaVar),
}

impl<S: State<Meta = ()>> Element<S> for Reference {
    fn meta(&self) -> &S::Meta {
        &()
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
    pub fn erase(self) -> crate::ast::Pattern<Erased> {
        Pattern {
            meta: (),
            constructor: Reference::Var(Ix(0)),
            arguments: self
                .arguments
                .into_iter()
                .map(|argument| Definition::new(argument.text.clone()))
                .collect(),
        }
    }
}

impl Term<Resolved> {
    /// Erase a term to a term in the untyped lambda calculus.
    pub fn erase(self, elab: &Elab) -> crate::ast::Term<Erased> {
        match self {
            Term::Group(_) => unreachable!(),
            Term::Universe(_) => Term::Universe(Universe { meta: () }),
            Term::Hole(_) => Term::Hole(Hole { meta: () }),
            Term::Int(v) => Term::Int(Int { meta: (), ..v }),
            Term::Str(v) => Term::Str(Str { meta: (), ..v }),
            Term::Error(v) => Term::Error(Error {
                meta: (),
                full_text: v.full_text,
                message: v.message,
            }),
            Term::Anno(v) => Term::Anno(Anno {
                meta: (),
                value: v.value.erase(elab).into(),
                type_repr: v.type_repr.erase(elab).into(),
            }),
            Term::Elim(elim) => Term::Elim(Elim {
                meta: (),
                scrutinee: elim.scrutinee.erase(elab).into(),
                patterns: elim
                    .patterns
                    .into_iter()
                    .map(|pattern| Case {
                        meta: (),
                        pattern: pattern.pattern.erase(),
                        value: pattern.value.erase(elab).into(),
                    })
                    .collect(),
            }),
            Term::Fun(fun) => Term::Fun(Fun {
                meta: (),
                arguments: Definition::new(fun.arguments.text.clone()),
                value: fun.value.erase(elab).into(),
            }),
            Term::Apply(apply) => {
                apply
                    .arguments
                    .into_iter()
                    .fold(apply.callee.erase(elab), |acc, argument| {
                        Term::Apply(Apply {
                            meta: (),
                            callee: acc.into(),
                            arguments: argument.erase(elab).into(),
                        })
                    })
            }
            Term::Pi(pi) => Term::Pi(Pi {
                meta: (),
                icit: pi.icit,
                domain: Domain {
                    icit: pi.domain.icit,
                    meta: (),
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
