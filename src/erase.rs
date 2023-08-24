use crate::{
    ast::{
        state::State, Apply, Case, Definition, Domain, Element, Elim, Error, Fun, Hole, Int,
        Pattern, Pi, Str, Term, Universe,
    },
    pass::resolver::Resolved,
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
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum BD {
    Bound,
    Defined,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct MetaVar(pub usize);

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Reference {
    Var(Ix),
    MetaVar(MetaVar),
    InsertedMeta(MetaVar, im_rc::Vector<BD>),
}

impl<S: State<Meta = ()>> Element<S> for Reference {
    fn meta(&self) -> &S::Meta {
        &()
    }
}

/// Defines a debruijin index.
#[derive(Debug, Clone, Hash, Copy, PartialEq, Eq, PartialOrd, Ord)]
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
    pub fn erase(self) -> crate::ast::Term<Erased> {
        match self {
            Term::Group(_) => unreachable!(),
            Term::Error(error) => Term::Error(Error { meta: (), ..error }),
            Term::Universe(_) => Term::Universe(Universe { meta: () }),
            Term::Hole(_) => Term::Hole(Hole { meta: () }),
            Term::Int(v) => Term::Int(Int { meta: (), ..v }),
            Term::Str(v) => Term::Str(Str { meta: (), ..v }),
            Term::Elim(elim) => Term::Elim(Elim {
                meta: (),
                patterns: elim
                    .patterns
                    .into_iter()
                    .map(|pattern| Case {
                        meta: (),
                        patterns: pattern
                            .patterns
                            .into_iter()
                            .map(|pattern| pattern.erase())
                            .collect(),
                        value: pattern.value.erase().into(),
                    })
                    .collect(),
            }),
            Term::Fun(fun) => Term::Fun(Fun {
                meta: (),
                arguments: Definition::new(fun.arguments.text.clone()),
                value: fun.value.erase().into(),
            }),
            Term::Apply(apply) => {
                apply
                    .arguments
                    .into_iter()
                    .fold(apply.callee.erase(), |acc, argument| {
                        Term::Apply(Apply {
                            meta: (),
                            callee: acc.into(),
                            arguments: argument.erase().into(),
                        })
                    })
            }
            Term::Pi(pi) => Term::Pi(Pi {
                meta: (),
                icit: pi.icit,
                domain: Domain {
                    icit: pi.domain.icit,
                    meta: (),
                    type_repr: pi.domain.type_repr.erase().into(),
                    name: Definition::new(pi.domain.name.text.clone()),
                },
                codomain: pi.codomain.erase().into(),
            }),
            Term::Reference(_) => todo!(),
        }
    }
}
