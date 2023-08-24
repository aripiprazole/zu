use crate::ast::{
    quoted::Ix, Apply, Case, Definition, Domain, Elim, Error, Fun, Hole, Int, Pattern, Pi, Str,
    Term, Universe,
};

impl Pattern<crate::ast::state::Resolved> {
    /// Erase a term to a term in the untyped lambda calculus.
    pub fn erase(self) -> crate::ast::Pattern<crate::ast::state::Quoted> {
        Pattern {
            meta: (),
            constructor: crate::ast::quoted::Reference::Var(Ix(0)),
            arguments: self
                .arguments
                .into_iter()
                .map(|argument| Definition::new(argument.text.clone()))
                .collect(),
        }
    }
}

impl Term<crate::ast::state::Resolved> {
    /// Erase a term to a term in the untyped lambda calculus.
    pub fn erase(self) -> crate::ast::Term<crate::ast::state::Quoted> {
        match self {
            Term::Error(error) => Term::Error(Error { meta: (), ..error }),
            Term::Universe(_) => Term::Universe(Universe { meta: () }),
            Term::Hole(_) => Term::Hole(Hole { meta: () }),
            Term::Int(v) => Term::Int(Int { meta: (), ..v }),
            Term::Str(v) => Term::Str(Str { meta: (), ..v }),
            Term::Group(box v) => v.erase(),
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
