use crate::ast::Icit;
use crate::ast::PrimKind;
use crate::ast::Term;
use crate::nfe::Delim;
use crate::nfe::Disposal;
use crate::nfe::Nfe;
use crate::nfe::Sep;
use crate::passes::elab::quote::Quote;
use crate::passes::elab::Elab;
use crate::passes::elab::Value;
use crate::quoting::Ix;
use crate::quoting::Lvl;
use crate::quoting::MetaHole;
use crate::quoting::Quoted;
use crate::quoting::Reference;

/// The context we need to pretty print a type right now.
///
/// It's a list of names, and the level of the last bound variable.
struct Show {
  prec: u8,
  lvl: Lvl,
  names: Vec<String>,
}

impl Show {
  fn build(&self, term: Term<Quoted>) -> Nfe {
    let _ = self.prec;

    match term {
      // Removed
      Term::Group(_) => todo!(),

      // Values
      Term::Hole(_) => todo!(),
      Term::Apply(_) => todo!(),
      Term::Error(_) => todo!(),
      Term::Prim(prim) => match prim.kind {
        PrimKind::String => Nfe::S("String".to_string()),
        PrimKind::Int => Nfe::S("Int".to_string()),
        PrimKind::Universe => Nfe::S("*".to_string()),
      },
      Term::Anno(anno) => Nfe::Apply {
        values: vec![self.build(*anno.value), self.build(*anno.type_repr)],
        sep: Sep::Colon,
        delim: Delim::default(),
        disposal: Disposal::Horizontal,
      },
      Term::Fun(fun) => Nfe::Apply {
        values: vec![Nfe::S(fun.arguments.text.clone()), self.build(*fun.value)],
        sep: Sep::ArrL,
        delim: Delim::None,
        disposal: Disposal::Horizontal,
      },
      Term::Elim(_) => todo!(),
      Term::Int(v) => Nfe::S(format!("{}", v.value)),
      Term::Str(v) => Nfe::S(format!("\"{}\"", v.value)),
      Term::Reference(Reference::Global(m)) => Nfe::S(m.definition.text.clone()),
      Term::Reference(Reference::MetaVar(m)) => match m.get() {
        MetaHole::Defined(value) => self.build(value.quote(self.lvl)),
        MetaHole::Nothing => Nfe::S("?".into()),
      },
      Term::Reference(Reference::Var(Ix(ix))) => Nfe::S(self.names[ix].clone()),
      Term::Pi(pi) => Nfe::Apply {
        values: vec![
          Nfe::Apply {
            values: vec![Nfe::S(pi.domain.name.text.clone()), self.build(*pi.domain.type_repr)],
            sep: Sep::Colon,
            delim: match pi.domain.icit {
              Icit::Expl => Delim::Paren,
              Icit::Impl => Delim::Brace,
            },
            disposal: Disposal::Horizontal,
          },
          self.build(*pi.codomain),
        ],
        delim: Delim::None,
        sep: Sep::ArrL,
        disposal: Disposal::Horizontal,
      },
    }
  }
}

impl Value {
  /// Show a value as a string. As the debruijin level 0 is the last bound variable, we need to
  /// pass the size of the environment.
  pub fn show(&self, elab: &Elab) -> Nfe {
    let show = Show {
      prec: 0,
      lvl: elab.lvl,
      names: elab.types.clone().into_iter().map(|(k, _)| k).collect(),
    };

    show.build(self.clone().quote(elab.lvl))
  }
}
