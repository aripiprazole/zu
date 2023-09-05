use crate::ast::Icit;
use crate::ast::PrimKind;
use crate::ast::Term;
use crate::nfe::Apply;
use crate::nfe::Delim;
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
  fn bind(&self, name: &str) -> Show {
    let mut names = self.names.clone();
    names.push(name.to_string());

    Show {
      prec: self.prec,
      lvl: self.lvl + 1,
      names,
    }
  }

  fn show(&self, term: Term<Quoted>) -> Nfe {
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
      Term::Anno(anno) => Nfe::Apply(Apply {
        values: vec![self.show(*anno.value), self.show(*anno.type_repr)],
        sep: Sep::Colon,
        ..Default::default()
      }),
      Term::Fun(fun) => Nfe::Apply(Apply {
        values: vec![
          Nfe::S(fun.arguments.text.clone()),
          self.bind(&fun.arguments.text).show(*fun.value),
        ],
        sep: Sep::ArrL,
        ..Default::default()
      }),
      Term::Elim(_) => todo!(),
      Term::Int(v) => Nfe::S(format!("{}", v.value)),
      Term::Str(v) => Nfe::S(format!("\"{}\"", v.value)),
      Term::Reference(Reference::Global(m)) => Nfe::S(m.definition.text.clone()),
      Term::Reference(Reference::MetaVar(m)) => match m.get() {
        MetaHole::Defined(value) => self.show(value.quote(self.lvl)),
        MetaHole::Nothing => Nfe::S("?".into()),
      },
      Term::Reference(Reference::Var(Ix(ix))) => Nfe::S(self.names[ix].clone()),
      Term::Pi(pi) => Nfe::Apply(Apply {
        values: vec![
          Nfe::Apply(Apply {
            values: vec![Nfe::S(pi.domain.name.text.clone()), self.show(*pi.domain.type_repr)],
            sep: Sep::Colon,
            delim: match pi.domain.icit {
              Icit::Expl => Delim::Paren,
              Icit::Impl => Delim::Brace,
            },
            ..Default::default()
          }),
          self.bind(&pi.domain.name.text).show(*pi.codomain),
        ],
        sep: Sep::ArrL,
        ..Default::default()
      }),
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

    show.show(self.clone().quote(elab.lvl))
  }
}
