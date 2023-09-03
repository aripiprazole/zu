use crate::ast::PrimKind;
use crate::ast::Term;
use crate::erase::Erased;
use crate::erase::Ix;
use crate::erase::Lvl;
use crate::erase::MetaHole;
use crate::erase::Reference;
use crate::nfe::Nfe;
use crate::passes::elab::quote::Quote;
use crate::passes::elab::Elab;
use crate::passes::elab::Value;

/// The context we need to pretty print a type right now.
///
/// It's a list of names, and the level of the last bound variable.
struct Show {
  prec: u8,
  lvl: Lvl,
  names: Vec<String>,
}

impl Show {
  fn build(&self, term: Term<Erased>) -> Nfe {
    let _ = self.prec;

    match term {
      // Removed
      Term::Group(_) => todo!(),

      // Values
      Term::Pi(_) => todo!(),
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
        sep: crate::nfe::Sep::Colon,
        delim: crate::nfe::Delim::default(),
        disposal: crate::nfe::Disposal::Horizontal,
      },
      Term::Fun(_) => todo!(),
      Term::Elim(_) => todo!(),
      Term::Int(v) => Nfe::S(format!("{}", v.value)),
      Term::Str(v) => Nfe::S(format!("\"{}\"", v.value)),
      Term::Reference(Reference::MetaVar(m)) => match m.get() {
        MetaHole::Defined(value) => self.build(value.quote(self.lvl)),
        MetaHole::Nothing(n) => Nfe::S(format!("?{n:?}")),
      },
      Term::Reference(Reference::Var(Ix(ix))) => Nfe::S(self.names[ix].clone()),
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
