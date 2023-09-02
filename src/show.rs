use crate::ast::Term;
use crate::erase::Erased;
use crate::erase::Ix;
use crate::erase::Reference;
use crate::nfe::Nfe;
use crate::passes::elab::Elab;
use crate::passes::elab::Quote;
use crate::passes::elab::Value;

/// The context we need to pretty print a type right now.
///
/// It's a list of names, and the level of the last bound variable.
struct Show {
  prec: u8,
  names: Vec<String>,
  term: Term<Erased>,
}

impl Show {
  fn build(self) -> Nfe {
    let _ = self.prec;

    match self.term {
      // Removed
      Term::Group(_) => todo!(),

      // Values
      Term::Pi(_) => todo!(),
      Term::Hole(_) => todo!(),
      Term::Apply(_) => todo!(),
      Term::Error(_) => todo!(),
      Term::Prim(_) => todo!(),
      Term::Anno(_) => todo!(),
      Term::Fun(_) => todo!(),
      Term::Elim(_) => todo!(),
      Term::Int(v) => Nfe::S(format!("{}", v.value)),
      Term::Str(v) => Nfe::S(format!("\"{}\"", v.value)),
      Term::Reference(Reference::MetaVar(_)) => todo!(),
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
      names: elab.types.clone().into_iter().map(|(k, _)| k).collect(),
      term: self.clone().quote(elab.lvl),
    };

    show.build()
  }
}
