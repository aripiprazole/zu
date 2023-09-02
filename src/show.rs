use crate::ast::Term;
use crate::erase::Erased;
use crate::erase::Lvl;
use crate::nfe::Nfe;
use crate::passes::elab::Elab;
use crate::passes::elab::Quote;
use crate::passes::elab::Value;

/// The context we need to pretty print a type right now.
///
/// It's a list of names, and the level of the last bound variable.
struct Show {
  lvl: Lvl,
  names: Vec<String>,
  term: Term<Erased>,
}

impl Show {
  fn build(self) -> Nfe {
    let _ = self.lvl;
    let _ = self.names;
    let _ = self.term;
    todo!()
  }
}

impl Value {
  /// Show a value as a string. As the debruijin level 0 is the last bound variable, we need to
  /// pass the size of the environment.
  pub fn show(&self, elab: &Elab) -> Nfe {
    let show = Show {
      lvl: elab.lvl,
      names: elab.types.clone().into_iter().map(|(k, _)| k).collect(),
      term: self.clone().quote(elab.lvl),
    };

    show.build()
  }
}
