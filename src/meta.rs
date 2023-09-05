use std::cell::RefCell;
use std::rc::Rc;

use crate::passes::elab::Type;

#[derive(Default, Debug, Clone, PartialEq)]
pub enum MetaHole {
  Defined(Type),

  #[default]
  Nothing,
}

#[derive(Default, Debug, Clone)]
pub struct MetaVar(pub Rc<RefCell<MetaHole>>);

impl PartialEq for MetaVar {
  fn eq(&self, other: &Self) -> bool {
    self.take() == other.take()
  }
}

impl MetaVar {
  pub fn new(value: Type) -> Self {
    Self(Rc::new(RefCell::new(MetaHole::Defined(value))))
  }

  pub fn update(&self, value: Type) {
    *self.0.borrow_mut() = MetaHole::Defined(value)
  }

  pub fn get(&self) -> MetaHole {
    self.0.borrow().clone()
  }

  pub fn take(&self) -> Option<Type> {
    match &*self.0.borrow() {
      MetaHole::Defined(value) => value.clone().into(),
      MetaHole::Nothing => None,
    }
  }
}
