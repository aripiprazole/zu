use super::*;

/// Global definition
pub type Global = crate::passes::resolver::Reference;

/// A global declaration.
#[derive(Debug, Clone)]
pub struct Declaration {
  pub name: Rc<Definition<Resolved>>,
  pub type_repr: Type,
  pub value: Type,
}

#[derive(Default, Debug, Clone)]
pub struct GlobalEnvironment {
  pub data: Rc<RefCell<std::collections::HashMap<String, Rc<Declaration>>>>,
}

impl GlobalEnvironment {
  pub fn lookup(&self, name: &str) -> Rc<Declaration> {
    self.data.borrow().get(name).unwrap().clone()
  }

  pub fn lookup_value(&self, name: &str) -> Type {
    self.lookup(name).value.clone()
  }

  pub fn insert(&mut self, name: String, value: Declaration) {
    self.data.borrow_mut().insert(name, value.into());
  }
}

/// Everything is ok.
impl PartialEq for GlobalEnvironment {
  fn eq(&self, _: &Self) -> bool {
    true
  }
}
