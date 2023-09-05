use super::*;

/// Global definition
pub type Global = crate::passes::resolver::Reference;

/// A global declaration.
#[derive(Debug, Clone)]
pub struct Declaration {
  pub name: String,
  pub type_repr: Type,
  pub value: Type,
}

#[derive(Default, Debug, Clone)]
pub struct GlobalEnvironment {
  pub data: im_rc::HashMap<String, Rc<Declaration>>,
}

impl GlobalEnvironment {
  pub fn lookup(&self, name: &str) -> Rc<Declaration> {
    self.data.get(name).unwrap().clone()
  }
}

/// Everything is ok.
impl PartialEq for GlobalEnvironment {
  fn eq(&self, _: &Self) -> bool {
    true
  }
}
