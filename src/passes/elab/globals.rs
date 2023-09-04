use super::*;

/// A global declaration.
#[derive(Debug, Clone)]
pub struct Declaration {
  pub name: String,
  pub type_repr: Type,
  pub value: Type,
}

#[derive(Default, Debug, Clone)]
pub struct GlobalEnvironment {
  pub data: im_rc::HashMap<String, Declaration>,
}

/// Everything is ok.
impl PartialEq for GlobalEnvironment {
  fn eq(&self, _: &Self) -> bool {
    true
  }
}
