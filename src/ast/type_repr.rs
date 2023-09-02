use super::*;
use crate::passes::parser::Parsed;

/// The type of constructor primitive.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PrimKind {
  String,
  Int,
  #[default]
  Universe,
}

/// Type of a type. It has a location.
#[derive(Default, Debug, Clone)]
pub struct Prim<S: state::State> {
  pub kind: PrimKind,
  pub meta: S::Meta,
}

pub use mk_prim::*;

/// Its a grouping module that contains functions to create a primitive.
mod mk_prim {
  use super::*;

  pub fn make_universe(meta: Location) -> Prim<Parsed> {
    Prim {
      kind: PrimKind::Universe,
      meta,
    }
  }

  pub fn make_int(meta: Location) -> Prim<Parsed> {
    Prim {
      kind: PrimKind::Int,
      meta,
    }
  }

  pub fn make_string(meta: Location) -> Prim<Parsed> {
    Prim {
      kind: PrimKind::String,
      meta,
    }
  }
}

impl<S: state::State> Element<S> for Prim<S> {
  fn meta(&self) -> &S::Meta {
    &self.meta
  }
}

/// A hole. It has a location.
#[derive(Default, Debug, Clone)]
pub struct Hole<S: state::State> {
  /// The location of the integer in the source code.
  pub meta: S::Meta,
}

impl<S: state::State> Element<S> for Hole<S> {
  fn meta(&self) -> &S::Meta {
    &self.meta
  }
}

/// A variable. It has a name and a location.
#[derive(Debug, Clone)]
pub struct Domain<S: state::State> {
  /// The name of the variable. The idea of the [`Option`] type, is when
  /// we have a binder like `_`, which is a placeholder for a variable for
  /// which we don't care about the name.
  pub name: S::NameSet,

  /// The type of the variable. If it's in an implicit argument position,
  /// it will fallback to the type `type`.
  pub type_repr: Box<Term<S>>,

  /// If the variable binds something implicitly or explicitly.
  pub icit: Icit,

  /// The location of the variable in the source code.
  pub meta: S::Meta,
}

impl<S: state::State> Element<S> for Domain<S> {
  fn meta(&self) -> &S::Meta {
    &self.meta
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Icit {
  /// Explicit binder `(x : A)`.
  Expl,

  /// Implicit binder `{x : A}`.
  Impl,
}

/// A pi type. It has a name, a domain, and a codomain.
///
/// ## Examples
///
/// For example, the pi type `x : A -> B`, where `x` is the name, `A` is the
/// domain, and `B` is the codomain.
///
/// The pi type can be an arrow type too, like `A -> B`
///
/// ## Implicit pi types
///
/// Pi types can be implicit too, like `{x : A} -> B`, where `x` is the name,
/// `A` is the domain, and `B` is the codomain.
#[derive(Debug, Clone)]
pub struct Pi<S: state::State> {
  pub icit: Icit,
  pub domain: Domain<S>,
  pub codomain: Box<Term<S>>,
  pub meta: S::Meta,
}

impl<S: state::State> Element<S> for Pi<S> {
  fn meta(&self) -> &S::Meta {
    &self.meta
  }
}
