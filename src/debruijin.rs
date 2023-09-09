/// Defines a debruijin level. It does represent the level of the context/environment
/// 
/// It can be transformed into a debruijin index by using the [`Lvl::into_ix`] method.
#[derive(Default, Debug, Clone, Hash, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Lvl(pub usize);

impl Lvl {
  /// Transforms a level into a debruijin index.
  pub fn as_ix(&self, Lvl(x): Lvl) -> Ix {
    let Lvl(l) = *self;
    assert!(l > x, "l > x, but {l} < {x}");
    assert!(l > 0, "l should be greater than 0");

    Ix(l - x - 1)
  }
}

impl std::ops::Add<usize> for Lvl {
  type Output = Self;

  fn add(self, rhs: usize) -> Self::Output {
    Self(self.0 + rhs)
  }
}

impl std::ops::AddAssign<usize> for Lvl {
  fn add_assign(&mut self, rhs: usize) {
    self.0 += rhs
  }
}

/// Defines a debruijin index. That can be converted by two levels.
/// 
/// It's used to represent a variable in the syntax tree.
#[derive(Debug, Clone, Hash, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Ix(pub usize);

impl std::ops::Add<usize> for Ix {
  type Output = Self;

  fn add(self, rhs: usize) -> Self::Output {
    Self(self.0 + rhs)
  }
}

impl std::ops::AddAssign<usize> for Ix {
  fn add_assign(&mut self, rhs: usize) {
    self.0 += rhs
  }
}