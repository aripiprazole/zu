use super::*;

/// Defines a spine, it's a list of arguments that can be applied to
/// either a rigid or a flexible variable.
pub type Spine = im_rc::Vector<Type>;

/// Defines the type of a term, elaborated to a value
///
/// The type of a term is a value, but the type of a value is a type.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
  Int(isize),
  Str(String),
  Prim(PrimKind),
  Flexible(MetaVar, Spine),
  Rigid(Lvl, Spine),
  Lam(DefinitionRs, Closure),
  Pi(DefinitionRs, Icit, Box<Type>, Closure),
}

/// Defines a type that can be evaluated, it's a value that is
/// in quoted form, and it's not evaluated yet.
///
/// The goal is evaluating by using the [`Closure::eval`] function
/// that will bind a value to the environment.
#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
  pub env: Environment,
  pub term: Expr,
}

impl Closure {
  /// Binds a closure with a new environment with the
  /// given value.
  pub fn apply(self, argument: Type) -> Type {
    self.term.eval(&self.env.create_new_value(argument))
  }
}

/// Type that holds all the information about a type, just like if
/// it's handwritten, synthesized, etc.
#[derive(Debug, Clone, PartialEq)]
pub struct Type(pub Location, pub Value);

impl Type {
  /// Gets the location of type
  #[inline(always)]
  pub fn location(&self) -> Location {
    self.0.clone()
  }

  /// Gets the value of the type
  #[inline(always)]
  pub fn value(&self) -> Value {
    self.1.clone()
  }
}

impl Type {
  /// Creates a type without location
  #[inline(always)]
  pub fn synthesized(value: Value) -> Self {
    Self(SYNTHESIZED.clone(), value)
  }

  /// Creates a new type universe
  #[inline(always)]
  pub fn universe() -> Self {
    Type::synthesized(Value::Prim(PrimKind::Universe))
  }

  /// Closes a type, it does creates a closure from a type
  pub fn close(&self, ctx: &Elab) -> Closure {
    Closure {
      env: ctx.env.clone(),
      term: self.quote(ctx.lvl + 1),
    }
  }

  /// Creates a rigid variable without applications and a
  /// spine to it
  #[inline(always)]
  pub fn rigid(lvl: Lvl) -> Self {
    Type::synthesized(Value::Rigid(lvl, Default::default()))
  }

  /// Creates a flexible variable without applications and a
  /// spine to it
  #[inline(always)]
  pub fn flexible(meta: MetaVar) -> Self {
    Type::synthesized(Value::Flexible(meta, Default::default()))
  }

  /// Creates a new pi type
  #[inline(always)]
  pub fn pi(name: &str, domain: Type, codomain: Closure) -> Self {
    let name = Definition::new(name.to_string());

    Type::synthesized(Value::Pi(name, Icit::Expl, domain.into(), codomain))
  }

  /// Function apply, it does applies a value to a value
  /// creating a new value.
  pub fn apply(self, argument: Type) -> Self {
    match self {
      Type(_, Value::Lam(_, closure)) => closure.apply(argument),
      Type(location, Value::Flexible(meta, mut spine)) => {
        spine.push_back(argument);
        Type(location, Value::Flexible(meta, spine))
      }
      Type(location, Value::Rigid(lvl, mut spine)) => {
        spine.push_back(argument);
        Type(location, Value::Rigid(lvl, spine))
      }
      _ => {
        log::error!("expected a function, got another value: {:?}", self.1);
        self
      }
    }
  }
}

impl Deref for Type {
  type Target = Value;

  fn deref(&self) -> &Self::Target {
    &self.1
  }
}

/// Create a new spine normal form
///
/// It does creates a new spine normal form from a value.
pub fn unspine(value: Type, spine: Spine) -> Type {
  spine.into_iter().fold(value, |value, argument| value.apply(argument))
}
