use super::Type;
use super::*;

#[derive(miette::Diagnostic, thiserror::Error, Debug, Clone, PartialEq)]
#[diagnostic()]
pub enum UnifyError {
  /// Int value mismatch between two values,
  #[error("expected int value {0} and got {1}")]
  #[diagnostic(url(docsrs), code(unify::int_mismatch))]
  MismatchBetweenInts(isize, isize),

  /// String value mismatch between two values,
  #[error("expected string value {0} and got {1}")]
  #[diagnostic(url(docsrs), code(unify::str_mismatch))]
  MismatchBetweenStrs(String, String),

  /// Unification error between two types
  #[error("expected type {0}, got another {1}")]
  #[diagnostic(url(docsrs), code(unify::cant_unify))]
  CantUnify(Nfe, Nfe, Location, Location),
}

/// Partial renaming from Γ to Δ. It does renames the variables
/// from Γ to Δ.
///
/// It does returns a list of pairs of the variables that were
/// renamed.
pub struct PartialRenaming {
  /// The size of domain Γ.
  pub domain: Lvl,

  /// The size of codomain Δ, that will be mapped from Γ.
  pub codomain: Lvl,

  /// The renaming function, that maps the variables from Γ to Δ.
  pub renames: im_rc::HashMap<Lvl, Lvl>,
}

impl PartialRenaming {
  /// Lifts a partial renaming over an extra bound variable.
  ///
  /// Given (σ : PartialRenaming Γ Δ),
  ///   (lift σ : PartialRenaming (Γ, x : A[σ]) (Δ, x : A)).
  pub fn lift(mut self) -> Self {
    self.domain += 1;
    self.codomain += 1;
    self.renames.insert(self.codomain, self.domain);
    self
  }

  /// invert : (Γ : Cxt) → (spine : Sub Δ Γ) → PartialRenaming Γ Δ@
  pub fn invert(lvl: Lvl, spine: Spine) -> miette::Result<Self, UnifyError> {
    let _ = lvl;
    let _ = spine;
    todo!()
  }

  /// Perform partial renaming on right-most term while searching the occurrences of
  /// the variable to rename.
  pub fn rename(self, m: MetaVar, value: Type) -> miette::Result<Expr, UnifyError> {
    let _ = m;
    let _ = value;
    todo!()
  }
}

/// SECTION: Pattern unification
impl Type {
  /// solve : (Γ : Cxt) → (spine : Sub Δ Γ) → (m : MetaVar) → (lvl : Lvl) → ()
  pub fn solve(self, spine: Spine, m: MetaVar, lvl: Lvl) -> miette::Result<(), UnifyError> {
    // unsound
    m.update(self);
    let _ = spine;
    let _ = lvl;

    // TODO: Make this thing sound
    Ok(())
  }
}

/// SECTION: Forcing
impl Type {
  /// Forcing is important because it does removes the holes created
  /// by the elaborator.
  ///
  /// It does returns a value without holes.
  pub fn force(self) -> Type {
    Type(self.location(), match self.1 {
      Value::Flexible(ref m, ref spine) => match m.take() {
        Some(value) => return unspine(value, spine.clone()),
        None => self.value(),
      },
      _ => self.value(),
    })
  }
}

/// SECTION: Basic unification
impl Type {
  /// Performs unification between two values, its a
  /// equality relation between two values.
  ///
  /// It does closes some holes.
  ///
  /// # Parameters
  ///
  /// - `self` - The left hand side of the unification
  /// - `rhs`  - The right hand side of the unification
  /// - `ctx`  - The context where the unification is happening
  ///            right now
  ///
  /// # Returns
  ///
  /// It does returns an error if the unification fails. And a
  /// unit if the unification succeeds.
  ///
  /// # Another stuff
  ///
  /// NOTE: I disabled the formatter so I can align values
  /// and it looks cutier.
  #[rustfmt::skip]
  pub fn unify(self, rhs: Type, ctx: &Elab) -> miette::Result<(), UnifyError> {
    /// Imports every stuff so we can't have a lot of
    /// `::` in the code blowing or mind.
    use Value::*;
    use UnifyError::*;

    /// Unifies a spine of applications, it does unifies two list of applications
    /// that are spines.
    /// 
    /// It requires that the spines have the same length, and it does unifies
    /// the spines.
    fn unify_sp(sp_a: Spine, sp_b: Spine, ctx: &Elab) -> miette::Result<(), UnifyError> {
      assert!(sp_a.len() == sp_b.len(), "spines must have the same length");

      for (u_a, u_b) in sp_a.into_iter().zip(sp_b) {
        u_a.unify(u_b, ctx)?;
      }

      Ok(())
    }

    // Forcing here is important because it does removes the holes created
    // by the elaborator, and it does returns a value without holes.
    //
    // It's important to do this because we don't want to unify holes
    // with values, because it will cause a lot of problems, and it will
    // increase the pattern matching complexity.
    match (self.force(), rhs.force()) {
      // Type universe unification is always true, because
      // we don't have universe polymorphism.
      (Type(_, Prim(PrimKind::Int     )), Type(_, Prim(PrimKind::Int)     )) => Ok(()),
      (Type(_, Prim(PrimKind::String  )), Type(_, Prim(PrimKind::String)  )) => Ok(()),
      (Type(_, Prim(PrimKind::Universe)), Type(_, Prim(PrimKind::Universe))) => Ok(()),

      // Unification of literal values, it does checks if the values are equal
      // directly. If they are not, it does returns an error.
      (Type(_, Int(v_a))                , Type(_, Int(v_b))) if v_a == v_b => Ok(()), // 1 = 1, 2 = 2, etc...
      (Type(_, Str(v_a))                , Type(_, Str(v_b))) if v_a == v_b => Ok(()), // "a" = "a", "b" = "b", etc...
      (Type(_, Int(v_a))                , Type(_, Int(v_b)))               => Err(MismatchBetweenInts(v_a, v_b))?,
      (Type(_, Str(v_a))                , Type(_, Str(v_b)))               => Err(MismatchBetweenStrs(v_a, v_b))?,

      // Unification of application spines or meta variables, it does unifies
      // flexibles, rigids and meta variable's spines.
      //
      // It does unifies the spines of the applications.
      (Type(_, Flexible(m_a, sp_a))     , Type(_, Flexible(m_b, sp_b))) if m_a == m_b => unify_sp(sp_a, sp_b, ctx),
      (Type(_,    Rigid(m_a, sp_a))     , Type(_,    Rigid(m_b, sp_b))) if m_a == m_b => unify_sp(sp_a, sp_b, ctx),

      // Lambda unification, that applies closures and pi types
      // using the spine of applications.
      //
      // It does unifies the closures and the pi types.
      (Type(_, Lam(_, v_a))             ,            Type(_, Lam(_, v_b))) => {
        v_a.apply(Type::rigid(ctx.lvl))
           .unify(v_b.apply(Type::rigid(ctx.lvl)), &ctx.lift())
      }
      (Type(_, Lam(_, v_a))             ,                              tt) => {
        v_a.apply(Type::rigid(ctx.lvl))
           .unify(tt.apply(Type::rigid(ctx.lvl)), &ctx.lift())
      }
      (tt                               ,            Type(_, Lam(_, v_b))) => {
        tt.apply(Type::rigid(ctx.lvl))
          .unify(v_b.apply(Type::rigid(ctx.lvl)), &ctx.lift())
      }

      // Unification of meta variables, it does unifies meta variables that
      // are present in the context.
      //
      // It does require a solver function.
      //
      // TODO: Solve
      (Type(_, Flexible(m, sp)), t) | (t, Type(_, Flexible(m, sp))) => t.solve(sp, m, ctx.lvl),

      // Pi type unification, it does unifies the domain and the codomain
      // of the pi types.
      //
      // NOTE: cod stands for codomain, and dom stands for domain.
      (Type(_, Value::Pi(_, i_a, box dom_a, cod_a)) , Type(_, Value::Pi(_, i_b, box dom_b, cod_b))) if i_a == i_b => {
        dom_a.unify(dom_b, ctx)?;
        cod_a.apply(Type::rigid(ctx.lvl))
             .unify(cod_b.apply(Type::rigid(ctx.lvl)), &ctx.lift())
      }

      // Fallback case which will cause an error if we can't unify
      // the values.
      //
      // It's the fallback of the fallbacks cases, the last error message
      // and the least meaningful.
      //
      // Debug locations for types, its useful to display better error messages
      // to the final user of the language.
      //
      // Like if the type is hand-written, it will display the location of the
      // type in the source code.
      (lhs, rhs) => Err(CantUnify(lhs.show(ctx), rhs.show(ctx), lhs.0, rhs.0))?,
    }
  }
}
