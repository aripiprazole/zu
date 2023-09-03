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
  CantUnify(Nfe, Nfe),
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
  pub fn invert(lvl: Lvl, spine: Spine) -> miette::Result<Self> {
    let _ = lvl;
    let _ = spine;
    todo!()
  }

  /// Perform partial renaming on right-most term while searching the occurrences of
  /// the variable to rename.
  pub fn rename(self, m: MetaVar, value: Value) -> miette::Result<Expr> {
    let _ = m;
    let _ = value;
    todo!()
  }
}

/// SECTION: Pattern unification
impl Value {
  /// solve : (Γ : Cxt) → (spine : Sub Δ Γ) → (m : MetaVar) → (lvl : Lvl) → ()
  pub fn solve(self, spine: Spine, m: MetaVar, lvl: Lvl) -> miette::Result<()> {
    // unsound
    m.update(self);
    let _ = spine;
    let _ = lvl;

    // TODO: Make this thing sound
    Ok(())
  }
}

/// SECTION: Forcing
impl Value {
  /// Forcing is important because it does removes the holes created
  /// by the elaborator.
  ///
  /// It does returns a value without holes.
  pub fn force(self) -> Self {
    match self {
      Value::Flexible(ref m, ref spine) => match m.take() {
        Some(value) => unspine(value, spine.clone()),
        None => self.clone(),
      },
      Value::SrcPos(_, box value) => value,
      _ => self,
    }
  }
}

/// SECTION: Basic unification
impl Value {
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
  pub fn unify(self, rhs: Value, ctx: &Elab) -> miette::Result<()> {
    /// Imports every stuff so we can't have a lot of
    /// `::` in the code blowing or mind.
    use Value::*;
    use UnifyError::*;

    /// Unifies a spine of applications, it does unifies two list of applications
    /// that are spines.
    /// 
    /// It requires that the spines have the same length, and it does unifies
    /// the spines.
    fn unify_sp(sp_a: Spine, sp_b: Spine, ctx: &Elab) -> miette::Result<()> {
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
      (Prim(PrimKind::Int)      ,      Prim(PrimKind::Int)) => Ok(()),
      (Prim(PrimKind::String)   ,   Prim(PrimKind::String)) => Ok(()),
      (Prim(PrimKind::Universe) , Prim(PrimKind::Universe)) => Ok(()),

      // Unification of literal values, it does checks if the values are equal
      // directly. If they are not, it does returns an error.
      (Int(v_a)            , Int(v_b)) if v_a == v_b => Ok(()), // 1 = 1, 2 = 2, etc...
      (Str(v_a)            , Str(v_b)) if v_a == v_b => Ok(()), // "a" = "a", "b" = "b", etc...
      (Int(v_a)            , Int(v_b))               => Err(MismatchBetweenInts(v_a, v_b))?,
      (Str(v_a)            , Str(v_b))               => Err(MismatchBetweenStrs(v_a, v_b))?,

      // Lambda unification, that applies closures and pi types
      // using the spine of applications.
      //
      // It does unifies the closures and the pi types.
      (Lam(_, v_a)         , Lam(_, v_b)) => {
        v_a.apply(Value::rigid(ctx.lvl))
           .unify(v_b.apply(Value::rigid(ctx.lvl)), &ctx.lift())
      }
      (t                   ,   Lam(_, v)) => {
        t.apply(Value::rigid(ctx.lvl))
         .unify(v.apply(Value::rigid(ctx.lvl)), &ctx.lift())
      }
      (Lam(_, v)           ,           t) => {
        v.apply(Value::rigid(ctx.lvl))
         .unify(t.apply(Value::rigid(ctx.lvl)), &ctx.lift())
      }

      // Pi type unification, it does unifies the domain and the codomain
      // of the pi types.
      //
      // NOTE: cod stands for codomain, and dom stands for domain.
      (Value::Pi(_, i_a, box dom_a, cod_a) , Value::Pi(_, i_b, box dom_b, cod_b)) if i_a == i_b => {
        dom_a.unify(dom_b, ctx)?;
        cod_a.apply(Value::rigid(ctx.lvl))
             .unify(cod_b.apply(Value::rigid(ctx.lvl)), &ctx.lift())
      }

      // Unification of application spines or meta variables, it does unifies
      // flexibles, rigids and meta variable's spines.
      //
      // It does unifies the spines of the applications.
      (Flexible(m_a, sp_a) , Flexible(m_b, sp_b)) if m_a == m_b => unify_sp(sp_a, sp_b, ctx),
      (Rigid(m_a, sp_a)    ,    Rigid(m_b, sp_b)) if m_a == m_b => unify_sp(sp_a, sp_b, ctx),

      // Unification of meta variables, it does unifies meta variables that
      // are present in the context.
      //
      // It does require a solver function.
      //
      // TODO: Solve
      (Flexible(m, sp) , t) | (t , Flexible(m, sp)) => t.solve(sp, m, ctx.lvl),

      // Fallback case which will cause an error if we can't unify
      // the values.
      //
      // It's the fallback of the fallbacks cases, the last error message
      // and the least meaningful.
      (a   ,   b) if a == b => Ok(()),
      (lhs , rhs)           => {
        Err(CantUnify(lhs.show(ctx), rhs.show(ctx)))?
      },
    }
  }
}
