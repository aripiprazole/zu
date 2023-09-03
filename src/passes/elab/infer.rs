use super::*;

impl Elab {
  /// Creates a new type elaborating it into a new
  /// value.
  pub fn infer(&self, term: &Tm) -> Value {
    /// Infers the type of a term in the context of the environment
    ///
    /// It does returns the type of the term.
    #[inline(always)]
    fn imp(ctx: &Elab, term: &Tm) -> Value {
      match term {
        // Removed
        Term::Group(_) => unreachable!(),

        // Values
        Term::Int(_) => Value::Prim(PrimKind::Int),
        Term::Str(_) => Value::Prim(PrimKind::String),
        Term::Prim(_) => Value::Prim(PrimKind::Universe), // Type of type
        Term::Hole(_) | Term::Error(_) => ctx.fresh_meta().eval(&ctx.env),
        Term::Fun(e) => {
          let name = Definition::new(e.arguments.text.clone());
          let domain = ctx.fresh_meta().eval(&ctx.env);
          let codomain = ctx.create_new_value(&name.text, domain.clone()).infer(&e.value);

          Value::Pi(name, Icit::Expl, domain.clone().into(), Closure {
            env: ctx.env.clone(),

            // Here we need to increase the level, because we are binding
            // in a new environment.
            term: codomain.quote(ctx.lvl + 1),
          })
        }
        Term::Elim(_) => todo!(),

        // Infers the type of a function application, it can apply
        // either a pi type, or a closure that is a lambda.
        Term::Apply(apply) => {
          let callee = ctx.infer(&apply.callee);

          apply
            .arguments
            .iter()
            .cloned()
            // Creates a spine of applications to the callee
            // and then applies the spine to the callee.
            .fold(callee, |callee, argument| {
              let (domain, codomain) = match callee.force() {
                Value::Pi(_, _, box tt, closure) => (tt, closure),
                value => {
                  let tt = ctx.fresh_meta().eval(&ctx.env);
                  let closure = Closure {
                    env: ctx.env.clone(),
                    term: ctx.create_new_value("x", tt.clone()).fresh_meta(),
                  };

                  ctx.unify(Value::pi("x", tt.clone(), closure.clone()), value);

                  (tt, closure)
                }
              };

              codomain.apply(ctx.check(&argument, domain).eval(&ctx.env))
            })
        }

        // Resolves and infers the type of a reference to a variable
        // in the environment. It does uses debruijin, and `erase` function
        // works very well with it.
        Term::Reference(n) => {
          let Term::Reference(Reference::Var(Ix(ix))) = term.clone().erase(ctx) else {
            // We already check at the beginning of the function with the
            // pattern matching that the term is a reference.
            //
            // So this is unreachable.
            unreachable!()
          };

          // Get the types from the context, and returns the first one.
          let mut types = ctx.types.clone();
          while let Some((c, t)) = types.pop_front() {
            if n.definition.text == c {
              return t;
            }
          }

          // TODO: Report error
          // Gets the value from the environment and clones it to avoid
          // borrowing the environment.
          ctx.env.data[ix].clone()
        }

        // Type check annotation, it does only checks the type of the
        // annotation, and returns the value of the annotation.
        //
        // It changes the mode of type checking, from inferring to checking.
        Term::Anno(anno) => ctx
          .check(&anno.value, anno.type_repr.clone().erase(ctx).eval(&ctx.env))
          .eval(&ctx.env),

        // Infers the type of a lambda, it does creates a new closure
        // and returns the type of the closure.
        Term::Pi(pi) => {
          let name = Definition::new(pi.domain.name.text.clone());
          let domain = ctx.infer(&pi.domain.type_repr);
          let codomain = Closure {
            env: ctx.env.create_new_value(domain.clone()),
            term: pi.codomain.clone().erase(ctx),
          };

          Value::Pi(name, pi.domain.icit, domain.into(), codomain)
        }
      }
    }

    let meta = term.meta();

    // Sets the position of the elaborator to the error diagnostics
    // goes to the right place.
    self.position.replace(meta.clone());

    // Infers the type of the term
    let value = imp(self, term);

    // Insert the type associating it with it's id,
    // so we can use it later.
    self.tt.borrow_mut().insert(meta.id, value.clone());

    value
  }
}
