use super::*;
use crate::passes::elab::Type;

impl Elab {
  /// Creates a new type elaborating it into a new
  /// value.
  pub fn infer(&self, term: &Tm) -> Type {
    /// Infers the type of a term in the context of the environment
    ///
    /// It does returns the type of the term.
    #[inline(always)]
    fn imp(ctx: &Elab, term: &Tm) -> Type {
      Type::synthesized(match term {
        // Removed
        Term::Group(_) => unreachable!(),

        // Values
        Term::Int(_) => Value::Prim(PrimKind::Int),
        Term::Str(_) => Value::Prim(PrimKind::String),
        Term::Prim(_) => Value::Prim(PrimKind::Universe), // Type of type
        Term::Hole(_) | Term::Error(_) => return ctx.fresh_meta().eval(&ctx.env),
        Term::Fun(e) => {
          let name = e.arguments.as_shift();
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

          let (domain, codomain) = match callee.clone().force() {
            Type(_, Value::Pi(_, _, box tt, closure)) => (tt, closure),
            value => {
              let tt = ctx.fresh_meta().eval(&ctx.env);
              let closure = Closure {
                env: ctx.env.clone(),
                term: ctx.create_new_value("x", tt.clone()).fresh_meta(),
              };

              ctx.unify(Type::pi("x", tt.clone(), closure.clone()), value.clone());

              (tt, closure)
            }
          };

          return codomain.apply(ctx.check(&apply.arguments, domain).eval(&ctx.env));
        }

        // Resolves global references, and returns the type of the
        // global.
        Term::Reference(name) if name.definition.is_global => {
          let declaration = ctx.env.globals.lookup(&name.definition.text);

          // Return the type of the declaration
          return declaration.type_repr.clone();
        }

        // Resolves and infers the type of a reference to a variable
        // in the environment. It does uses debruijin, and `erase` function
        // works very well with it.
        Term::Reference(n) => {
          // Get the types from the context, and returns the first one.
          let mut types = ctx.types.clone();
          while let Some((c, t)) = types.pop_front() {
            if n.definition.text == c {
              return t;
            }
          }

          let Term::Reference(Reference::Var(Ix(ix))) = term.clone().erase(ctx) else {
            // We already check at the beginning of the function with the
            // pattern matching that the term is a reference.
            //
            // So this is unreachable.
            unreachable!("term is not a reference: {:?}", term)
          };

          // TODO: Report error
          // Gets the value from the environment and clones it to avoid
          // borrowing the environment.
          return ctx.env.data[ix].clone();
        }

        // Type check annotation, it does only checks the type of the
        // annotation, and returns the value of the annotation.
        //
        // It changes the mode of type checking, from inferring to checking.
        Term::Anno(anno) => {
          return ctx
            .check(&anno.value, anno.type_repr.clone().erase(ctx).eval(&ctx.env))
            .eval(&ctx.env)
        }

        // Infers the type of a lambda, it does creates a new closure
        // and returns the type of the closure.
        Term::Pi(pi) => {
          // Checks that the type of the domain is a type
          let domain = ctx.check(&pi.domain.type_repr, Type::universe()).eval(&ctx.env);

          ctx
            .create_new_value(&pi.domain.name.text, domain)
            .check(&pi.codomain, Type::universe());

          Value::Prim(PrimKind::Universe)
        }
      })
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
