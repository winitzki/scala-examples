package example

import cats.{Contravariant, Functor}
import cats.syntax.functor._
import cats.syntax.contravariant._
import CatsMonad.CatsMonadSyntax
import org.scalatest.{FlatSpec, Matchers}

class Chapter11_02_rigidSpec extends FlatSpec with Matchers {

  behavior of "rigid functors"
  
  it should "define flatMap methods for composed rigid monads for Example 13.5.2.2" in {
    def withParams[Z, Q] = {
      type R1[A] = (A => Q) => A

      def map_R1[A, B](r1: R1[A])(f: A => B): R1[B] = { (b2q: B => Q) => f(r1(f andThen b2q)) }

      def flatMap_R1[A, B, M[_] : CatsMonad](r1: R1[M[A]])(f: A => R1[M[B]]): R1[M[B]] = {
        (q: M[B] => Q) => map_R1(r1) { (m: M[A]) => m.flatMap(x => f(x)(q)) }(q)
      }

      type R2[A] = Z => A

      def map_R2[A, B](r2: R2[A])(f: A => B): R2[B] = {
        r2 andThen f
      }

      def flatMap_R2[A, B, M[_] : CatsMonad](r2: R2[M[A]])(f: A => R2[M[B]]): R2[M[B]] = {
        z => map_R2(r2) { (m: M[A]) => m.flatMap(x => f(x)(z)) }(z)
      }

      type T[A] = R1[R2[A]]

      def flatMap_T[A, B, M[_] : CatsMonad](t: T[M[A]])(f: A => T[M[B]]): T[M[B]] = {
        (q: R2[M[B]] => Q) => map_R1(t) { (m: R2[M[A]]) => flatMap_R2(m)(x => f(x)(q)) }(q)
      }
    }
  }
  
  it should "show the rigid functor construction H[A] ⇒ R[A]" in {
    def withParams[H[_] : Contravariant, R[_] : Rigid : Functor] = {

      type Q[A] = H[A] ⇒ R[A]

      implicit val functorQ: Functor[Q] = new Functor[Q] {
        def map[A, B](qa: Q[A])(f: A ⇒ B): Q[B] = hb ⇒ qa(hb.contramap(f)).map(f)
      }

      implicit val rigidQ: Rigid[Q] = new Rigid[Q] {
        def fuseIn[A, B](kleisli: A ⇒ Q[B]): Q[A ⇒ B] = {
          // Given a value `kleisli: A ⇒ H[B] ⇒ R[B]`,
          // need to compute a value of type `H[A ⇒ B] ⇒ R[A ⇒ B]`.
          hab ⇒ // Need to return an R[A ⇒ B] here. 
            // We will first compute a value of type `A ⇒ R[B]` and then use `R.fuseIn` on that.
            val hb: H[B] = hab.contramap(b ⇒ _ ⇒ b) // Convert H[A ⇒ B] to H[B].

            val arb: A ⇒ R[B] = a ⇒ kleisli(a)(hb)
            Rigid[R].fuseIn(arb)
        }
      }

    }
  }

  it should "show the rigid functor construction (A ⇒ P[Q]) ⇒ P[A]" in {
    def withParams[P[_] : Rigid : Functor, Q] = {
      type S[A] = (A ⇒ P[Q]) ⇒ P[A]

      implicit val functorS: Functor[S] = new Functor[S] {
        def map[A, B](sa: S[A])(f: A ⇒ B): S[B] = bpq ⇒ sa(a ⇒ bpq(f(a))).map(f)
      }

      implicit val rigidS: Rigid[S] = new Rigid[S] {
        def fuseIn[A, B](kleisli: A ⇒ S[B]): S[A ⇒ B] = {
          // Given a value `kleisli: A ⇒ (B ⇒ P[Q]) ⇒ P[B]`, 
          // need to compute a value of type `((A ⇒ B) ⇒ P[Q]) ⇒ P[A ⇒ B]`.
          (abpq: (A ⇒ B) ⇒ P[Q]) ⇒ // Need to return a value of type `P[A ⇒ B]` here.
            // We will first compute a value of type `A ⇒ P[B]` and then use `P.fuseIn` on that.
            val bpq: B ⇒ P[Q] = b ⇒ abpq(_ ⇒ b)
            val apb: A ⇒ P[B] = a ⇒ kleisli(a)(bpq)
            Rigid[P].fuseIn(apb)
        }
      }

    }
  }

  it should "show the rigid functor construction F[A ⇒ P[Q]] ⇒ P[A]" in {
    def withParams[P[_] : Rigid : Functor, F[_] : Functor, Q] = {
      type S[A] = F[A ⇒ P[Q]] ⇒ P[A]

      implicit val functorS: Functor[S] = new Functor[S] {
        def map[A, B](sa: S[A])(f: A ⇒ B): S[B] = { fbpq ⇒
          val fapq: F[A ⇒ P[Q]] = fbpq.map(bpq ⇒ a ⇒ bpq(f(a)))
          sa(fapq).map(f)
        }
      }

      implicit val rigidS: Rigid[S] = new Rigid[S] {
        def fuseIn[A, B](kleisli: A ⇒ S[B]): S[A ⇒ B] = {
          // Given a value `kleisli: A ⇒ F[B ⇒ P[Q]] ⇒ P[B]`, 
          // need to compute a value of type `F[(A ⇒ B) ⇒ P[Q]] ⇒ P[A ⇒ B]`.
          (fabpq: F[(A ⇒ B) ⇒ P[Q]]) ⇒ // Need to return a value of type `P[A ⇒ B]` here.
            // We will first compute a value of type `A ⇒ P[B]` and then use `P.fuseIn` on that.
            val fbpq: F[B ⇒ P[Q]] = fabpq.map(abpq ⇒ b ⇒ abpq(_ ⇒ b))
            val apb: A ⇒ P[B] = a ⇒ kleisli(a)(fbpq)
            Rigid[P].fuseIn(apb)
        }
      }

    }
  }

  it should "show that Option[A] is not rigid" in {
    // Define fuseIn for Option[A]. The only possibility is to always return None.

    def fuseIn[A, B](kleisli: A ⇒ Option[B]): Option[A ⇒ B] = {
      // We need to return a value of type `Option[A ⇒ B]`. We can either return `None`, or `Some(f)` for f : A ⇒ B.
      // But we cannot obtain a function A ⇒ B out of A ⇒ Option[B] since we cannot always obtain a B out of Option[B].
      None
    }

    // The non-degeneracy law, fuseOut(fuseIn(x)) == x, fails because
    // `fuseOut(None) = _ ⇒ None`, which cannot be always equal to `x : A ⇒ Option[B]`.

  }

  it should "show that W × A is not rigid" in {
    def withParams[W] = {
      def fuseIn[A, B](kleisli: A ⇒ (W, B)): (W, A ⇒ B) = ??? // Cannot implement since we do not have a value of W.
    }
  }

  it should "show that a rigid monad has a multi-valued flatMap" in {
    def withParams[R[_] : Rigid : Functor : CatsMonad, M[A] : CatsMonad] = {

      import example.CatsMonad.CatsMonadSyntax

      // In addition to flatMap: M[A] ⇒ (A ⇒ M[B]) ⇒ M[B],
      // we also have flatMapR: M[A] ⇒ (A ⇒ R[M[B]]) ⇒ R[M[B]],
      // which returns an R-valued container of M-valued monadic values.
      def flatMapR[A, B](ma: M[A])(f: A ⇒ R[M[B]]): R[M[B]] = {
        val ramb: R[A ⇒ M[B]] = Rigid[R].fuseIn(f)
        val mFlatMap: (A ⇒ M[B]) ⇒ M[B] = amb ⇒ ma.flatMap(amb)
        ramb.map(mFlatMap) // Map R[A ⇒ M[B]] to R[M[B]].
      }
    }
  }

  it should "show that a rigid monad has a refactoring transformation" in {
    def withParams[R[_] : Rigid : Functor] = {

      def refactor[A, B, C](program: (A ⇒ B) ⇒ C): (A ⇒ R[B]) ⇒ R[C] = {
        arb ⇒
          val rab: R[A ⇒ B] = Rigid[R].fuseIn(arb)
          rab.map(program) // Map R[A ⇒ B] to R[C].
      }
    }
  }

  behavior of "rigid functor and monad constructions"
  
  it should "show that the composition of rigid monads is rigid" in {
    
  }

}
