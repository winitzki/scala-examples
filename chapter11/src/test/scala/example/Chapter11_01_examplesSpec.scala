package example

import cats.{Functor, Monad, Monoid}
import cats.syntax.functor._
import org.scalatest.{FlatSpec, Matchers}

class Chapter11_01_examplesSpec extends FlatSpec with Matchers {

  behavior of "monad transformers"

  it should "fail to compose Option with Reader" in {
    def withParams[R] = {
      type OR[A] = Option[R ⇒ A]

      var orMonad: CatsMonad[OR] = new CatsMonad[OR] {

        def flatMap[A, B](fa: OR[A])(f: A ⇒ OR[B]): OR[B] = fa match {
          // Need to return Option[R ⇒ B]. We should avoid always returning `None`,
          // because that would violate the identity law `fa.flatMap(pure) = fa`.
          case None ⇒ None // This is the only possibility here.
          case Some(rToA) ⇒ // Here, we need to return `None` or `Some[R ⇒ B]`.
            // However, we don't have any values of types A or R here, 
            // so we can't apply `f` to anything at this point.
            // Therefore, we have to decide in advance whether to return `None` or `Some[R ⇒ B]`.
            // We would like to try returning a `Some[R ⇒ B]` so that we could satisfy the identity law.
            Some { r ⇒
              // We need to get a `B` here. We can apply `f` to `rToA(r) : A`.
              val orb: Option[R ⇒ B] = f(rToA(r))
              // Now, `orb` is either `None` or `Some[R ⇒ B]`.
              orb match {
                case Some(rToB) ⇒ rToB(r)
                case None ⇒ ??? // Here we get stuck: there is no way to compute a `B`.
              }
            }
        }

        def pure[A](x: A): OR[A] = Some { _ ⇒ x }
      }
    }

  }

  behavior of "State monad does not compose with other monads"

  it should "fail to implement composed-outside transformer for State monad" in {
    def withParams[R, S] = {
      type St[A] = S ⇒ (S, A) // State monad with state type S.
      type SR[A] = St[R ⇒ A] // Composition of State and Reader.

      // Implement the type signature of Monad for SR[?].

      val srMonad: CatsMonad[SR] = new CatsMonad[SR] {
        def flatMap[A, B](fa: SR[A])(f: A ⇒ SR[B]): SR[B] = { s ⇒
          val (s1, ra): (S, R ⇒ A) = fa(s)
          (s1, { r ⇒ // Have to use s or s1 here, since `r` is only available inside the closure.
            val a: A = ra(r)
            val (s2, rb): (S, R ⇒ B) = f(a)(s1)
            rb(r) // Have to discard s2.
          })
        }

        def pure[A](x: A): SR[A] = { s ⇒ (s, _ ⇒ x) }
      }

      // We have not used `f` in computing the new `s` after flatMap(f).
      // This means we are discarding the effect of the second monad.
      // This will violate the identity law pure(x).flatMap(f) = f(x):
      /*
      For any f : A ⇒ St[B] and for any x : A we should have f(x) = pure(x).flatMap(f)
      
      pure(x).flatMap(f) = flatMap { s ⇒ (s, _ ⇒ x) } (f) = { s ⇒
        val (s1, ra) = (s, _ ⇒ x)
        (s, ...)
      }
      
      The result always has the initial unmodified `s` as the first element of the pair.
      However, f(x) : St[B] could have the form { s ⇒ (s1, ...) } where s1 != s, depending on x.
      Therefore, pure(x).flatMap(f) is not equal to f(x) for arbitrary functions f.
       */
    }
  }

  it should "fail to implement composed-inside transformer for State monad" in {
    def withParams[R, S] = {
      type St[A] = S ⇒ (S, A) // State monad with state type S.
      type OS[A] = Option[St[A]] // Composition of Option and State.

      // Implement the type signature of Monad for OS[?].

      val osMonad: CatsMonad[OS] = new CatsMonad[OS] {
        def flatMap[A, B](fa: OS[A])(f: A ⇒ OS[B]): OS[B] = fa match {
          // Need to return Option[St[B]]. We should avoid always returning `None`,
          // because that would violate the identity law `fa.flatMap(pure) = fa`.
          case None ⇒ None // This is the only possibility here.
          case Some(sta) ⇒
            // If we return None here, we will violate the identity law.
            Some { s ⇒ // We are trying to return a Some(St[B]) here.
              // We can now apply `sta` to `s` and get some more values.
              val (s1, a) = sta(s)
              // Can we get a value sb : (S, B)?
              val s2: S = s // We could return various values of type s.
            val b: B = {
              // The only way of possibly getting `b: B` is to apply `f` to an `A`.
              val osb: Option[St[B]] = f(a)
              // Now, `osb` could be either `None` or `Some(...)`.
              osb match {
                case Some(stb) ⇒ stb(s2)._2
                case None ⇒ ??? // Here we get stuck: there is no way of computing a `b : B`.
              }
            }
              (s2, b)
            }
        }

        def pure[A](x: A): OS[A] = Some { s ⇒ (s, x) }
      }
    }
  }

  behavior of "monad transformers for single-value monads"

  it should "implement monad transformer for EW monad" in {
    def withParams[E, W: Monoid] = {

      import cats.syntax.monoid._

      type EW[A] = Either[E, (W, A)] // It could also be (W, Either[E, A]). Both are monads.
      // W × (E + A) = W × E + W × A can be rewritten as E + W × A if we replace `E × W` by a new `E`.
      // The Option, Writer, and Either monads are special cases of `EW`.

      // Transformer: composed-inside.
      type EWT[M[_], A] = M[EW[A]]

      // Functor instance for EW.
      implicit val functorEW: Functor[EW] = cats.derive.functor[EW]

      // Implement `flatten` for `EW`.
      def flatten[A]: EW[EW[A]] ⇒ EW[A] = {
        case Left(e) ⇒ Left(e)
        case Right((w, Left(e))) ⇒ Left(e)
        case Right((w1, Right((w2, x)))) ⇒ Right((w1 |+| w2, x))
      }

      // Monad instance for EW.
      implicit val monadEW: CatsMonad[EW] = new CatsMonad[EW] {
        def flatMap[A, B](fa: EW[A])(f: A ⇒ EW[B]): EW[B] = flatten(fa.map(f))

        def pure[A](x: A): EW[A] = Right((Monoid[W].empty, x))
      }

      // Functor instance for EWT.
      //      implicit def functorEWT[M[_] : Functor]: Functor[EWT[M, ?]] = new Functor[EWT[M, ?]] {
      //        def map[A, B](fa: EWT[M, A])(f: A ⇒ B): EWT[M, B] = ???
      //      }

      // Traverse instance for EW; this is needed to define the transformer monad.
      implicit val travEW: Trav[EW] = new Trav[EW] {
        def seq[F[_] : WuZip : Functor, A](lfa: EW[F[A]]): F[EW[A]] = lfa match {
          case Left(e) ⇒ WuZip[F].pure(Left(e))
          case Right((w, fa)) ⇒ fa.map(x ⇒ Right((w, x)))
        }
      }

      import Trav.TravSyntax
      import CatsMonad.CatsMonadSyntax

      // Implement flatten for EWT.
      def flatten[M[_] : CatsMonad : WuZip : Functor, A](mewmewa: M[EW[M[EW[A]]]]): EWT[M, A] = {
        // The plan is to transform M[EW[M[EW[A]]]] into M[M[EW[EW[A]]]], then flatten M and EW.
        mewmewa.flatMap { ewmewa: EW[M[EW[A]]] ⇒ // Will return M[EW[A]] here.
          val mewewa: M[EW[EW[A]]] = ewmewa.seq // Using `Trav` instance.
          val mewa: M[EW[A]] = mewewa.map(flatten) // Using `flatten` defined above for `EW`.
          mewa
        }
      }
      
      // The monad laws for the monad EWT were already verified in Chapter 7.
    }
  }
}
