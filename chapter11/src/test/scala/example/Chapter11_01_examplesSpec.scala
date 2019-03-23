package example

import cats.syntax.functor._
import cats.{Functor, Id, Monoid, ~>}
import example.CatsMonad.CatsMonadSyntax
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

class Chapter11_01_examplesSpec extends FlatSpec with Matchers {

  behavior of "composition of Future and Option"

  it should "compute with Future[Option[A]] without monad transformers" in {

    import scala.concurrent.ExecutionContext.Implicits.global
    val r = for {
      xOpt ← Future(Option(1))
      yOpt ← xOpt match {
        case Some(x) ⇒ for {
          zOpt ← Future(Option(x + 1))
          t ← zOpt match {
            case Some(z) ⇒ Future(Option(z + 3))
            case None ⇒ Future.successful(None)
          }
        } yield t
        case None ⇒ Future.successful(None)
      }
      z ← yOpt match {
        case Some(y) ⇒ Future(Option(y * 2).orElse(Some(-1)))
        case None ⇒ Future.successful(None)
      }
    } yield z

    Await.result(r, Duration.Inf).get shouldEqual 10
  }

  it should "compute with Future[Option[A]] with a custom monad transformer" in {
    /** Wrapper monad class for `Future[Option[A]]` that supports `for/yield` syntax more easily,
      * without nested type constructors.
      *
      * @tparam A Type of a computed value inside the monad.
      */
    case class FutureWithOption[A](nested: Future[Option[A]]) {
      def map[B](f: A ⇒ B)(implicit ec: ExecutionContext): FutureWithOption[B] = nested.map(_.map(f))

      def flatMap[B](f: A ⇒ FutureWithOption[B])(implicit ec: ExecutionContext): FutureWithOption[B] = {
        nested.flatMap {
          case None ⇒ Future.successful(None)
          case Some(x) ⇒ f(x).nested
        }
      }

      // This is necessary to support pattern-matching, e.g. `(x, y) ← futureWithOption(...)`.
      def withFilter(p: A ⇒ Boolean)(implicit ec: ExecutionContext): FutureWithOption[A] = nested

      // Convenience method to lift Option's operations into the wrapper.
      def liftFunction[B](f: Option[A] ⇒ Option[B])(implicit ec: ExecutionContext): FutureWithOption[B] = nested.map(f)

      // Convenience method.
      def orElse[B](y: ⇒ Option[B])(implicit ec: ExecutionContext): FutureWithOption[B] = liftFunction(x ⇒ y)
    }

    // Lower-precedence implicit conversion.
    trait FutureWithOptionConversion {
      implicit def toFutureWithErrorPlain[E, A](x: Future[A])(implicit ec: ExecutionContext): FutureWithOption[A] =
        FutureWithOption(x.map(Some.apply))
    }

    object FutureWithOption extends FutureWithOptionConversion {
      implicit def toFutureWithError[A](x: Future[Option[A]]): FutureWithOption[A] = FutureWithOption(x)

      def pure[A](x: A): FutureWithOption[A] = FutureWithOption(Future.successful(Some(x)))
    }

    import scala.concurrent.ExecutionContext.Implicits.global
    val r = for {
      x ← FutureWithOption.pure(1)
      y ← FutureWithOption.pure(x + 1)
      t ← FutureWithOption.pure(y + 3)
      z ← FutureWithOption.pure(t * 2).orElse(Some(-1))
    } yield z

    Await.result(r.nested, Duration.Inf).get shouldEqual 10
  }

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

  behavior of "monad transformer for linear-value monads"

  it should "implement monad transformer for EW monad" in {
    def withParams[E, W: Monoid] = {

      import cats.syntax.monoid._

      type EW[A] = Either[E, (W, A)] // It could also be (W, Either[E, A]). Both are lawful monads.
      // W × (E + A) = W × E + W × A can be rewritten as E + W × A if we replace `E × W` by a new `E`.
      // The Option, Writer, and Either monads are special cases of `EW`.

      // Transformer: composed-inside.
      type EWT[M[_], A] = M[EW[A]]

      // Functor instance for EW.
      implicit val functorEW: Functor[EW] = cats.derive.functor[EW]

      // Implement `flatten` for `EW`.
      def flattenEW[A]: EW[EW[A]] ⇒ EW[A] = {
        case Left(e) ⇒ Left(e)
        case Right((w, Left(e))) ⇒ Left(e)
        case Right((w1, Right((w2, x)))) ⇒ Right((w1 |+| w2, x))
      }

      // Monad instance for EW.
      implicit val monadEW: CatsMonad[EW] = new CatsMonad[EW] {
        def flatMap[A, B](fa: EW[A])(f: A ⇒ EW[B]): EW[B] = flattenEW(functorEW.map(fa)(f))

        def pure[A](x: A): EW[A] = Right((Monoid[W].empty, x))
      }

      // Functor instance for EWT.
      implicit def functorEWT[M[_] : Functor]: Functor[EWT[M, ?]] = new Functor[EWT[M, ?]] {
        def map[A, B](fa: M[EW[A]])(f: A ⇒ B): M[EW[B]] = fa.map(functorEW.map(_)(f))
      }

      // `sequence` method for EW; this is needed to define the transformer monad.
      def seq[M[_] : CatsMonad : Functor, A](ewma: EW[M[A]]): M[EW[A]] = ewma match {
        case Left(e) ⇒ CatsMonad[M].pure(Left(e))
        case Right((w, fa)) ⇒ fa.map(x ⇒ Right((w, x)))
      }

      // Implement flatten for EWT.
      def flatten[M[_] : CatsMonad : Functor, A](mewmewa: M[EW[M[EW[A]]]]): EWT[M, A] = {
        // The plan is first to transform M[EW[M[EW[A]]]] into M[M[EW[EW[A]]]], then flatten M and EW.
        mewmewa.flatMap { ewmewa: EW[M[EW[A]]] ⇒ // Will return M[EW[A]] here.
          val mewewa: M[EW[EW[A]]] = seq[M, EW[A]](ewmewa) // Using `seq` defined above for `EW` and `M`.
        val mewa: M[EW[A]] = mewewa.map(flattenEW) // Using `flatten` defined above for `EW`.
          mewa
        }
      }

      // Shorter code: mewmewa.flatten = mewmewa.flatMap(ewmewa ⇒ seq(ewmewa).map(EW.flatten))
      //                  = mewmewa.flatMap(seq andThen _.map(EW.flatten))

      // Monad instance for EWT[M, ?].
      implicit val mtransdefEWT: MTransDef[EWT] = new MTransDef[EWT] {
        def transformed[M[_] : CatsMonad : Functor]: CatsMonad[EWT[M, ?]] = new CatsMonad[EWT[M, ?]] {
          def flatMap[A, B](fa: M[EW[A]])(f: A ⇒ EWT[M, B]): EWT[M, B] = flatten(fa.map(functorEW.map(_)(f)))

          def pure[A](x: A): M[EW[A]] = CatsMonad[M].pure(CatsMonad[EW].pure(x))
        }
      }

      // The monad laws for the monad EWT[M, ?] were already verified in Chapter 7 (monad construction 6).

      // For a full monad transformer, we still need to define `lift`, `blift`, `mrun`, and `brun`.
      // These definitions are straightforward since the monad transformer is defined via functor composition.
      implicit val mtransEWT: MTrans[EWT, EW] = new MTrans[EWT, EW] {
        def lift[M[_] : CatsMonad : Functor, A](ma: M[A]): M[EW[A]] = ma.map(CatsMonad[EW].pure)

        def blift[M[_] : CatsMonad : Functor, A](la: EW[A]): M[EW[A]] = CatsMonad[M].pure(la)

        def mrun[M[_] : CatsMonad : Functor, N[_] : CatsMonad](mn: M ~> N): EWT[M, ?] ~> EWT[N, ?] = new (EWT[M, ?] ~> EWT[N, ?]) {
          def apply[A](fa: M[EW[A]]): N[EW[A]] = mn[EW[A]](fa)
        }

        def brun[M[_] : CatsMonad : Functor](lrun: EW ~> Id): EWT[M, ?] ~> M = new (EWT[M, ?] ~> M) {
          def apply[A](fa: M[EW[A]]): M[A] = fa.map(ewa ⇒ lrun(ewa))
        }
      }

      /* Monad transformer laws:
      
      `lift` is a monadic morphism.
      
      Identity law: lift(M.pure(x)) = M.pure(x).map(EW.pure) = < use naturality of M.pure >
        = M.pure(EW.pure(x)) = EWT.pure(x)
      
      Composition law (for flatten):
        lift andThen EWT.fmap(lift) andThen EWT.flatten = M.flatten andThen lift
      Both sides are functions M[M[A]] ⇒ EWT[A]. Apply both sides to an arbitrary value mma: M[M[A]].
      
      mma.map(EW.pure)
        .map(_.map(_.map(EW.pure))
        .flatMap(seq andThen _.map(EW.flatten)) =
      M.flatten(mma)
        .map(_.map(EW.pure))
      
      To perform such calculations, it is quicker to use the short code notation.
      
      Here we will just establish two properties of `seq` as implemented above,
      showing the relationship of `seq` with `EW.pure` and `M.pure`:
      
      1. seq(EW.pure(ma)) = ma.map(EW.pure)   or     EW.pure andThen seq = _.map(EW.pure)
      
      seq(EW.pure(ma)) = seq(Right((W.empty, ma))) = ma.map(x ⇒ Right((W.empty, x))) = ma.map(EW.pure)
      
      2. seq(ewa.map(M.pure)) = M.pure(ewa)    or    _.map(M.pure) andThen seq = M.pure
      
      If ewa = Left(e)  then seq(Left(e).map(M.pure)) = seq(Left(e)) = M.pure(Left(e))
      If ewa = Right((w, x)) then ewa.map(M.pure)) = Right((w, M.pure(x))) and so
        seq(ewa.map(M.pure)) = M.pure(x).map(x ⇒ Right((w, x))) = M.pure(Right((w, x))) = M.pure(ewa)
      
      */

    }
  }
}
