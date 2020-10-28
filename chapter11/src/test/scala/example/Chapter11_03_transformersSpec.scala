package example

import example.TypeClassDefinitions.{Id, Monad, pureAsMonadMorphism}
import io.chymyst.ch.implement
import org.scalatest.{FlatSpec, Matchers}

import scala.language.higherKinds
import scala.util.Try

// Two monads are in a lift relation if there exists a monad morphism between those monads.
trait MM[P[_], Q[_]] {
  def apply[A]: P[A] => Q[A]
} // The function `up` must be a monad morphism, not just a natural transformation.


final case class BaseMonadOf[L[_], KT[_[_], _]](iso: MM[L, KT[Id, *]])

// Monad transformer typeclass for this example.
trait MTr[KT[_[_], _]] { // The base monad is KT[Id, A].
  def monadT[M[_] : Monad]: Monad[KT[M, *]]

  def flift[M[_] : Monad, A]: M[A] => KT[M, A]

  def blift[M[_] : Monad, A]: KT[Id, A] => KT[M, A] = frun[Id, M, A](pureAsMonadMorphism[M]) // Default implementation.

  def frun[M[_] : Monad, N[_] : Monad, A](phi: MM[M, N]): KT[M, A] => KT[N, A]
}

trait TypeClassDefinitionsLowerPriorityImplicits {

  type Id[A] = A

  implicit class PipeOps[A](a: A) {
    def |>[B](f: A ⇒ B): B = f(a)
  }

  def pureAsMonadMorphism[M[_] : Monad]: MM[Id, M] = new MM[Id, M] {
    override def apply[A]: Id[A] ⇒ M[A] = Monad[M].pure
  }

  def identityAsMonadMorphism[M[_]]: MM[M, M] = new MM[M, M] {
    override def apply[A]: M[A] ⇒ M[A] = identity
  }

  // Any monad P is in a lift relation with itself.
  implicit def liftableId[P[_]]: MM[P, P] = identityAsMonadMorphism[P]

  // Lift a monad to the more nested transformed monad. This assumes L not equal to M.
  implicit def monadToTransformer[KT[_[_], _], L[_] : Monad, M[_] : Monad]
  (implicit kt: MTr[KT], lm: MM[L, M]): MM[L, KT[M, *]] = new MM[L, KT[M, *]] {
    override def apply[A]: L[A] ⇒ KT[M, A] = lm.apply andThen kt.flift
  }

}

object TypeClassDefinitions extends TypeClassDefinitionsLowerPriorityImplicits {

  // Quick and self-contained definition of the Monad typeclass.
  trait Monad[M[_]] {
    def pure[A]: A ⇒ M[A]

    def fmap[A, B](f: A ⇒ B): M[A] ⇒ M[B]

    def flm[A, B](f: A ⇒ M[B]): M[A] ⇒ M[B]
  }

  object Monad {
    def apply[M[_]](implicit mm: Monad[M]): Monad[M] = mm
  }

  implicit class FunctorBlockSyntax[M[_], A](m: M[A])(implicit monadM: Monad[M]) {
    def map[B](f: A ⇒ B): M[B] = monadM.fmap(f)(m)

    def flatMap[B](f: A ⇒ M[B]): M[B] = monadM.flm(f)(m)

    def withFilter(p: A ⇒ Boolean): M[A] = m
  }

  implicit val monadId: Monad[Id] = new Monad[Id] {
    override def pure[A]: A ⇒ Id[A] = implement

    override def fmap[A, B](f: A ⇒ B): Id[A] ⇒ Id[B] = implement

    override def flm[A, B](f: A ⇒ Id[B]): Id[A] ⇒ Id[B] = implement
  }

  // Produce a monad instance automatically for any transformed monads.
  implicit def monadTransformed[M[_] : Monad, KT[_[_], _] : MTr]: Monad[KT[M, *]] = implicitly[MTr[KT]].monadT

  implicit class LiftableSyntax[P[_], A](p: P[A]) {
    def up[Q[_]](implicit mm: MM[P, Q]): Q[A] = mm.apply(p)
  }

  // Lift base monad to the transformed monad.
  implicit def baseMonadToTransformer[KT[_[_], _], M[_] : Monad, L[_] : Monad]
  (implicit kt: MTr[KT], isBase: BaseMonadOf[L, KT]): MM[L, KT[M, *]] = new MM[L, KT[M, *]] {
    override def apply[A]: L[A] ⇒ KT[M, A] = isBase.iso.apply andThen kt.blift[M, A]
  }

  // Lift foreign monad to the transformed monad.
  implicit def foreignLiftable[KT[_[_], _], M[_] : Monad](implicit kt: MTr[KT]): MM[M, KT[M, *]] = new MM[M, KT[M, *]] {
    override def apply[A]: M[A] ⇒ KT[M, A] = kt.flift[M, A]
  }

  // Does not work:
  //  def getBaseMonad[KT[_[_], _]](implicit kt: MTr[KT]): BaseMonadOf[kt.Base, KT] =
  //    BaseMonadOf[kt.Base, KT](kt.baseId)

}

class Chapter11_03_transformersSpec extends FlatSpec with Matchers {

  import TypeClassDefinitions._

  behavior of "monad transformer typeclass"

  it should "implement transformer instances for ReaderT and EitherT" in {
    // Declare types and some values for brevity.
    def withParams[E, R](r0: R, transform1: (R, Int) ⇒ Option[Int], check1: (R, Int) ⇒ Either[E, Int]) = {

      // The test will create a monadic program that uses a read-only value R and will work with optional values.
      // The given initial optional value is first transformed using transform1.
      // If the resulting optional value is empty or is invalid (which is determined using the read-only value via check1),
      // the program will return an error value of type E.

      // Helper functions that implement this business logic are transform1 and check1.
      // We would like to write code like this:
      /*
      val program = for {
        x <- input // : Option[Int]
        y <- Reader(r => transform1(r, x))
        z <- Reader(r => check1(r, y)) // : Reader[R, Either[E, Int]]
      } yield z

      program.run(...) // Need to use some runner here.
       */

      final case class Reader[A](run: R ⇒ A)

      // We remove some boilerplate by using the helper function `ask`:
      val ask: Reader[R] = Reader(identity)

      // Then the program should eventually look like this:
      /*
      val program = for {
        x <- input.up // : Option[Int] lifted to the monad stack
        r <- ask.up // : Reader[R] lifted to the monad stack
        y <- transform1(r, x).up // Option[Int] lifted to the monad stack
        z <- check1(r, y) // : Either[E, Int] lifted to the monad stack
      } yield z

      program.run(...) // Will use some runner here.
       */

      // A type alias does not work with implicit derivation of instances later, so we need a case class.
      final case class ReaderT[M[_], A](run: Reader[M[A]]) // The fixed type R must be already defined.

      implicit val monadReader: Monad[Reader] = new Monad[Reader] {
        def pure[A]: A ⇒ Reader[A] = implement

        def fmap[A, B](f: A ⇒ B): Reader[A] ⇒ Reader[B] = implement

        def flm[A, B](f: A ⇒ Reader[B]): Reader[A] ⇒ Reader[B] = implement
      }

      implicit val MTrReaderT: MTr[ReaderT] = new MTr[ReaderT] {

        override def monadT[M[_] : Monad]: Monad[ReaderT[M, *]] = new Monad[ReaderT[M, *]] {
          override def pure[A]: A ⇒ ReaderT[M, A] = a ⇒ ReaderT(Reader(_ ⇒ Monad[M].pure(a)))

          override def fmap[A, B](f: A ⇒ B): ReaderT[M, A] ⇒ ReaderT[M, B] =
            rma ⇒ ReaderT(Reader(r ⇒ rma.run.run(r).map(f)))

          override def flm[A, B](f: A ⇒ ReaderT[M, B]): ReaderT[M, A] ⇒ ReaderT[M, B] =
            rma ⇒ ReaderT(Reader(r ⇒ rma.run.run(r).flatMap(a ⇒ f(a).run.run(r))))
        }

        override def flift[M[_] : Monad, A]: M[A] => ReaderT[M, A] = { ma => ReaderT(Reader(_ => ma)) }

        override def frun[M[_] : Monad, N[_] : Monad, A](phi: MM[M, N]): ReaderT[M, A] => ReaderT[N, A] =
          t => ReaderT(Reader(r => phi.apply(t.run.run(r))))

      }

      implicit val readerBase: BaseMonadOf[Reader, ReaderT] = BaseMonadOf(new MM[Reader, ReaderT[Id, *]] {
        def apply[A]: Reader[A] ⇒ ReaderT[Id, A] = ReaderT[Id, A](_)
      })

      implicit val monadEither: Monad[Either[E, *]] = new Monad[Either[E, *]] {
        override def pure[A]: A ⇒ Either[E, A] = implement

        override def fmap[A, B](f: A ⇒ B): Either[E, A] ⇒ Either[E, B] = implement

        override def flm[A, B](f: A ⇒ Either[E, B]): Either[E, A] ⇒ Either[E, B] = implement
      }

      // A type alias does not work here, need a case class.
      final case class EitherT[M[_], A](run: M[Either[E, A]]) // The fixed type E must be already defined.

      implicit val MTrEitherT: MTr[EitherT] = new MTr[EitherT] {
        override def monadT[M[_] : Monad]: Monad[EitherT[M, *]] = new Monad[EitherT[M, *]] {
          override def pure[A]: A ⇒ EitherT[M, A] = a ⇒ EitherT(Monad[M].pure(Right(a): Either[E, A]))

          override def fmap[A, B](f: A ⇒ B): EitherT[M, A] ⇒ EitherT[M, B] = { m: EitherT[M, A] ⇒ EitherT(m.run.map(_.map(f))) }

          override def flm[A, B](f: A ⇒ EitherT[M, B]): EitherT[M, A] ⇒ EitherT[M, B] = { m: EitherT[M, A] ⇒
            EitherT(m.run.flatMap {
              case Left(e) ⇒ Monad[M].pure(Left(e): Either[E, B])
              case Right(a) ⇒ f(a).run
            })
          }
        }

        override def flift[M[_] : Monad, A]: M[A] ⇒ EitherT[M, A] = m ⇒ EitherT(m.map(Right.apply))

        override def frun[M[_] : Monad, N[_] : Monad, A](phi: MM[M, N]): EitherT[M, A] ⇒ EitherT[N, A] = { m: EitherT[M, A] ⇒
          EitherT(phi.apply(m.run))
        }
      }

      implicit val baseEither: BaseMonadOf[Either[E, *], EitherT] = BaseMonadOf(new MM[Either[E, *], EitherT[Id, *]] {
        override def apply[A]: Either[E, A] ⇒ EitherT[Id, A] = EitherT[Id, A](_) // Full type annotation is required in the r.h.s.
      })

      // The last monad in the stack is Option. We do not need to supply a transformer for it.
      implicit val monadOption: Monad[Option] = new Monad[Option] {
        override def pure[A]: A ⇒ Option[A] = Some.apply

        override def fmap[A, B](f: A ⇒ B): Option[A] ⇒ Option[B] = _.map(f)

        override def flm[A, B](f: A ⇒ Option[B]): Option[A] ⇒ Option[B] = _.flatMap(f)
      }

      // We would like to write a functor blocks using the following monad stack:
      type MStack1[A] = EitherT[ReaderT[Option, *], A]
      // The lift relation should be produced automatically for all possible lifts within this stack.
      // Let us verify that this works step by step.

      // Should be able to create a monad instance for MStack1 and other monads automatically.
      implicitly[Monad[Option[*]]]
      implicitly[Monad[ReaderT[Option, *]]]
      implicitly[Monad[EitherT[Option, *]]]
      implicitly[Monad[MStack1]]

      implicitly[BaseMonadOf[Reader, ReaderT]]

      // Should be able to create these Liftable instances automatically.
      implicitly[MM[Option, ReaderT[Option, *]]] // Foreign lift.
      implicitly[MM[Reader, ReaderT[Option, *]]] // Base lift.
      implicitly[MM[Option, EitherT[Option, *]]] // Foreign lift.
      implicitly[MM[Either[E, *], EitherT[Option, *]]] // Base lift.
      implicitly[MM[Either[E, *], EitherT[ReaderT[Option, *], *]]] // Base lift.
      implicitly[MM[Option, EitherT[ReaderT[Option, *], *]]] // Two foreign lifts.
      implicitly[MM[Reader, EitherT[ReaderT[Option, *], *]]] // One foreign and one base lift.

      // We should be able to lift any of the three monads to the stack.
      implicitly[MM[Option, MStack1]]
      implicitly[MM[Reader, MStack1]]
      implicitly[MM[Either[E, *], MStack1]]

      val input: Option[Int] = Some(5)

      val resultStack: MStack1[Int] = for {
        x ← input.up[MStack1]
        r ← ask.up[MStack1]
        y ← transform1(r, x).up[MStack1]
        z ← check1(r, y).up[MStack1]
      } yield z

      // Run the stack and get the result value.
      val result: Int = resultStack.run.run.run(r0).getOrElse(Right(0)).getOrElse(0)
      result
    }

    withParams[String, String](
      "123",
      (s, i) ⇒ Try(s.toInt).toOption.map(_ + i),
      (s, i) ⇒ if (i < 10) Left("error") else Right(i)
    ) shouldEqual 128 // 123 + 5

  }
}
