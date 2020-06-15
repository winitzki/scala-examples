package example

import org.scalatest.{FlatSpec, Matchers}
import io.chymyst.ch.implement
import scala.language.higherKinds

object TypeClassDefinitions {

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

  type Id[A] = A // The identity monad's typeclass instance must be defined elsewhere.

  implicit val monadId = new Monad[Id] {
    override def pure[A]: A ⇒ Id[A] = implement

    override def fmap[A, B](f: A ⇒ B): Id[A] ⇒ Id[B] = implement

    override def flm[A, B](f: A ⇒ Id[B]): Id[A] ⇒ Id[B] = implement
  }

  def pureAsMonadMorphism[M[_] : Monad]: Id ~> M = new ~>[Id, M] {
    override def apply[A]: Id[A] ⇒ M[A] = Monad[M].pure
  }

  def identityAsMonadMorphism[M[_]]: M ~> M = new ~>[M, M] {
    override def apply[A]: M[A] ⇒ M[A] = identity
  }

  trait MTrans[T[_[_], _]] {
    type Base[A] = T[Id, A] // The type constructor describing the base monad.

    def monadT[M[_] : Monad]: Monad[T[M, *]]

    def flift[M[_] : Monad, A]: M[A] => T[M, A]

    def blift[M[_] : Monad, A]: Base[A] => T[M, A] = frun[Id, M, A](pureAsMonadMorphism[M])

    def frun[M[_] : Monad, N[_] : Monad, A](phi: M ~> N): T[M, A] => T[N, A]
  }

  // Produce a monad instance automatically for any transormed monads.
  implicit def monadTransformed[M[_] : Monad, T[_[_], _] : MTrans]: Monad[T[M, *]] = implicitly[MTrans[T]].monadT

  // Natural transformations, monad morphisms, etc.
  trait ~>[P[_], Q[_]] {
    def apply[A]: P[A] ⇒ Q[A]
  }

  // The lift relation.
  case class Liftable[P[_], Q[_]](up: P ~> Q) // The function `up` must be a monad morphism, not just a natural transformation.

  implicit class LiftableSyntax[P[_], A](p: P[A]) {
    def up[Q[_]](implicit liftable: Liftable[P, Q]): Q[A] = liftable.up.apply(p)
  }

  implicit def liftableId[P[_]]: Liftable[P, P] = Liftable[P, P](identityAsMonadMorphism[P])

  implicit def base[KT[_[_], _], M[_] : Monad](implicit kt: MTrans[KT]): Liftable[kt.Base, KT[M, *]] = Liftable(
    new ~>[kt.Base, KT[M, *]] {
      override def apply[A]: kt.Base[A] ⇒ KT[M, A] = kt.blift[M, A]
    }
  )

  implicit def forn[KT[_[_], _], M[_] : Monad](implicit kt: MTrans[KT]): Liftable[M, KT[M, *]] = Liftable(
    new ~>[M, KT[M, *]] {
      override def apply[A]: M[A] ⇒ KT[M, A] = kt.flift[M, A]
    }
  )

}

class Chapter11_03_transformersSpec extends FlatSpec with Matchers {

  import TypeClassDefinitions._

  behavior of "monad transformer typeclass"

  it should "implement transformer instances for ReaderT and EitherT" in {
    def withParams[E, R]() = {

      final case class ReaderT[M[_], A](run: R => M[A]) // The fixed type R must be already defined.

      implicit val mTransReaderT: MTrans[ReaderT] = new MTrans[ReaderT] {
        def monadT[M[_] : Monad]: Monad[ReaderT[M, *]] = new Monad[ReaderT[M, *]] {
          override def pure[A]: A ⇒ ReaderT[M, A] = a ⇒ ReaderT(_ ⇒ Monad[M].pure(a))

          override def fmap[A, B](f: A ⇒ B): ReaderT[M, A] ⇒ ReaderT[M, B] =
            rma ⇒ ReaderT(r ⇒ rma.run(r).map(f))

          override def flm[A, B](f: A ⇒ ReaderT[M, B]): ReaderT[M, A] ⇒ ReaderT[M, B] =
            rma ⇒ ReaderT(r ⇒ rma.run(r).flatMap(a ⇒ f(a).run(r)))
        }

        def flift[M[_] : Monad, A]: M[A] => ReaderT[M, A] = { ma => ReaderT(_ => ma) }

        def frun[M[_] : Monad, N[_] : Monad, A](phi: M ~> N): ReaderT[M, A] => ReaderT[N, A] =
          t => ReaderT(r => phi.apply(t.run(r)))
      }

      final case class EitherT[M[_], A](run: M[Either[E, A]]) // The fixed type E must be already defined.

      implicit val mTransEitherT: MTrans[EitherT] = new MTrans[EitherT] {
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

        override def frun[M[_] : Monad, N[_] : Monad, A](phi: M ~> N): EitherT[M, A] ⇒ EitherT[N, A] = { m: EitherT[M, A] ⇒
          EitherT(phi.apply(m.run))
        }
      }

      // The last monad in the stack is Option. We do not supply a transformer for it.
      implicit val monadOption: Monad[Option] = new Monad[Option] {
        override def pure[A]: A ⇒ Option[A] = Some.apply

        override def fmap[A, B](f: A ⇒ B): Option[A] ⇒ Option[B] = _.map(f)

        override def flm[A, B](f: A ⇒ Option[B]): Option[A] ⇒ Option[B] = _.flatMap(f)
      }

      // The lift relation must be produced automatically.
      // We can now write some functor blocks using these transformers.
      type MyMonadStack[A] = EitherT[ReaderT[Option, *], A]

      // Must create a monad instance for MyMonadStack automatically.
      implicitly[Monad[Option[*]]]
      implicitly[Monad[ReaderT[Option, *]]]
      implicitly[Monad[EitherT[Option, *]]]
      implicitly[Liftable[Option, ReaderT[Option, *]]]
      implicitly[Monad[MyMonadStack]]

      ()
    }

    withParams[Int, String]()

  }
}
