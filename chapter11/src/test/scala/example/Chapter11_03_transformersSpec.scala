package example

import org.scalatest.{FlatSpec, Matchers}
import io.chymyst.ch.implement
import scala.language.higherKinds

object AuxTC {

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

  trait ~>[P[_], Q[_]] {
    def apply[A]: P[A] ⇒ Q[A]
  }

  case class Liftable[P[_], Q[_]](up: P ~> Q) // This function must be a monad morphism.

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

  import AuxTC._

  behavior of "monad transformer typeclass"

  it should "implement transformer instance for ReaderT and EitherT" in {
    def withParams[E, R] = {

      type ReaderT[M[_], A] = R => M[A] // The fixed type R must be already defined.
      implicit val mTransReaderT = new MTrans[ReaderT] {
        def monadT[M[_] : Monad]: Monad[ReaderT[M, *]] = new Monad[ReaderT[M, *]] {
          override def pure[A]: A ⇒ ReaderT[M, A] = a ⇒ _ ⇒ Monad[M].pure(a)

          override def fmap[A, B](f: A ⇒ B): ReaderT[M, A] ⇒ ReaderT[M, B] = rma ⇒ r ⇒ rma(r).map(f)

          override def flm[A, B](f: A ⇒ ReaderT[M, B]): ReaderT[M, A] ⇒ ReaderT[M, B] = rma ⇒ r ⇒ rma(r).flatMap(a ⇒ f(a)(r))
        }

        def flift[M[_] : Monad, A]: M[A] => R => M[A] = { ma => _ => ma }

        def frun[M[_] : Monad, N[_] : Monad, A](phi: M ~> N): (R => M[A]) => R => N[A] = t => r => phi.apply(t(r))
      }

      type EitherT[M[_], A] = M[Either[E, A]]
      implicit val mTransEitherT = new MTrans[EitherT] {
        override def monadT[M[_] : Monad]: Monad[EitherT[M, *]] = new Monad[EitherT[M, *]] {
          override def pure[A]: A ⇒ EitherT[M, A] = a ⇒ Monad[M].pure(Right(a): Either[E, A])

          override def fmap[A, B](f: A ⇒ B): EitherT[M, A] ⇒ EitherT[M, B] = { m: M[Either[E, A]] ⇒ m.map(_.map(f)) }

          override def flm[A, B](f: A ⇒ EitherT[M, B]): EitherT[M, A] ⇒ EitherT[M, B] = { m: M[Either[E, A]] ⇒
            m.flatMap {
              case Left(e) ⇒ Monad[M].pure(Left(e): Either[E, B])
              case Right(a) ⇒ f(a)
            }
          }
        }

        override def flift[M[_] : Monad, A]: M[A] ⇒ EitherT[M, A] = _.map(Right.apply)

        override def frun[M[_] : Monad, N[_] : Monad, A](phi: M ~> N): EitherT[M, A] ⇒ EitherT[N, A] = phi.apply[Either[E, A]]
      }

      // Write some functor blocks using this.
    }


  }
}

