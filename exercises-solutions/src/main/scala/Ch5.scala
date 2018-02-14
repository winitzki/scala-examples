package swscala

import scala.concurrent.Future
import cats.{Bifunctor, Contravariant, Functor, Monoid, Semigroup}
import io.chymyst.ch._

object Ch5 {

  object Problem1 {
    final case class LongBits[T](b: Boolean)

    implicit val longBitsLongEv: LongBits[Long] = LongBits(true)
    implicit val longBitsDoubleEv: LongBits[Double] = LongBits(true)
    implicit val longBitsIntEv: LongBits[Int] = LongBits(false)
    implicit val longBitsShortEv: LongBits[Short] = LongBits(false)
    implicit val longBitsFloatEv: LongBits[Float] = LongBits(false)

    def isLong[T](implicit ev: LongBits[T]): Boolean = ev.b
  }

  object Problem2 {
    // Define a monoid instance for the type String × (1 + Int)
    type Data = (String, Option[Int])

    val monoidInstance = new Monoid[Data] {
      override def empty: Data = ("", None)

      override def combine(x: Data, y: Data): Data = x match {
        case (s1, Some(i1)) => y match {
          case (s2, Some(i2)) => (s1 + s2, Some(i1 + i2))
          case (s2, None) => (s1 + s2, Some(i1))
        }
        case (s1, None) => y match {
          case (s2, Some(i2)) => (s1 + s2, Some(i2))
          case (s2, None) => (s1 + s2, None)
        }
      }
    }
  }

  object Problem3 {
    // If A is a monoid and R any type, define monoid instance for R ⇒ A
    implicit def monoidRToAInstance[R, A](
      implicit evA: Monoid[A]
    ): Monoid[R => A] = new Monoid[R => A] {

      override def empty: R => A = r => evA.empty

      override def combine(x: R => A, y: R => A): R => A = r => evA.combine(x(r), y(r))
    }
  }

  object Problem4 {
    // Show: If S is a semigroup then Option[S] is a monoid
    implicit def monoidInstance[S](implicit evS: Semigroup[S]): Monoid[Option[S]] = new Monoid[Option[S]] {
      override def empty: Option[S] = None

      override def combine(x: Option[S], y: Option[S]): Option[S] = x match {
        case Some(s1) => y match {
          case Some(s2) => Some(evS.combine(s1, s2))
          case _ => Some(s1)
        }
        case _ => y
      }
    }
  }

  // $COVERAGE-OFF$
  object Problem5 {
    import scala.concurrent.ExecutionContext.Implicits.global

    // Define a functor instance for type F[T] = Future[Seq[T]]
    type F[T] = Future[Seq[T]]

    implicit val fFunctorInstance: Functor[F] = new Functor[F] {
      override def map[A, B](fa: F[A])(f: A => B): F[B] = fa.map(_.map(f))
    }
  }
  // $COVERAGE-ON$

  object Problem6 {
    // Define a Cats Bifunctor instance for B[X,Y] ≡ (Int ⇒ X) + Y × Y
    type B[X,Y] = Either[(Int => X), (Y, Y)]

    implicit val bifunctorInstance = new Bifunctor[B] {
      override def bimap[A, B, C, D](
        fab: Either[(Int => A), (B, B)]
      )(f: A ⇒ C, g: B ⇒ D): Either[(Int => C), (D, D)] = implement
    }
  }

  // $COVERAGE-OFF$
  object Problem7 {

    trait Profunctor[F[_]] {
      def dimap[A, B](f: A ⇒ B, g: B ⇒ A): F[A] ⇒ F[B]
    }

    type P[A] = A ⇒ (Int, A)

    implicit val profunctorPInstance = new Profunctor[P] {
      override def dimap[A, B](f: A ⇒ B, g: B ⇒ A): P[A] ⇒ P[B] = implement
    }
  }
  // $COVERAGE-ON$

  object Problem8 {
    // Q[A] = Either[String, (A, Q[A])]
    sealed trait Q[A]
    final case class C1[A](s: String) extends Q[A]
    final case class C2[A](a: A, q: Q[A]) extends Q[A]

    implicit val qFunctorInstance: Functor[Q] = new Functor[Q] {
      override def map[A, B](qa: Q[A])(f: A => B): Q[B] = qa match {
        case C1(s) => C1(s)
        case C2(a, q) => C2(f(a), map(q)(f))
      }
    }
  }

  object Problem9 {
    // If F[A] and G[A] are functors, define functor instance for F[A] x G[A]
    implicit def fgFunctorInstance[F[_], G[_]](
      implicit evF: Functor[F], evG: Functor[G]
    ): Functor[λ[X ⇒ (F[X], G[X])]] = {

      // Define the new functor as a type here, for convenience.
      type FAndG[T] = (F[T], G[T])

      new Functor[FAndG] {
        override def map[A, B](fga: FAndG[A])(t: A ⇒ B): FAndG[B] =
          (evF.map(fga._1)(t), evG.map(fga._2)(t))
      }
    }
  }

  object Problem10 {
    // F[A]: Contravariant, G[A] is a functor
    implicit def fToGFunctorInstance[F[_], G[_]](
      implicit evF: Contravariant[F], evG: Functor[G]
    ): Functor[λ[X ⇒ F[X] => G[X]]] = {

      type FtoG[T] = F[T] => G[T]

      new Functor[FtoG] {
        override def map[A, B](fga: FtoG[A])(t: A => B): FtoG[B] = { (fb: F[B]) =>
          evG.map(fga(evF.contramap(fb)(t)))(t)
        }
      }
    }
  }
}
