package example

import cats.syntax.monoid._
import cats.{Functor, Monoid, Semigroup, derive}
import org.scalatest.FlatSpec

class Chapter07_02_examplesSpec extends FlatSpec with FlattenableLawChecking with CatsLawChecking {

    behavior of "misc. examples"

    it should "check semigroup laws for a non-monoid type" in {

      type TripleM[T] = (T, T, T)

      // If P is a monoid type then we can define P × P × P as a semigroup in a special way.
      implicit def tripleMonoid[P: Monoid]: Semigroup[TripleM[P]] = { (x: TripleM[P], y: TripleM[P]) ⇒ (x._1, x._2 combine y._2, y._3) }

      // Use the Int monoid as an example.

      implicit val monoidInt: Monoid[Int] = new Monoid[Int] {
        override def empty: Int = 0

        override def combine(x: Int, y: Int): Int = x + y
      }

      checkCatsSemigroupLaws[TripleM[Int]]()
    }

    it should "check associativity for a semimonad" in {
      type F[A] = Either[A, (A, A)]

      implicit val functorF: Functor[F] = derive.functor[F]
      //    implicit val functorF: Functor[F] = new Functor[F] {
      //      override def map[A, B](fa: F[A])(f: A => B): F[B] = implement
      //    }
      implicit val flattenableF: Flattenable[F] = new Flattenable[F] {

        private def extractLeft[A](fa: F[A]): A = fa match {
          case Left(x) ⇒ x
          case Right((x, _)) ⇒ x
        }

        private def extractRight[A](fa: F[A]): A = fa match {
          case Left(x) ⇒ x
          case Right((_, x)) ⇒ x
        }

        override def flatten[A](ffa: F[F[A]]): F[A] = extractLeft(ffa)

        //      override def flatten[A](ffa: F[F[A]]): F[A] = ffa match {
        //        case Left(fa) ⇒ fa
        //        case Right((fa1, fa2)) ⇒ Right(extractLeft(fa1), extractRight(fa2))
        //      }
      }

      checkFlattenLaws[F, Int, String]()
    }

}
