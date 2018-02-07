package example

import cats.kernel.Semigroup
import cats.{Functor, Monoid, derive}
import cats.syntax.functor._
import cats.syntax.semigroup._
import org.scalatest.FlatSpec

class Chapter07_01_examplesSpec extends FlatSpec with CatsLawChecking {

  behavior of "examples"

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

}
