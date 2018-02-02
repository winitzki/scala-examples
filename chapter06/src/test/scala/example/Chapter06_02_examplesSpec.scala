package example

import cats.Functor
import org.scalatest.FlatSpec
import cats.syntax.functor._
import io.chymyst.ch._
import org.scalatest.FlatSpec
import org.scalacheck.ScalacheckShapeless._
import Filterable._

class Chapter06_02_examplesSpec  extends FlatSpec with FilterableLawChecking {


  behavior of "filterable type class"

  // Type 1 + T x T
  it should "declare a Filterable instance for stingy product" in {
    type P[T] = Option[(T, T)]

    implicit val functorP: Functor[P] = new Functor[P] {
      override def map[A, B](fa: P[A])(f: A => B): P[B] = fa.map { case (x, y) ⇒ (f(x), f(y)) }
    }

    implicit val filterablePStingy: Filterable[P] = new Filterable[P]() {
      override def flatten[A](fa: P[Option[A]]): P[A] = fa.flatMap {
        case (Some(x), Some(y)) ⇒ Some((x, y))
        case _ ⇒ None
      }
    }

    // Check laws.
    checkFilterableLaws[P, Int, String, Boolean]()

  }

}
