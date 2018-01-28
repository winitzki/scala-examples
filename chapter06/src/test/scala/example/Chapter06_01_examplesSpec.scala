package example

import io.chymyst.ch._
import org.scalatest.FlatSpec

class Chapter06_01_examplesSpec extends FlatSpec {

  behavior of "Cats filterable type class"

  it should "declare a Filterable instance for stingy product of two Options" in {
    type P[T] = (Option[T], Option[T])

    val filterablePStingy: Filterable[P] = new Filterable[P] {
      override def flatten[A](fa: P[Option[A]]): P[A] = fa match {
        case (Some(x), Some(y)) ⇒ (x, y)
        case _ ⇒ (None, None)
      }
    }

    val filterablePNormal: Filterable[P] = new Filterable[P] {
      // curryhoward fails to pick the right implementation here in any case
      override def flatten[A](fa: P[Option[A]]): P[A] = fa match {
        case (x, y) ⇒ (x.flatten, y.flatten)
      }
    }


  }

}
