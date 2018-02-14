package swscala.unit

import cats.syntax.functor._
import org.scalacheck.Arbitrary
import org.scalacheck.ScalacheckShapeless._
import org.scalatest.FlatSpec

import example.Filterable._
import example.FilterableLawChecking

import swscala._

class Ch6Spec extends FlatSpec with FilterableLawChecking {

  import Ch6._

  behavior of "Exercises 1"

  it should "Problem 1" in {
    import Part1.Problem1._

    val data: Confucious[String] = ForgotSun("mon", "tue", "wed", "thu", "fri", "sat")

    val result: Confucious[String] = for {
      x ← data
      if x == "thu" // forget before thu
      y = s"$x drink"
    } yield y

    result shouldEqual ForgotWed("thu drink", "fri drink", "sat drink")

    // Fails
    //checkFilterableLawsWithFilter[Confucious, Boolean, Int]()
  }

}
