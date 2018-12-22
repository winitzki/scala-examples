package example

import org.scalacheck.Arbitrary
import org.scalatest.{Assertion, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

object Utils extends GeneratorDrivenPropertyChecks with Matchers {
  def funcEq[A: Arbitrary, B](f: A ⇒ B, g: A ⇒ B)(dataEqual: (B, B) ⇒ Assertion = (x: B, y: B) ⇒ x shouldEqual y): Assertion =
    forAll { x: A ⇒ dataEqual(f(x), g(x)) }

  val timeReps = 20000

  def time[A](x: ⇒ A): (A, Double) = (1 to timeReps).foldLeft((null.asInstanceOf[A], 0.0)) { case ((_, c), _)  ⇒
    val initTime = System.nanoTime()
    val result = x
    val elapsedTime = System.nanoTime() - initTime
    (result, c + elapsedTime / 1000000000.0 / timeReps)
  }

  def elapsed[A](x: ⇒ A): (A, Double) = {
    val initTime = System.nanoTime()
    val result = x
    val elapsedTime = System.nanoTime() - initTime
    (result, elapsedTime / 1000000000.0)
  }

}
