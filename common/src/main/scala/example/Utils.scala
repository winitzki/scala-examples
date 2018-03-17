package example

import org.scalacheck.Arbitrary
import org.scalatest.{Assertion, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

object Utils extends GeneratorDrivenPropertyChecks with Matchers {
  def funcEq[A: Arbitrary, B](f: A ⇒ B, g: A ⇒ B)(dataEqual: (B, B) ⇒ Assertion = (x: B, y: B) ⇒ x shouldEqual y): Assertion =
    forAll { (x: A) ⇒ dataEqual(f(x), g(x)) }

  def time[A](x: ⇒ A): (A, Double) = {
    val initTime = System.nanoTime()
    val result = x
    val elapsedTime = System.nanoTime() - initTime
    (result, elapsedTime / 1000000000.0)
  }
}
