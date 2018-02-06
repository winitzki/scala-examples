package example

import org.scalacheck.Arbitrary
import org.scalatest.{Assertion, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

object Utils extends GeneratorDrivenPropertyChecks with Matchers {
  def funcEq[A: Arbitrary, B](f: A ⇒ B, g: A ⇒ B)(dataEqual: (B, B) ⇒ Assertion = (x: B, y: B) ⇒ x shouldEqual y): Assertion =
    forAll { (x: A) ⇒ dataEqual(f(x), g(x)) }
}
