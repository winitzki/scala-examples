package example

import example.PipeOps.PipeOp
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class FastComposeSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  behavior of "correctness of FastCompose"

  it should "convert a single function to counted" in {
    val f1: Boolean ⇒ Int = b ⇒ if (b) 10 else 11
    val f2: Int ⇒ String = x ⇒ s"have $x"
    val f3: Int ⇒ Boolean = x ⇒ x % 2 == 0
    forAll { x: Boolean ⇒ x |> f1 |> f3 shouldEqual x }
    
  }

  behavior of "speed of FastCompose"

}
