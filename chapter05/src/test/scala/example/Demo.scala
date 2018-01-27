package example

import org.scalatest.FlatSpec

class Demo extends FlatSpec with CatsLawChecking {

  behavior of "test"

  it should "work" in {
  }

}
