package example.unit

import example.HelloWorld
import org.scalatest.{FlatSpec, Matchers}

class ExampleSpec extends FlatSpec with Matchers {

  behavior of "text"

  it should "compute text message" in {
    HelloWorld.computeMessage() shouldEqual "hello: "
  }

  behavior of "number"

  it should "compute result number" in {
    HelloWorld.computeNumber() shouldEqual 123
  }
}
