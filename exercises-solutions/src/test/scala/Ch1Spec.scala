package swscala.unit

import org.scalatest._
import swscala.Ch1


class Ch1Spec extends FlatSpec with Matchers {

  "Ch1" should "pass all tests" in {
    // Problem 1
    Ch1.normalize(Seq(1.0, -2.0, 4.0)) shouldEqual Seq(0.25, -0.5, 1.0)
    Ch1.normalize(Seq(0.0, 0.0)) shouldEqual Seq(0.0, 0.0)

    // Problem2
    Ch1.add202DSeq(Seq(Seq(1, 2), Seq(3, 4))) shouldEqual Seq(Seq(21, 22), Seq(23, 24))

    // Problem 3
    Ch1.threeFactor shouldEqual Seq(16, 81, 625)

    // Problem 4
    Ch1.threeFactorAlt shouldEqual Seq(16, 81, 625)

    // Problem 5
    Ch1.compose((x: Int) => x.toString, (y: String) => y + "hello")(3) shouldEqual "3hello"
  }
}
