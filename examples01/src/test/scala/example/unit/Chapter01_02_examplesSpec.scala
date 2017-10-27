package example.unit

import org.scalatest.{FlatSpec, Matchers}

import example.Chapter01_02_examples._

class Chapter01_02_examplesSpec extends FlatSpec with Matchers {

  behavior of "examples 1-3"

  it should "compute functions correctly" in {
    ex01(1) shouldEqual 21
    ex01a(1) shouldEqual 21

    val r1 = ex02(10)
    r1(100) shouldEqual 110

    val r2 = ex02a(10)
    r2(100) shouldEqual 110

    ex03(53) shouldEqual false
    ex03a(53) shouldEqual false
    ex03(52) shouldEqual true
    ex03a(52) shouldEqual true

  }

  behavior of "other examples"

  it should "compute results" in {
    ex04(Seq(0.0, 1.0, 3.0, 4.0)) shouldEqual 2.0
    ex05(1000) shouldEqual 3.14159/2.0 +- 0.001
    ex06(Seq(
      Set(1,2),
      Set(1,2,3),
      Set(2),
      Set(1,2,3),
      Set(1,2,3,4),
      Set(4,2)
    )) shouldEqual Seq(
      Set(1,2,3),
      Set(3,2,1),
      Set(1,2,3,4)
    )
  }
}
