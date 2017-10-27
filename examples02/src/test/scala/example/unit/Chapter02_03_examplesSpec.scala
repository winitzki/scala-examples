package example.unit

import example.Chapter02_03_examples._
import org.scalatest.{FlatSpec, Matchers}

class Chapter02_03_examplesSpec extends FlatSpec with Matchers {

  behavior of "sum of digits"

  it should "be computed using unfold" in {
    ex07(0) shouldEqual 0
    ex07(2) shouldEqual 2
    ex07(10) shouldEqual 1
    ex07(123) shouldEqual 6
    ex07(1234) shouldEqual 10
  }

  it should "be computed using a recursive function" in {
    ex07a(0) shouldEqual 0
    ex07a(2) shouldEqual 2
    ex07a(10) shouldEqual 1
    ex07a(123) shouldEqual 6
    ex07a(1234) shouldEqual 10
  }

  it should "be computed using `digits`" in {
    ex07b(0) shouldEqual 0
    ex07b(2) shouldEqual 2
    ex07b(10) shouldEqual 1
    ex07b(123) shouldEqual 6
    ex07b(1234) shouldEqual 10
  }

  behavior of "repeated sum of digits"

  it should "compute digits correctly" in {
    digits(0).toList shouldEqual List(0)
    digits(4).toList shouldEqual List(0, 4)
    digits(10).toList shouldEqual List(0, 0, 1)
    digits(12).toList shouldEqual List(0, 2, 1)
  }

  it should "be computed using unfold" in {
    ex08(0) shouldEqual 0
    ex08(2) shouldEqual 2
    ex08(10) shouldEqual 1
    ex08(123) shouldEqual 6
    ex08(1234) shouldEqual 1
    ex08(999999) shouldEqual 9
  }

  behavior of "repeated sum of squared digits"

  it should "compute sum of squared digits" in {
    ex09(1) shouldEqual 1
    ex09(2) shouldEqual 4
    ex09(45) shouldEqual 41
  }

  it should "be computed using foldLeft" in {
    ex10(1) shouldEqual 1
    ex10(2) shouldEqual 4
    ex10(45) shouldEqual 4
    ex10(123) shouldEqual 4
  }

  it should "find only 1,2,3,4 for first million integers" in {
    (1 to 1000000).map(ex10).count(_ > 4) shouldEqual 0
  }

}
