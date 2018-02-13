package swcala.unit

import org.scalatest._
import swscala._


class Ch2Spec extends FlatSpec with Matchers {

  "Ch2Ex1" should "pass all tests" in {

    import Ch2Ex1._

    // Problem 1
    p1 shouldEqual Seq((0,1), (0,2), (0,3), (0,4), (0,5), (0,6), (0,7), (0,8), (0,9), (1,0), (1,1), (1,2), (1,3), (1,4), (1,5), (1,6), (1,7), (1,8), (1,9), (2,0), (2,1), (2,2), (2,3), (2,4), (2,5), (2,6), (2,7), (2,8), (2,9), (3,0), (3,1), (3,2), (3,3), (3,4), (3,5), (3,6), (3,7), (3,8), (3,9), (4,0), (4,1), (4,2), (4,3), (4,4), (4,5), (4,6), (4,7), (4,8), (4,9), (5,0), (5,1), (5,2), (5,3), (5,4), (6,0), (6,1), (6,2), (7,0), (7,1), (7,2), (8,0), (8,1), (9,0), (9,1))

    // Problem 2
    p2(Seq("a", "b", "c"), Seq(false, true, true)) shouldEqual Seq("b", "c")

    // Problem 3
    p3(Seq(1,3,2,4)) shouldEqual Seq((1,true),(3,false),(2,true))

    // Problem 4
    p4(Seq(true, false), Seq(Set(1), Set(2))) shouldEqual Map(Set(1) -> true, Set(2) -> false)

    // Problem 5
    p5(Seq("a", "b", "c"), Seq(3, 1, 2)) shouldEqual Seq("b", "c", "a")
  }

  "Ch2Ex2" should "pass all tests" in {

    import Ch2Ex2._

    // Problem 1
    val p1Input = Seq(("a", 1), ("b", 2), ("a", 2), ("b", 4), ("c", 10))
    p1(p1Input) shouldEqual Map("b" -> 6, "a" -> 3, "c" -> 10)

    // Problem 2
    val p2Input1 = Seq(List(2,6,3,8), List(20, 60, 30, 80))
    p2(p2Input1) shouldEqual Seq(List(8, 6, 3), List(80, 60, 30))
    val p2Input2 = Seq(List(2,6,3,8), List(20, 60))
    p2(p2Input2) shouldEqual Seq(List(8, 6, 3), List(60, 20))

    // Problem 3
    p3(Set(1, 2), Set(4, 5)) shouldEqual(Set((1, 4), (1, 5), (2, 4), (2, 5)))
    p3(Set("1", "2"), Set(4, 5)) shouldEqual(Set(("1", 4), ("1", 5), ("2", 4), ("2", 5)))
    val p4Input = Seq(
      Map("a" -> 11, "b" -> 21, "c" -> 1),
      Map("a" -> 12, "b" -> 22, "d" -> 2),
      Map("a" -> 13, "b" -> 23, "e" -> 3),
      )
    val p4ExpectedOutput = Map(
      "a" -> Seq(11, 12, 13),
      "b" -> Seq(21, 22, 23),
      "c" -> Seq(1),
      "d" -> Seq(2),
      "e" -> Seq(3)
    )
    p4(p4Input) shouldEqual p4ExpectedOutput
  }

  "Ch2Ex3" should "pass all tests" in {

    import Ch2Ex3._

    // Problem 1
    sqSumDigits(123) shouldEqual 14
    sqSumDigits(1) shouldEqual 1

    // Problem 2
    isNotCubeHappy(30) shouldEqual true
    isNotCubeHappy(23) shouldEqual true
    isNotCubeHappy(45) shouldEqual true
    isNotCubeHappy(10) shouldEqual false
    isNotCubeHappy(100) shouldEqual false
    isNotCubeHappy(1) shouldEqual false

    // Problem 3
    collatz(12) shouldEqual Seq(12, 6, 3, 10, 5, 16, 8, 4, 2, 1)

    // Problem 4
    val p4ExpectedOutput = Set(
      Set(1, 4, 6),
      Set(1, 4, 7),
      Set(1, 5, 6),
      Set(1, 5, 7),
      Set(2, 4, 6),
      Set(2, 4, 7),
      Set(2, 5, 6),
      Set(2, 5, 7),
    )
    set3(Set(1, 2), Set(4, 5), Set(6, 7)) shouldEqual p4ExpectedOutput

    // Problem 5
    val p5Input = Set(
      Set(1, 2),
      Set(4, 5),
      Set(6, 7)
    )
    val p5ExpectedOutput = p4ExpectedOutput
    set3Alt(p5Input) shouldEqual p5ExpectedOutput
  }
}
