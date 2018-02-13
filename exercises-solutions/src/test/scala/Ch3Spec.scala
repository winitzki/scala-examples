package swscala.unit

import org.scalacheck.Arbitrary
import org.scalacheck.ScalacheckShapeless._
import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import swscala._


class Ch3Spec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  "Ch3Ex1" should "pass all tests" in {

    import Ch3Ex1._

    // Problem 1
    id(id[Int])(3) shouldEqual 3
    val int2Str = (x: Int) => x.toString
    id(id[Int => String])(int2Str) shouldEqual int2Str

    // Problem 2
    val constBoolInt = const[Boolean, Int]
    const[Boolean => Int => Boolean, String](constBoolInt)("hello") shouldEqual constBoolInt

    // Problem 3
    val add1: Int => Int = n => n + 1
    val n = 0
    twice[Int](add1)(n) shouldEqual 2
    twice(twice[Int])(add1)(n) shouldEqual 4
    twice(twice(twice[Int]))(add1)(n) shouldEqual 16
    twice(twice(twice(twice[Int])))(add1)(n) shouldEqual 256 // 16^2

    // Problem 4
    thrice[Int](add1)(n) shouldEqual 3
    thrice(thrice[Int])(add1)(n) shouldEqual 27
    thrice(thrice(thrice[Int]))(add1)(n) shouldEqual 27 * 27 * 27

    // Problem 5
    ence[Int](add1, 0)(n) shouldEqual 0
    ence[Int](add1, 1)(n) shouldEqual 1
    ence[Int](add1, 2)(n) shouldEqual 2
    ence[Int](add1, 3)(n) shouldEqual 3
    ence[Int](add1, 10)(n) shouldEqual 10

    // Problem 6
    def f(x: Int, y: Int) = x - y // check that f(10, 2) gives 8
    val g = swapFunc(f) // now check that g(10, 2) gives (– 8)
    f(10, 2) shouldEqual 8
    g(10, 2) shouldEqual -8
    g(2, 10) shouldEqual 8

    // Problem 7
    "def r[A, B, C]: (((A => B) => B) => C) => A => C = p ⇒ q ⇒ p(t ⇒ t(q))" should compile

    // Problem 9
    "def s[B, C, D]: ((((C => D) => D) => B) => C) => (((C => D) => D) => B) => B = f ⇒ g ⇒ g(x ⇒ x(f(g)))" should compile
  }

  "Ch3Ex2" should "pass all tests" in {

    import Ch3Ex2._

    // Problem 1
    ClosedCell().isBomb shouldEqual false
    BombCell().isBomb shouldEqual true
    OpenCell(3).isBomb shouldEqual false

    // Problem 2
    val inputCells = Seq(
      Seq(OpenCell(0), BombCell(), ClosedCell()),
      Seq(OpenCell(1), BombCell(), ClosedCell()),
      Seq(OpenCell(0), BombCell(), ClosedCell())
    )
    numCellsShowingZeroNeighborBombs(inputCells) shouldEqual 2

    // Problem 3
    solve1(0, 0) shouldEqual AllXRoots()
    solve1(0, 3) shouldEqual NoRoot()
    solve1(2, 4) shouldEqual OneRoot(-2)

    // Problem 4
    solve1(Seq((0, 0), (0, 1), (2, 4), (3, 6), (4, -8), (-9, 9))) shouldEqual Seq(-2, -2 , 2, 1)

    // Problem 5
    f1(Some((1, "a"))) shouldEqual (Some(1), Some("a"))
    f1(None) shouldEqual (None, None)

    f2(Left(1)) shouldEqual (Some(1), None)
    f2(Right("a")) shouldEqual (None, Some("a"))

    var f3In: Either[Int, Either[String, Boolean]] = Left(1)
    var f3Out: Either[Either[Int, String], Boolean] = Left(Left(1))
    f3(f3In) shouldEqual f3Out
    f3In = Right(Left("a"))
    f3Out = Left(Right("a"))
    f3(f3In) shouldEqual f3Out
    f3In = Right(Right(false))
    f3Out = Right(false)
    f3(f3In) shouldEqual f3Out
  }

  "Ch3Ex3" should "pass all tests" in {

    import Ch3Ex3._

    // Problem 1
    "val myTUEmpty: MyTU[String, Int] = EmptyValuesTU[String, Int]" should compile
    """val myTUAnd: MyTU[String, Boolean] = TAndU[String, Boolean]("a", true)""" should compile
    """val myTUIntAndT: MyTU[String, Boolean] = IntAndT[String, Boolean](1, "hello")""" should compile
    """val myTUStringAndU: MyTU[String, Boolean] = StringAndU[String, Boolean]("s", false)""" should compile

    // Problem 2
    """def p2Forward[A, B, C](fOfA: A => Either[B, C]): Either[A => B, A => C] = {
         Left((a: A) => fOfA(a))
    }""" shouldNot typeCheck

    // Problem 3
    def p3Check[A: Arbitrary]() = {
      forAll { (p3T1: P3T1[A]) => p3Backward(p3Forward(p3T1)) shouldEqual p3T1  }
      forAll { (p3T2: P3T2[A]) => p3Forward(p3Backward(p3T2)) shouldEqual p3T2  }
    }
    p3Check[String]

    // Problem 4
    def p4Check[A: Arbitrary, B: Arbitrary]() = {
      forAll { (x: OptEither[A, B]) => map[A, B, B](x)(identity[B]) shouldEqual x  }
      forAll { (x: OptEither[A, B]) => flatMap[A, B, B](x)((b: B) => OptRight(b)) shouldEqual x }

      // Verify for equivalent type Either[Option[A], B]
      forAll { (x: Either[Option[A], B]) => x.map(identity[B]) shouldEqual x  }
      forAll { (x: Either[Option[A], B]) => x.flatMap((b: B) => Right(b)) shouldEqual x  }
    }
    p4Check[String, Boolean]

    // Problem 5
    def p5Check[T: Arbitrary, U: Arbitrary]() = {
      // Checking that map for MyT doesn't lose information
      forAll {
        (x: MyT[T], b: Boolean, s: String) => {
          val mappedMyTVals: MyTVals[T] = mapMyT(x)(identity[T])(b)
          x(b) match {
            case StringToT(sToT) => {
              mappedMyTVals match {
                // sToT is a function, so we must evaluate it for comparison
                case StringToT(sToTMapped) => sToT(s) shouldEqual sToTMapped(s)
                case _ => fail()
              }
            }
            case otherMyT@_ => otherMyT shouldEqual mappedMyTVals
          }
        }
      }

      // Checking that map for MyTU dosn't lose information
      forAll { (x: MyTU[T, U]) => mapMyTU(x)(identity[T]) shouldEqual x }
    }
    p5Check[String, Int]
    p5Check[Int, Boolean]
    p5Check[Boolean, String]

    // Problem 6.1
    def p6P1Check[S: Arbitrary, A: Arbitrary]()(implicit arbPsa: Arbitrary[P6State[S, A]]) = {
      forAll {
        (x: P6State[S, A], s: S) => {
          p6P1Map(x)(identity[(S, A)])(s) shouldEqual x(s)
        }
      }
    }
    p6P1Check[String, Int]

    // Problem 6.2
    def p6P2Check[A: Arbitrary, B: Arbitrary, Z: Arbitrary]() = {
      // Test p6P2Map
      forAll { (az: Either[A, Z]) => p6P2Map(az)(identity[A]) shouldEqual az }

      // Test p6P2Map2
      forAll {
        (az: Either[A, Z], bz: Either[B, Z]) => {
          az match {
            case Left(a) => bz match {
              case Left(b) => p6P2Map2(az)(bz)(a => b => a) shouldEqual az
              case Right(z) => p6P2Map2(az)(bz)(a => b => a) shouldEqual bz
            }
            case Right(z) => p6P2Map2(az)(bz)(a => b => a) shouldEqual az
          }
        }
      }
    }
    p6P2Check[String, Int, Boolean]

    // Problem 6.3
    def p6P3Check[E: Arbitrary, A: Arbitrary]()(implicit arbREA: Arbitrary[Reader[E, A]]) = {
      forAll {
        (r: Reader[E, A], e: E) => {
          p6P3FlatMap(r)(a => e2 => a)(e) shouldEqual r(e)
        }
      }
    }
    p6P3Check[String, Int]

    // Problem 7
    def p7Check[Z: Arbitrary, A: Arbitrary]()(implicit arbD: Arbitrary[Density[Z, A]], arbAToZ: Arbitrary[A => Z]) = {
      forAll {
        (dza: Density[Z, A], aToZ: A => Z) => {
          p7Map(dza)(identity[A])(aToZ) shouldEqual dza(aToZ)
          p7FlatMap(dza){a => aToZ2:(A => Z) => a}(aToZ) shouldEqual dza(aToZ)
        }
      }
    }
    p7Check[String, Int]

    // Problem 8
    def p8Check[R: Arbitrary, T: Arbitrary]()(implicit arbC: Arbitrary[Cont[R, T]], arbTToR: Arbitrary[T => R]) = {
      forAll {
        (contRT: Cont[R, T], tToR: T => R) => {
          p8Map(contRT)(identity[T])(tToR) shouldEqual contRT(tToR)
          p8FlatMap(contRT){t => tToR2:(T => R) => tToR2(t)}(tToR) shouldEqual contRT(tToR)
        }
      }
    }
    p8Check[String, Int]

    // Problem 9
    def p9Check[A: Arbitrary]() = {
      forAll {
        (tr3: Tr3[A]) => {
          p9Map(tr3)(identity[A]) shouldEqual tr3
        }
      }
    }
    p9Check[Boolean]
  }
}
