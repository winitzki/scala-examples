package swscala.unit

import org.scalacheck.Arbitrary
import org.scalacheck.ScalacheckShapeless._
import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import swscala._

class Ch4Spec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  "Ch4Ex1" should "pass all tests" in {
    import Ch4Ex1._

    // Problem 1
    // Identity law.
    forAll { (x: P1Data[Int]) ⇒ p1Fmap(identity[Int])(x) shouldEqual x }

    // Composition law.
    forAll { (x: P1Data[Int], f: Int ⇒ String, g: String ⇒ Long) ⇒
      p1Fmap(f andThen g)(x) shouldEqual (p1Fmap(f) andThen p1Fmap(g)) (x) }


    // Problem 2
    def p2DataEqual[A](
      d1: P2Data[A], d2: P2Data[A]
    )(implicit arbAToStr: Arbitrary[A => String]) = {
      forAll { (aToString: A => String) =>
        val (a1, iOrA1) = d1.d(aToString)
        val (a2, iOrA2) = d2.d(aToString)
        a1 shouldEqual a2
        iOrA1 shouldEqual iOrA2
      }
    }

    // Identity law.
    forAll { (x: P2Data[Int]) ⇒ p2DataEqual(p2Fmap(identity[Int])(x), x) }

    // Composition law.
    forAll { (x: P2Data[Int], f: Int ⇒ String, g: String ⇒ Long) ⇒
      p2DataEqual(
        p2Fmap(f andThen g)(x),
        (p2Fmap(f) andThen p2Fmap(g))(x)
      )
    }


    // Problem 3
    def p3DataEqual[A, B](
      d1: P3Data[A, B], d2: P3Data[A, B]
    )(implicit arbA: Arbitrary[A], arbAOrB: Arbitrary[Either[A, B]]) = {
      forAll { (a: A, aOrB: Either[A, B]) =>
        val (aToString1, aOrBToInt1) = d1
        val (aToString2, aOrBToInt2) = d2
        aToString1(a) shouldEqual aToString2(a)
        aOrBToInt1(aOrB) shouldEqual aOrBToInt2(aOrB)
      }
    }

    // Identity Law for B => C
    forAll { (x: P3Data[Int, String]) ⇒ p3DataEqual(p3ContraFmapBC(identity[String])(x), x) }

    // Composition Law for B => C
    forAll { (x: P3Data[Boolean, Int], f: String ⇒ Int, g: Long ⇒ String) ⇒
      p3DataEqual(
        p3ContraFmapBC(g andThen f)(x),
        (p3ContraFmapBC[Boolean, Int, String](f) andThen p3ContraFmapBC(g))(x)
      )
    }

    // Identity Law for A => C
    forAll { (x: P3Data[String, Int]) ⇒ p3DataEqual(p3ContraFmapAC(identity[String])(x), x) }

    // Composition Law for A => C
    forAll { (x: P3Data[Int, Boolean], f: String ⇒ Int, g: Long ⇒ String) ⇒
      p3DataEqual(
        p3ContraFmapAC(g andThen f)(x),
        (p3ContraFmapAC[Int, Boolean, String](f) andThen p3ContraFmapAC(g))(x)
      )
    }


    // Problem 4
    def p4DataEqual[A](d1: P4Data[A], d2: P4Data[A])(
      implicit arbOAToStr: Arbitrary[Option[A => String]],
      arbOAToInt: Arbitrary[Option[A => Int]]
    ) = forAll {
      (oAToStr: Option[A => String], oAToInt: Option[A => Int]) => {
        d1(oAToStr)(oAToInt) shouldEqual d2(oAToStr)(oAToInt)
      }
    }

    // Identity law
    forAll { (x: P4Data[String]) =>
      p4DataEqual(p4Fmap(identity[String])(x), x)
    }

    // Composition law
    forAll { (x: P4Data[Int], f: Int ⇒ String, g: String ⇒ Boolean) ⇒
      p4DataEqual(
        p4Fmap[Int, Boolean](f andThen g)(x),
        (p4Fmap(f) andThen p4Fmap(g))(x)
      )
    }


    // Problem 5

    // Check equality of BOrXToB[B, X]
    def bOrXToBEqual[B, X](
      d1: BOrXToB[B, X], d2: BOrXToB[B, X]
    )(implicit arbX: Arbitrary[X]) = {
      forAll { (x: X) =>
        d1 match {
          case Left(b1) => d2 match {
            case Left(b2) => b1 shouldEqual b2
            case Right(_) => fail
          }
          case Right(xToB1) => d2 match {
            case Left(_) => fail
            case Right(xToB2) => xToB1(x) shouldEqual xToB2(x)
          }
        }
      }
    }

    // Identity Law for fmapBOrXToB
    forAll { (x: BOrXToB[Int, String]) ⇒
      bOrXToBEqual(fmapBOrXToB[Int, Int, String](identity[Int])(x), x) }

    // Composition Law for fmapBOrXToB
    forAll { (x: BOrXToB[Int, Boolean], f: Int ⇒ String, g: String ⇒ Long) ⇒
      bOrXToBEqual(
        fmapBOrXToB[Int, Long, Boolean](f andThen g)(x),
        (fmapBOrXToB[Int, String, Boolean](f) andThen fmapBOrXToB(g))(x)
      )
    }

    // Check equality of P4Data[B]
    def p5DataEqual[B](d1: P5Data[B], d2: P5Data[B]) = {
      val (bOrIntToB1, bOrStrToB1) = d1
      val (bOrIntToB2, bOrStrToB2) = d2

      forAll { (i: Int) =>
        bOrIntToB1 match {
          case Left(b1) => bOrIntToB2 match {
            case Left(b2) => b1 shouldEqual b2
            case Right(_) => fail
          }
          case Right(intToB1) => bOrIntToB2 match {
            case Left(_) => fail
            case Right(intToB2) => intToB1(i) shouldEqual intToB2(i)
          }
        }
      }

      forAll { (s: String) =>
        bOrStrToB1 match {
          case Left(b1) => bOrStrToB2 match {
            case Left(b2) => b1 shouldEqual b2
            case Right(_) => fail
          }
          case Right(strToB1) => bOrStrToB2 match {
            case Left(_) => fail
            case Right(strToB2) => strToB1(s) shouldEqual strToB2(s)
          }
        }
      }
    }

    // Identity Law
    forAll { (x: P5Data[String]) ⇒ p5DataEqual(p5Fmap(identity[String])(x), x) }

    // Composition Law
    forAll { (x: P5Data[Boolean], f: Boolean ⇒ Int, g: Int ⇒ String) ⇒
      p5DataEqual(
        p5Fmap(f andThen g)(x),
        (p5Fmap(f) andThen p5Fmap(g))(x)
      )
    }


    // Problem 6
    import io.chymyst.ch._

    // Checking Param A
    // 0 possible implementations
    def allFmapsA[A, B, C] = allOfType[(A => C) => Result[A, B] => Result[C, B]]
    // 0 possible implementations
    def allContraFmapsA[A, B, C] = allOfType[(C => A) => Result[A, B] => Result[C, B]]

    // Checking Param B
    // > 0 possible implementations
    def allFmapsB[A, B, C] = allOfType[(B => C) => Result[A, B] => Result[A, C]]
    allFmapsB[Int, String, Boolean].length should be > 0
    // def fmapResult1[A, B, C](f: B => C): Result[A, B] => Result[A, C] = implement

    // 0 possible implementations
    def allContraFmapsB[A, B, C] = allOfType[(C => B) => Result[A, B] => Result[A, C]]
    allContraFmapsB[Int, String, Boolean].length shouldEqual 0
  }

}
