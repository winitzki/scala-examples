package example

import org.scalacheck.Arbitrary._
import org.scalacheck._
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, TableDrivenPropertyChecks}
import org.scalatest.{Assertion, FlatSpec, Matchers}

class Chapter03_03_examples1Spec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks with TableDrivenPropertyChecks {

  // The ScalaTest library does not have an adapter for "exists" that works similarly to "forAll".

  // See https://groups.google.com/forum/#!msg/scalacheck/Ped7joQLhnY/gNH0SSWkKUgJ

  // "The reason there's no exists in GeneratorDrivenPropertyChecks is that
  // I haven't every had a user request it (until you asked about it now)."
  // -- Bill Venners

  def existsSome[T: Arbitrary](test: T ⇒ Assertion): Assertion = {
    val sampleValues = Iterator.continually(arbitrary[T].sample).collect { case Some(x) ⇒ x }.take(100).toSeq
    val sampleTable = Table("sample", sampleValues: _*)
    exists(sampleTable)(test)
  }

  behavior of "Curry-Howard correspondence"

  // ∀A : A × 1 ≡ A
  it should "verify example 1" in {
    def f1[A]: ((A, Unit)) ⇒ A = {
      case (a, b) ⇒ a
    }

    def f2[A]: A ⇒ (A, Unit) = a ⇒ (a, ())

    forAll { (n: Int) ⇒ f1(f2(n)) shouldEqual n }
    forAll { (x: (String, Unit)) ⇒ f2(f1(x)) shouldEqual x }
  }

  // It is not true that ∀A : A + 1 ≡ 1, although these are equivalent in logic.
  it should "falsify example 2" in {
    def f1[A]: Either[A, Unit] ⇒ Unit = _ ⇒ () // "information loss"

    def f2[A]: Unit ⇒ Either[A, Unit] = _ ⇒ Right(())

    val u: Unit = ()

    f1(f2(u)) shouldEqual u

    existsSome { (v: Either[Int, Unit]) ⇒ f2(f1(v)) should not equal v }
  }

  // ∀A∀B∀C : (A × B) × C ≡ A × (B × C)
  it should "verify example 3" in {
    def f1[A, B, C]: (((A, B), C)) ⇒ (A, (B, C)) = {
      case ((x, y), z) ⇒ (x, (y, z))
    }

    def f2[A, B, C]: ((A, (B, C))) ⇒ ((A, B), C) = {
      case (x, (y, z)) ⇒ ((x, y), z)
    }

    forAll { (q: ((Int, Boolean), String)) ⇒ f2(f1(q)) shouldEqual q }

    forAll { (q: (Int, (Boolean, String))) ⇒ f1(f2(q)) shouldEqual q }
  }

  // ∀A∀B∀C : (A + B) × C ≡ A × C + B × C
  it should "verify example 4" in {
    def f1[A, B, C]: ((Either[A, B], C)) ⇒ Either[(A, C), (B, C)] = {
      case (eab, c) ⇒ eab match {
        case Left(a) ⇒ Left((a, c))
        case Right(b) ⇒ Right((b, c))
      }
    }

    def f2[A, B, C]: Either[(A, C), (B, C)] ⇒ (Either[A, B], C) = {
      case Left((a, c)) ⇒ (Left(a), c)
      case Right((b, c)) ⇒ (Right(b), c)
    }

    forAll { (q: (Either[Int, Boolean], String)) ⇒ f2(f1(q)) shouldEqual q }
    forAll { (q: Either[(Int, String), (Boolean, String)]) ⇒ f1(f2(q)) shouldEqual q }
  }

  // ∀A∀B∀C : (A + B) ⇒ C ≡ (A ⇒ C) × (B ⇒ C)
  it should "verify example 5" in {
    def f1[A, B, C]: (Either[A, B] ⇒ C) ⇒ (A ⇒ C, B ⇒ C) = { p ⇒
      (a ⇒ p(Left(a)), b ⇒ p(Right(b)))
    }

    def f2[A, B, C]: ((A ⇒ C, B ⇒ C)) ⇒ Either[A, B] ⇒ C = {
      case (ac, bc) ⇒ eab ⇒ eab match {
        case Left(a) ⇒ ac(a)
        case Right(b) ⇒ bc(b)
      }
    }

    forAll { (p: Either[Int, Boolean] ⇒ String) ⇒
      val p12: Either[Int, Boolean] ⇒ String = f2(f1(p))
      forAll { (ab: Either[Int, Boolean]) ⇒ p(ab) shouldEqual p12(ab) }
    }

    forAll { (p: (Int ⇒ String, Boolean ⇒ String)) ⇒
      val p21: (Int ⇒ String, Boolean ⇒ String) = f1(f2(p))
      forAll { (a: Int, b: Boolean) ⇒
        p._1(a) shouldEqual p21._1(a)
        p._2(b) shouldEqual p21._2(b)
      }
    }
  }

  // It is not true that ∀A∀B∀C : A + B×C ≡ (A+B) × (A+C), although these are equivalent in logic.
  it should "falsify example 6" in {
    def f1[A, B, C]: Either[A, (B, C)] ⇒ (Either[A, B], Either[A, C]) = {
      case Left(a) ⇒ (Left(a), Left(a))
      case Right((b, c)) ⇒ (Right(b), Right(c))
    }

    def f2[A, B, C]: ((Either[A, B], Either[A, C])) ⇒ Either[A, (B, C)] = {
      case (ab, ac) ⇒ ab match {
        case Left(a) ⇒ Left(a)
        case Right(b) ⇒ ac match {
          case Left(a) ⇒ Left(a)
          case Right(c) => Right((b, c))
        }
      }
    }

    def check[A: Arbitrary, B: Arbitrary, C: Arbitrary]() = {
      forAll { (v: Either[A, (B, C)]) ⇒ f2(f1(v)) shouldEqual v }

      existsSome { (v: (Either[A, B], Either[A, C])) ⇒ f1(f2(v)) should not equal v }
    }

    check[Int, Boolean, String]()
    check[(Int, Double, Float), Option[Option[Option[String]]], Set[Int]]()
  }

}
