package example

import org.scalacheck.Arbitrary
import org.scalatest.{Assertion, FlatSpec}

class Chapter05_01_examplesSpec extends FlatSpec with CatsLawChecking {

  behavior of "simple type classes"

  it should "implement GADT" in {
    sealed trait MyTC[A]
    final case class Case1(d: Double) extends MyTC[Int]
    final case class Case2() extends MyTC[String]

    "def f[A]: MyTC[A] = Case2()" shouldNot typeCheck

    type T1 = MyTC[Int] // PTTF applied to Int
    type T2 = MyTC[String] // PTTF applied to String

    val x1: T1 = Case1(1.0) // OK
    val x2: T2 = Case2() // OK

    (x2 match {
      case Case2() ⇒ 0.0
    }) shouldEqual 0.0

    type T3 = MyTC[Boolean] // PTTF applied to Boolean, outside its type domain. The Scala compiler does not discover this.

    "val x3: T3 = Case2()" shouldNot typeCheck // But, we are unable to compute any values of x3.

    val x3: T3 = x2.asInstanceOf[T3] // Run-time type cast kills type checking. Still, this will not help us do anything useful with `x3`.

    "x3 match { case Case2() ⇒ 0.0 }" shouldNot typeCheck
  }

  it should "implement Semigroup without any data in PTTF" in {
    // PTTF without any data, defined on Int and String.
    sealed trait Semigroup[T]
    implicit case object SemigroupIntEvidence extends Semigroup[Int]
    implicit case object SemigroupStringEvidence extends Semigroup[String]

    // PTVF defined by explicit type casting.
    // This code is unsafe and error-prone! Shown here only for comparison with better implementations.
    def op[T](x: T, y: T)(implicit ev: Semigroup[T]): T = ev match {
      case SemigroupIntEvidence ⇒ (x.asInstanceOf[Int] + y.asInstanceOf[Int]).asInstanceOf[T]
      case SemigroupStringEvidence ⇒ (x.asInstanceOf[String] + y.asInstanceOf[String]).asInstanceOf[T]
    }

    op(1, 2) shouldEqual 3

    op("a", "b") shouldEqual "ab"

    "add(Some(1), Some(2))" shouldNot compile
  }

  it should "implement Semigroup by putting unnamed evidence data into PTTF type" in {
    // PTTF carrying a function.
    type Semigroup[T] = (T, T) ⇒ T

    // Define the type domain.
    implicit val semigroupIntEvidence: Semigroup[Int] = _ + _
    implicit val semigroupStringEvidence: Semigroup[String] = _ + _

    // PTVF
    def sum[T](ts: Seq[T], default: T)(implicit ev: Semigroup[T]): T = ts.foldLeft(default)(ev)

    sum(Seq(1, 2, 3), 0) shouldEqual 6

    sum(Seq("a", "b", "c", "d"), "") shouldEqual "abcd"

  }

  it should "implement Semigroup with data in PTTF trait" in {
    // PTTF as a trait with an abstract `op` method.
    sealed trait Semigroup[T] {
      def op(x: T, y: T): T
    }
    implicit case object SemigroupIntEvidence extends Semigroup[Int] {
      override def op(x: Int, y: Int): Int = x + y
    }
    implicit case object SemigroupStringEvidence extends Semigroup[String] {
      override def op(x: String, y: String): String = x + y
    }

    // PTVF defined externally, by using the `op` method.
    def add3[T](x: T, y: T, z: T)(implicit ev: Semigroup[T]): T = ev.op(x, ev.op(y, z))

    add3(1, 2, 3) shouldEqual 6

    add3("a", "b", "c") shouldEqual "abc"

    "add3(Some(1), Some(2), Some(3))" shouldNot compile
  }

  behavior of "monoid type class"

  it should "implement Monoid without using traits and without method names" in {
    // PTTF
    type Monoid[T] = (T, (T, T) ⇒ T)

    // Define the type domain.
    implicit val monoidIntEvidence: Monoid[Int] = (0, _ + _)
    implicit val monoidStringEvidence: Monoid[String] = ("", _ + _)

    // PTVF
    def sum[T](ts: Seq[T])(implicit ev: Monoid[T]): T = ts.foldLeft(ev._1)(ev._2)

    sum(Seq(1, 2, 3)) shouldEqual 6

    sum(Seq("a", "b", "c", "d")) shouldEqual "abcd"
  }

  it should "implement Monoid using a case class as PTTF" in {
    // PTTF
    final case class Monoid[T](empty: T, combine: (T, T) ⇒ T)

    // Define the type domain.
    implicit val monoidIntEvidence: Monoid[Int] = Monoid(0, _ + _)
    implicit val monoidStringEvidence: Monoid[String] = Monoid("", _ + _)

    // PTVF defined externally.
    def sum[T](ts: Seq[T])(implicit ev: Monoid[T]): T = ts.foldLeft(ev.empty)(ev.combine)

    sum(Seq(1, 2, 3)) shouldEqual 6

    sum(Seq("a", "b", "c", "d")) shouldEqual "abcd"
  }

  behavior of "combining type classes"

  // PTTFs
  final case class Pointed[T](point: T)

  implicit val pointedIntEvidence: Pointed[Int] = Pointed(0)
  implicit val pointedStringEvidence: Pointed[String] = Pointed("")

  final case class Semigroup[T](op: (T, T) ⇒ T)

  implicit val semigroupIntEvidence: Semigroup[Int] = Semigroup(_ + _)
  implicit val semigroupStringEvidence: Semigroup[String] = Semigroup(_ + _)

  final case class Monoid[T](empty: T, combine: (T, T) ⇒ T)

  // Automatically derive class instances for Monoid for pointed semigroups.
  implicit def monoidInstance[T](implicit semigroupEv: Semigroup[T], pointedEv: Pointed[T]): Monoid[T] =
    Monoid(pointedEv.point, semigroupEv.op)

  it should "define Monoid using Pointed and Semigroup" in {
    // Define `sum` for monoids as before.
    // PTVF defined externally.
    def sum[T](ts: Seq[T])(implicit ev: Monoid[T]): T = ts.foldLeft(ev.empty)(ev.combine)

    sum(Seq(1, 2, 3)) shouldEqual 6

    sum(Seq("a", "b", "c", "d")) shouldEqual "abcd"
  }

  it should "define `sum` using cats.Monoid" in {
    implicit def catsMonoidInstance[T](implicit semigroupEv: Semigroup[T], pointedEv: Pointed[T]): cats.Monoid[T] = new cats.Monoid[T] {
      override def empty: T = pointedEv.point

      override def combine(x: T, y: T): T = semigroupEv.op(x, y)
    }

    // Define `sum` for monoids as before.
    // PTVF
    def sum[T](ts: Seq[T])(implicit ev: cats.Monoid[T]): T = ts.foldLeft(ev.empty)(ev.combine)

    sum(Seq(1, 2, 3)) shouldEqual 6

    sum(Seq("a", "b", "c", "d")) shouldEqual "abcd"

    // check laws.
    checkCatsMonoidLaws[Int]()
    checkCatsMonoidLaws[String]()
    "checkCatsMonoidLaws[Double]()" shouldNot compile // We did not define a Monoid instance for Double.
  }

  behavior of "law checking"

  def checkSemigroupLaw[T: Arbitrary](implicit semigroupEv: Semigroup[T]): Assertion = forAll { (x: T, y: T, z: T) ⇒
    val op = semigroupEv.op

    op(x, op(y, z)) shouldEqual op(op(x, y), z)
  }

  it should "check semigroup law for Int and String" in {
    checkSemigroupLaw[Int]
    checkSemigroupLaw[String]
  }

  def checkMonoidLaw[T: Arbitrary](implicit monoidEv: Monoid[T]): Assertion = forAll { (x: T) ⇒
    monoidEv.combine(x, monoidEv.empty) shouldEqual x
    monoidEv.combine(monoidEv.empty, x) shouldEqual x
  }

  it should "check monoid law for Int and String" in {
    checkMonoidLaw[Int]
    checkMonoidLaw[String]
  }

  it should "detect non-associative operation" in {
    // Boolean implication (if x then y) is not associative
    implicit val badSemigroupEvidence: Semigroup[Boolean] = Semigroup((x, y) ⇒ if (x) y else true)

    //    checkSemigroupLaw[Boolean] // fails and prints a counterexample (x = false, y = true, z = false)
    // So, let's run the test on this counterexample.

    val op = implicitly[Semigroup[Boolean]].op

    op(false, op(true, false)) should not be op(op(false, true), false)
  }

}
