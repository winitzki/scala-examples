package example

import org.scalacheck.Arbitrary
import org.scalatest.{Assertion, FlatSpec}

class Chapter05_01_examplesSpec extends FlatSpec with CatsLawChecking {

  behavior of "simple type classes"

  it should "implement Summable without any data in PTTF" in {
    // PTTF without any data, defined on Int and String.
    sealed trait Summable[T]
    implicit case object SummableIntEvidence extends Summable[Int]
    implicit case object SummableStringEvidence extends Summable[String]

    // PTVF defined by explicit type matching and type casting.
    // This code is unsafe and error-prone! Shown here only for comparison.
    def add[T](x: T, y: T)(implicit ev: Summable[T]): T = x match {
      case _: Int ⇒ (x.asInstanceOf[Int] + y.asInstanceOf[Int]).asInstanceOf[T]
      case _: String ⇒ (x.asInstanceOf[String] + y.asInstanceOf[String]).asInstanceOf[T]
    }

    add(1, 2) shouldEqual 3

    add("a", "b") shouldEqual "ab"

    "add(Some(1), Some(2))" shouldNot compile
  }

  it should "implement Summable by putting evidence data into PTTF type" in {
    // PTTF carrying a function.
    type Summable[T] = (T, T) ⇒ T

    // Define the type domain.
    implicit val summableIntEvidence: Summable[Int] = _ + _
    implicit val summableStringEvidence: Summable[String] = _ + _

    // PTVF
    def sum[T](ts: Seq[T], default: T)(implicit ev: Summable[T]): T = ts.foldLeft(default)(ev)

    sum(Seq(1, 2, 3), 0) shouldEqual 6

    sum(Seq("a", "b", "c", "d"), "") shouldEqual "abcd"

  }

  it should "implement Summable with data in PTTF trait" in {
    // PTTF as a trait with an abstract `plus` method
    sealed trait Summable[T] {
      def plus(x: T, y: T): T
    }
    implicit case object SummableIntEvidence extends Summable[Int] {
      def plus(x: Int, y: Int): Int = x + y
    }
    implicit case object SummableStringEvidence extends Summable[String] {
      def plus(x: String, y: String): String = x + y
    }

    // PTVF defined by using the `plus` method.
    def add3[T](x: T, y: T, z: T)(implicit ev: Summable[T]): T = ev.plus(x, ev.plus(y, z))

    add3(1, 2, 3) shouldEqual 6

    add3("a", "b", "c") shouldEqual "abc"

    "add3(Some(1), Some(2), Some(3))" shouldNot compile
  }

  behavior of "monoid type class"

  it should "implement Monoid without using traits" in {
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
    final case class Monoid[T](zero: T, add: (T, T) ⇒ T)

    // Define the type domain.
    implicit val monoidIntEvidence: Monoid[Int] = Monoid(0, _ + _)
    implicit val monoidStringEvidence: Monoid[String] = Monoid("", _ + _)

    // PTVF
    def sum[T](ts: Seq[T])(implicit ev: Monoid[T]): T = ts.foldLeft(ev.zero)(ev.add)

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

  final case class Monoid[T](zero: T, add: (T, T) ⇒ T)

  // Automatically derive class instances for Monoid for pointed semigroups.
  implicit def monoidInstance[T](implicit semigroupEv: Semigroup[T], pointedEv: Pointed[T]): Monoid[T] =
    Monoid(pointedEv.point, semigroupEv.op)

  it should "define Monoid using Pointed and Semigroup" in {
    // Define `sum` for monoids as before.
    // PTVF
    def sum[T](ts: Seq[T])(implicit ev: Monoid[T]): T = ts.foldLeft(ev.zero)(ev.add)

    sum(Seq(1, 2, 3)) shouldEqual 6

    sum(Seq("a", "b", "c", "d")) shouldEqual "abcd"
  }

  it should "define `sum` using cats.Monoid" in {
    implicit def catsMonoidInstance[T](implicit semigroupEv: Semigroup[T], pointedEv: Pointed[T]): cats.Monoid[T] = new cats.Monoid[T] {
      def empty: T = pointedEv.point

      def combine(x: T, y: T): T = semigroupEv.op(x, y)
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
    monoidEv.add(x, monoidEv.zero) shouldEqual x
    monoidEv.add(monoidEv.zero, x) shouldEqual x
  }

  it should "check monoid law for Int and String" in {
    checkMonoidLaw[Int]
    checkMonoidLaw[String]
  }

  it should "detect non-associative operation" in {
    // classical implication (if x then y) is not associative
    implicit val badSemigroupEvidence: Semigroup[Boolean] = Semigroup((x, y) ⇒ if (x) y else true)

    //    checkSemigroupLaw[Boolean] // fails and prints a counterexample (x = false, y = true, z = false)

    val op = implicitly[Semigroup[Boolean]].op

    op(false, op(true, false)) should not be op(op(false, true), false)
  }

}
