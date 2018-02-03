package example

import java.time.LocalDateTime

import org.scalatest.{FlatSpec, Matchers}

class Chapter03_02_examplesSpec extends FlatSpec with Matchers {

  behavior of "disjunction types"

  it should "define functions consuming and returning Either" in {

    def safeDivide(x: Int, y: Int): Either[String, Int] = {
      if (y == 0)
        Left("Cannot divide by zero")
      else
        Right(x / y)
    }

    def logError(x: Either[String, Int]): Int = x match {
      case Left(error) ⇒
        println(s"Got error: $error")
        -1
      case Right(res) ⇒ res
    }

    val result1 = safeDivide(10, 5)
    result1 shouldEqual Right(2)
    logError(result1) shouldEqual 2

    val result2 = safeDivide(5, 0)
    result2 shouldEqual Left("Cannot divide by zero")
    logError(result2) shouldEqual -1
  }

  it should "ex01: define a disjunction type via case classes" in {
    sealed trait DayOfWeek
    final case object Sunday extends DayOfWeek
    final case object Monday extends DayOfWeek
    final case object Tuesday extends DayOfWeek
    final case object Wednesday extends DayOfWeek
    final case object Thursday extends DayOfWeek
    final case object Friday extends DayOfWeek
    final case object Saturday extends DayOfWeek

    val a: DayOfWeek = Monday
    val b: DayOfWeek = Saturday

    // DayOfWeek.toString is already defined.
    println(s"a is $a, b is $b")

    def isThisSaturday(d: DayOfWeek): Boolean = d match {
      case Saturday ⇒ true
      case _ ⇒ false
    }

    isThisSaturday(a) shouldEqual false
    isThisSaturday(b) shouldEqual true
  }

  it should "ex02: enhanced DayOfWeek" in {
    sealed trait DayOfWeek
    final case object Sunday extends DayOfWeek
    final case object Monday extends DayOfWeek
    final case object Tuesday extends DayOfWeek
    final case object Wednesday extends DayOfWeek
    final case object Thursday extends DayOfWeek
    final case class Friday(restaurantName: String, amountPaid: Double) extends DayOfWeek
    final case class Saturday(wakeUp: LocalDateTime) extends DayOfWeek

    val a: DayOfWeek = Monday
    val b: DayOfWeek = Friday("McDonald’s", 23.49)

    // DayOfWeek.toString is already defined.
    println(s"a is $a, b is $b")

    def howMuchPaid(d: DayOfWeek): Option[Double] = d match {
      case Friday(_, amountPaid) ⇒ Some(amountPaid)
      case _ ⇒ None
    }

    Seq(a, b).map(howMuchPaid) shouldEqual Seq(None, Some(23.49))
  }

  sealed trait RootsOfQuadratic

  final case object NoRealRoots extends RootsOfQuadratic

  final case class EqualRoots(x: Double) extends RootsOfQuadratic

  final case class TwoRoots(x: Double, y: Double) extends RootsOfQuadratic

  val solve2: ((Double, Double)) ⇒ RootsOfQuadratic = {
    case (b, c) ⇒
      // x^2 + b*x + c == 0
      val discriminant = b * b - 4.0 * c
      if (discriminant > 0) {
        val x1 = (-b - math.sqrt(discriminant)) / 2
        val x2 = (-b + math.sqrt(discriminant)) / 2
        TwoRoots(x1, x2)
      } else if (discriminant == 0)
        EqualRoots(-b / 2)
      else NoRealRoots
  }

  it should "ex03: solve quadratic equation" in {
    Seq(
      (-2.0, 1.0), // x^2 - 2*x + 1 = 0
      (1.0, 1.0), // x^2 + x + 1 = 0
      (-1.0, -2.0) // x^2 - x - 2 = 0
    ).map(solve2) shouldEqual Seq(EqualRoots(1.0), NoRealRoots, TwoRoots(-1.0, 2.0))
  }

  def rootAverage(roots: RootsOfQuadratic): Option[Double] = roots match {
    case NoRealRoots ⇒ None
    case EqualRoots(x) ⇒ Some(x)
    case TwoRoots(x, y) ⇒ Some((x + y) / 2)
  }

  it should "ex04: compute rootAverage" in {
    rootAverage(TwoRoots(-1.0, 1.0)) shouldEqual Some(0)
    rootAverage(EqualRoots(1.0)) shouldEqual Some(1.0)
    rootAverage(NoRealRoots) shouldEqual None
  }

  it should "ex05: solve 100 random equations" in {
    def random(): Double = scala.util.Random.nextDouble() * 2 - 1

    val coeffs: Seq[(Double, Double)] = Seq.fill(100)((random(), random()))
    val solutions: Seq[RootsOfQuadratic] = coeffs.map(solve2)

    val averages: Seq[Option[Double]] = solutions.map(rootAverage)

    val resultBadType: Seq[Option[Double]] = averages.filter {
      case Some(x) ⇒ true
      case None ⇒ false
    }
    // flatten: Seq[Seq[T]] ⇒ Seq[T]
    // flatten: Seq[Option[T]] ⇒ Seq[T]

    val result1: Seq[Double] = averages.flatten
    val result2: Seq[Double] = averages.collect { case Some(x) ⇒ x }

    println(s"Average root is ${result1.sum / result1.size}")
  }

  it should "ex06: implement fully generic function with option pairs" in {

    def check(f: ((Option[Int], Option[String])) ⇒ Option[(Int, String)]) = {
      f((Some(1), Some("a"))) shouldEqual Some((1, "a"))
      f((Some(1), None)) shouldEqual None
      f((None, Some("a"))) shouldEqual None
      f((None, None)) shouldEqual None
    }

    def f1[A, B]: ((Option[A], Option[B])) ⇒ Option[(A, B)] = {
      case (maybeA, maybeB) ⇒
        maybeA match {
          case Some(a) ⇒ maybeB match {
            case Some(b) ⇒ Some((a, b))
            case None ⇒ None
          }
          case None ⇒ None
        }
    }

    /*
    maybeA : Option[T]
    maybeA match {
      case Some(x) ⇒ Some(f(x))
      case None ⇒ None
    }

    is the same as:

    maybeA.map(f) // maybeA map f

     */

    def f2[A, B]: ((Option[A], Option[B])) ⇒ Option[(A, B)] = {
      case (maybeA, maybeB) ⇒
        val result = maybeA.map(x ⇒ maybeB.map(y ⇒ (x, y)))
        // Option[A] .map ( A ⇒ Option[(A, B)] ) : Option[Option[(A, B)]]
        result.flatten
    }

    /*
    collection.map(...).flatten is the same as collection.flatMap(...)
     */

    def f3[A, B]: ((Option[A], Option[B])) ⇒ Option[(A, B)] = {
      case (maybeA, maybeB) ⇒
        maybeA.flatMap(x ⇒ maybeB.map(y ⇒ (x, y)))
    }

    def f4[A, B]: ((Option[A], Option[B])) ⇒ Option[(A, B)] = {
      case (maybeA, maybeB) ⇒
        val result: Option[Option[(A, B)]] = maybeA.map {a ⇒
          val c: Option[(A, B)] = maybeB.map(b ⇒ (a, b))
          c
        }
        val r: Option[(A, B)] = result.flatten
        r
    }

    check(f1)
    check(f2)
    check(f3)
    check(f4)
  }

}
