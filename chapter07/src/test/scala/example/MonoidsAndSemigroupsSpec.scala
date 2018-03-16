package example

import cats.kernel.{Monoid, Semigroup}
import org.scalatest.{FlatSpec, Matchers}
import cats.syntax.semigroup._ // for |+|

class MonoidsAndSemigroupsSpec extends FlatSpec with Matchers {

  behavior of "examples"

  def makeExampleString(implicit monoidString: Monoid[String]) = "Hi " |+| "there" |+| Monoid[String].empty

  it should "import monoid syntax " in {
    import cats.instances.string._ // for Monoid
    val stringResult = makeExampleString

    stringResult shouldEqual "Hi there"
  }

  it should "define non-standard instance for String" in {
    implicit val monoidString: Monoid[String] = new Monoid[String] {
      val separator: String = "|"
      val emptyValue: String = "<nothing>"

      override def empty: String = emptyValue

      override def combine(x: String, y: String): String = (x, y) match {
        case (`emptyValue`, z) => z
        case (z, `emptyValue`) => z
        case _ => s"$x$separator$y"
      }
    }

    val result = makeExampleString
    result shouldEqual "Hi |there"
  }

  def monoidOption[A](implicit ev: Semigroup[A]): Monoid[Option[A]] = new Monoid[Option[A]] {
    override def empty: Option[A] = None

    override def combine(x: Option[A], y: Option[A]): Option[A] = (x, y) match {
      case (None, z) ⇒ z
      case (z, None) ⇒ z
      case (Some(u), Some(v)) ⇒ Some(u |+| v)
    }
  }

  it should "construct monoid instance for Option[T]" in {

    implicit def monoidFunction[A]: Monoid[A ⇒ A] = new Monoid[A => A] {
      override def empty: A => A = identity

      override def combine(x: A => A, y: A => A): A => A = t => y(x(t)) // x andThen y
    }
    // Why is this associative?

    val x: Int => Int = x => x + 10
    val y: Int => Int = x => x * 2
    val z: Int => Int = x => 1 - 3 * x

    val f = x |+| y |+| z
    /*
    What does f() do?
    f = t => y(x(t)) |+| z = t => z(y(x(t)))
    f = x |+| t => z(y(t)) = t = >z(y(x(t)))
     */
  }

  it should "define monoid instance for another type" in {
    // Define a semigroup for (A, A, A)
    type P[A] = (A, A, A)

    implicit def semigroupP[A]: Semigroup[P[A]] = {
      (x: (A, A, A), y: (A, A, A)) => (x._1, y._2, y._3)
    }

    implicit def semigroupAny[A]: Semigroup[A] = new Semigroup[A] {
      override def combine(x: A, y: A): A = x
    }

    implicit def semigroupPair[A]: Semigroup[(A, A)] = new Semigroup[(A, A)] {
      override def combine(x: (A, A), y: (A, A)): (A, A) = (x._1, y._2)
    }

  }

}
