package example

import cats.kernel.{Monoid, Semigroup}
import org.scalatest.{FlatSpec, Matchers}
import cats.syntax.semigroup._ // for |+|

class MonoidsAndSemigroupsSpec extends FlatSpec with Matchers {

  behavior of "extra examples for the Monoids chapter of the Cats book"

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

  behavior of "nonstandard semimonads and monads"

  it should "check associativity for the nonstandard Pair semimonads" in {
    import io.chymyst.ch._
    type Pair[A] = (A, A)

    def fmap[A, B] = ofType[(A => B) => Pair[A] => Pair[B]]

    def flatten[A] = anyOfType[Pair[Pair[A]] ⇒ Pair[A]]()

    val terms = flatten

    terms.length shouldEqual 16

    // Select the implementations that satisfy rigorously the associativity law.
    // fmap ftn . ftn = ftn . ftn

    def tA[A] = freshVar[A]

    def tC[C] = freshVar[C]

    def tCC[C] = freshVar[Pair[C]]

    def associativeTerms[A] = flatten[A].filter { ftn ⇒
      val ftnA = ftn.lambdaTerm

      // ftnC: Pair[Pair[C]] ⇒ Pair[C]
      val ftnC = ftnA.substTypeVar(tA, tC)

      val ftnCftnC = (ftnA.substTypeVar(tA, tCC) andThen ftnC).simplify

      // ftnLC: Pair[Pair[Pair[C]]] ⇒ Pair[Pair[C]]
      val ftnLC = (fmap.lambdaTerm :@ ftnC).simplify

      val ftnLCftn = (ftnLC andThen ftnC).simplify
      ftnLCftn.prettyRename equiv ftnCftnC.prettyRename
    }

    println("Semimonads:")
    associativeTerms[Int].map(_.lambdaTerm.prettyPrint).foreach(println)
    associativeTerms[Int].length shouldEqual 7 // One standard and six non-standard semimonads.
    /*
a ⇒ a._2 // Choose second outer tuple.
a ⇒ a._1 // Choose first outer tuple.
a ⇒ Tuple2(a._1._1, a._1._2) // Same as a ⇒ a._1
a ⇒ Tuple2(a._2._1, a._2._2) // Same as a ⇒ a._2
a ⇒ Tuple2(a._1._1, a._2._2) // The standard monad.
a ⇒ Tuple2(a._1._2, a._2._2) // Choose second inner tuple.
a ⇒ Tuple2(a._1._1, a._2._1) // Choose first inner tuple.
a ⇒ Tuple2(a._1._1, a._1._1) // Choose first element of first inner tuple.
a ⇒ Tuple2(a._2._2, a._2._2) // Choose second element of second inner tuple.
     */

    // Of these, select the implementations that satisfy rigorously the two identity laws.
    // pure . ftn = id
    // fmap pure . ftn = id

    def pure[A] = ofType[A ⇒ Pair[A]]

    val pureA = pure[Int].lambdaTerm
    val pureC = pureA.substTypeVar(tA, tC) // pure: C ⇒ Pair[C]
    val pureCC = pureA.substTypeVar(tA, tCC) // pure: Pair[C] ⇒ Pair[Pair[C]]
    val pureLC = fmap.lambdaTerm :@ pureC // fmap pure: Pair[C] ⇒ Pair[Pair[C]]

    def idA[A] = ofType[A ⇒ A]

    val idCC = idA.lambdaTerm.substTypeVar(tA, tCC)

    def monadTerms[A] = associativeTerms[A].filter { ftn ⇒
      val ftnA = ftn.lambdaTerm

      val ftnC = ftnA.substTypeVar(tA, tC)
      val law1 = (pureCC andThen ftnC).simplify.prettyRename
      val law2 = (pureLC andThen ftnC).simplify.prettyRename
      println(s"Checking laws for ${ftnA.prettyPrint}: ${law1.prettyPrint}, ${law2.prettyPrint}")
      (law1 equiv idCC.prettyRename) && (law2 equiv idCC.prettyRename)
    }
    println("Monads:")
    monadTerms[Int].map(_.lambdaTerm.prettyPrint).foreach(println)
    monadTerms.length shouldEqual 1
  }


}
