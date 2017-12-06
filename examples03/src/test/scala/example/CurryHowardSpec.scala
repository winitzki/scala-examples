package example

import org.scalatest.{FlatSpec, Matchers}

class CurryHowardSpec extends FlatSpec with Matchers {

  behavior of "syntax for untyped functions"

  it should "define syntax for untyped lambda calculus with products" in {

    "(x => x)" shouldNot compile

    val a1 = \(x ⇒ \(y ⇒ x))

    def f1[X, Y]: X => Y => X = x => y => x

    val a2 = \(x ⇒ \(y ⇒ x(y)))
    val a3 = \(x ⇒ \(y ⇒ y((x, x))))
    val a4 = \(x ⇒ \(y ⇒ \(z ⇒ x(y(z)))))
    val a5 = \(x ⇒ \(y ⇒ \(z ⇒ z(x(y)))))
    val a6 = \(x ⇒ (x, \(y ⇒ (x, y, x(y))))) // can use tuples
    val qqq = \ { x ⇒ x() } // can apply to unit
    val a7 = \: { case (x: Term, y: Term) ⇒ x(y(x)) } // use tuples as argument types, need annotations
  }

  behavior of "syntax of `inhabit`"

  it should "compile" in {
    import CurryHoward._

    def f1[X, Y]: X ⇒ Y ⇒ X = inhabit1

    f1[Int, Boolean] shouldEqual null

    //    def f2[X, Y] = inhabit1[X ⇒ Y ⇒ X] // this does not work if `inhabit` wants to access the type of the enclosing owner! ("recursive method f2 needs type)
    "def f2[X, Y] = inhabit1[X ⇒ Y ⇒ X]" shouldNot compile

    def f2[X, Y] = inhabit[X ⇒ Y ⇒ X]
  }

  behavior of "type parameter introspection"

  it should "get printable representation of enclosing owner's type" in {
    import CurryHoward._

    def result[A, B, C]: (String, String) = testType[Int]
    result._2 shouldEqual "<base classes: scala.Tuple2, scala.Serializable, java.io.Serializable, scala.Product2, scala.Product, scala.Equals, java.lang.Object, scala.Any>[A, B, C]=> (String, String)"
  }

  it should "get printable representation of simple types" in {
    import CurryHoward._

    def result[A, B, C]: (String, String) = testType[Int]

    result._1 shouldEqual "<base classes: scala.Int, scala.AnyVal, scala.Any>Int"
  }

  it should "get printable representation of parametric type" in {
    import CurryHoward._

    def result[A, B, C]: (String, String) = testType[A]

    result._1 shouldEqual "<tparam>A"
  }

  it should "get printable representation of function types" in {
    import CurryHoward._

    def result[A, B, C]: (String, String) = testType[A ⇒ B]

    result._1 shouldEqual "<tparam>A ..=>.. <tparam>B"
  }

  it should "get printable representation of fixed types with type constructors" in {
    import CurryHoward._

    def result[A, B, C]: (String, String) = testType[Option[Seq[Int]] ⇒ Option[List[Set[A]]] ⇒ B]

    result._1 shouldEqual "(1 + <constructor>Seq[Int]) ..=>.. (1 + <constructor>List[Set[A]]) ..=>.. <tparam>B"
  }

  it should "get printable representation of fixed types with type constructors with [_]" in {
    import CurryHoward._

    def result[A, B, C]: (String, String) = testType[Option[_] ⇒ B]

    result._1 shouldEqual "(1 + _) ..=>.. <tparam>B"
  }

  it should "get printable representation of Option types" in {
    import CurryHoward._

    def result[A, B, C]: (String, String) = testType[Option[A] ⇒ Either[A, B]]

    result._1 shouldEqual "(1 + <tparam>A) ..=>.. (<tparam>A + <tparam>B)"
  }

  it should "get printable representation of Any, Unit, and Nothing types" in {
    import CurryHoward._

    def result[A, B, C]: (String, String) = testType[Any ⇒ Nothing ⇒ Unit]

    result._1 shouldEqual "_ ..=>.. 0 ..=>.. 1"
  }
}
