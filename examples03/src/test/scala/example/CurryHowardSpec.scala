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

  it should "get printable representation of simple types" in {
    import CurryHoward._

    def result1[A, B, C]: (String, String) = testType[Int]

    result1 shouldEqual (("Int", "[A, B, C]=> (String, String)"))
  }

  it should "get printable representation of parametric type" in {
    import CurryHoward._

    def result1[A, B, C]: (String, String) = testType[A]

    result1 shouldEqual (("A", "[A, B, C]=> (String, String)"))
  }

  it should "get printable representation of function types" in {
    import CurryHoward._

    def result1[A, B, C]: (String, String) = testType[A ⇒ B]

    result1._1 shouldEqual "A ..=>.. B"
    result1._2 shouldEqual "[A, B, C]=> (String, String)"
  }

}
