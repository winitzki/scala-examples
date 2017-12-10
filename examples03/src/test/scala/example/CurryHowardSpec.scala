package example

import com.sun.xml.internal.xsom.impl.Ref.Term
import example.CHTypes._
import example.CurryHoward._
import org.scalatest.{FlatSpec, Matchers}

class CurryHowardSpec extends FlatSpec with Matchers {

  behavior of "syntax for untyped functions"

  it should "define syntax for untyped lambda calculus with products" in {
    sealed trait Term {
      def apply[T](x: T): Term = this // dummy implementation to simplify code
    }

    final case class \[T](v: Term ⇒ T) extends Term

    final case class \:[T](v: Any ⇒ T) extends Term

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
    def f1[X, Y]: X ⇒ Y ⇒ X = inhabit

    // This does not work because `inhabit` needs to access the type of the enclosing owner!
    // The compiler error is "recursive method f2 needs type".
    " def f2a[X, Y] = inhabit[X ⇒ Y ⇒ X] " shouldNot compile

    def f2[X, Y] = ofType[X ⇒ Y ⇒ X]

    // does not work because ofType[Nothing] is instantiated: the macro does not use the enclosing owner.
    " def f3[X, Y]: X ⇒ Y ⇒ X = ofType " shouldNot compile
  }

  it should "get the list of propositions" in {
    TermExpr.propositions(LamE(PropE("A", "A"), AppE(PropE("A", "A"), PropE("B", "B")))) shouldEqual Set(PropE("A", "A"), PropE("B", "B"))
  }

  it should "generate correct code for the identity function" in {
    def f1[X] = ofType[X ⇒ X]

    f1(123) shouldEqual 123
    f1("abc") shouldEqual "abc"
    f1(true) shouldEqual true
  }

  it should "generate correct code for the const function" in {
    def f2[X, Y] = ofType[X ⇒ Y ⇒ X]

    val cTrue = f2(true)
    cTrue(123) shouldEqual true
    cTrue("abc") shouldEqual true
    cTrue(true) shouldEqual true

    val c3 = f2(3)
    c3(123) shouldEqual 3
    c3("abc") shouldEqual 3
    c3(true) shouldEqual 3

  }

  it should "generate correct code for the identity function with standard syntax" in {
    def f1[X]: X ⇒ X = inhabit

    f1(123) shouldEqual 123
    f1("abc") shouldEqual "abc"
    f1(true) shouldEqual true
  }

  it should "generate correct code for the const function with standard syntax" in {
    def f2[X, Y]: X ⇒ Y ⇒ X = inhabit

    val cTrue = f2(true)
    cTrue(123) shouldEqual true
    cTrue("abc") shouldEqual true
    cTrue(true) shouldEqual true

    val c3 = f2(3)
    c3(123) shouldEqual 3
    c3("abc") shouldEqual 3
    c3(true) shouldEqual 3

  }

  behavior of "type parameter introspection"

  it should "get printable representation of enclosing owner's type" in {
    def result[A, B, C]: (String, String) = testType[Int]

    result._2 shouldEqual "(<basic>String, <basic>String)"
  }

  it should "get printable representation of basic types" in {
    def result[A, B, C]: (String, String) = testType[Int]

    result._1 shouldEqual "<basic>Int"
  }

  it should "get printable representation of parametric type" in {
    def result[A, B, C]: (String, String) = testType[A]

    result._1 shouldEqual "<tparam>A"
  }

  it should "get printable representation of function types" in {
    def result[A, B, C]: (String, String) = testType[A ⇒ B]

    result._1 shouldEqual "(<tparam>A) ..=>.. <tparam>B"
  }

  it should "get printable representation of fixed types with type constructors" in {
    def result[A, B, C]: (String, String) = testType[Option[Seq[Int]] ⇒ Option[List[Set[A]]] ⇒ B]

    result._1 shouldEqual "(1 + <constructor>Seq[Int]) ..=>.. (1 + <constructor>List[Set[A]]) ..=>.. <tparam>B"
  }

  it should "get printable representation of fixed types with type constructors with [_]" in {
    def result[A, B, C]: (String, String) = testType[Option[_] ⇒ B]

    result._1 shouldEqual "(1 + _) ..=>.. <tparam>B"
  }

  it should "get printable representation of Option types" in {
    def result[A, B, C]: (String, String) = testType[Option[A] ⇒ Either[A, B]]

    result._1 shouldEqual "(1 + <tparam>A) ..=>.. <tparam>A + <tparam>B"
  }

  it should "get printable representation of Any, Unit, and Nothing types" in {
    def result[A, B, C]: (String, String) = testType[Any ⇒ Nothing ⇒ Unit]

    result._1 shouldEqual "(_) ..=>.. (0) ..=>.. 1"
  }

  it should "not confuse a type parameter with a type inheriting from Any" in {
    class Q

    def result[A, B, C]: (String, String) = testType[A ⇒ Q]

    result._1 shouldEqual "(<tparam>A) ..=>.. <other>Q"
  }

  it should "get printable representation of tuple types" in {
    def result[A, B, C]: (String, String) = testType[(Any, Nothing, Unit, A, B, C)]

    result._1 shouldEqual "(_, 0, 1, <tparam>A, <tparam>B, <tparam>C)"
  }

  it should "get printable representation of tuple as function argument" in {
    def result[A, B, C]: (String, String) = testType[((A, B)) ⇒ C]

    result._1 shouldEqual "((<tparam>A, <tparam>B)) ..=>.. <tparam>C"
  }

  it should "get printable representation of tuple of basic types" in {
    def result[A, B, C]: (String, String) = testType[(Int, String, Boolean, Float, Double, Long, Symbol, Char)]

    result._1 shouldEqual "(" + CurryHoward.basicTypes.map("<basic>" + _).mkString(", ") + ")"
  }

  behavior of "proof search"

  it should "correctly explode sequences of integers" in {
    explode[Int](Seq(Seq(1, 2))) shouldEqual Seq(Seq(1), Seq(2))
    explode[Int](Seq(Seq(1, 2), Seq())) shouldEqual Seq()
    explode[Int](Seq(Seq())) shouldEqual Seq()
    explode[Int](Seq()) shouldEqual Seq(Seq())
    explode[Int](Seq(Seq(1, 2), Seq(10, 20, 30))) shouldEqual Seq(Seq(1, 10), Seq(1, 20), Seq(1, 30), Seq(2, 10), Seq(2, 20), Seq(2, 30))
  }

  it should "correctly produce proofs from axiom" in {
    followsFromAxioms(Sequent[Int](Seq(1, 2, 3), 0)) shouldEqual Seq()
    followsFromAxioms(Sequent[Int](Seq(1, 2, 3), 1)) shouldEqual Seq(LamE(PropE("x1", 1), LamE(PropE("x3", 2), LamE(PropE("x2", 3), PropE("x1", 1)))))
    followsFromAxioms(Sequent[Int](Seq(1, 2, 1), 1)) shouldEqual Seq(
      LamE(PropE("x4", 1), LamE(PropE("x6", 2), LamE(PropE("x5", 1), PropE("x4", 1)))),
      LamE(PropE("x9", 1), LamE(PropE("x8", 2), LamE(PropE("x7", 1), PropE("x7", 1))))
    )
  }

  it should "correctly compute LJT subformulas" in {
    subformulas[Int](TP(1) :-> TP(1)) shouldEqual Seq(TP(1) :-> TP(1), TP(1))
    subformulas[Int](TP(1) :-> TP(2)) shouldEqual Seq(TP(1) :-> TP(2), TP(1), TP(2))
    subformulas[Int](TP(1) :-> (TP(2) :-> TP(3))).toSet shouldEqual Set(TP(1) :-> (TP(2) :-> TP(3)), TP(2) :-> TP(3), TP(1), TP(2), TP(3))

    // Example from the paper.
    subformulas[String](((TP("A") :-> TP("A")) :-> TP("C")) :-> TP("C")).toSet shouldEqual Set(
      ((TP("A") :-> TP("A")) :-> TP("C")) :-> TP("C"),
      (TP("A") :-> TP("A")) :-> TP("C"),
      TP("A") :-> TP("A"),
      TP("A") :-> TP("C"),
      TP("C") :-> TP("C"),
      TP("A"), TP("C")
    )

    // Disjunctions.
    subformulas[Int](DisjunctT(Seq(TP(1), TP(2))) :-> TP(3)).toSet shouldEqual Set(
      DisjunctT(Seq(TP(1), TP(2))) :-> TP(3),
      DisjunctT(Seq(TP(1), TP(2))),
      TP(1) :-> TP(3),
      TP(2) :-> TP(3),
      TP(1), TP(2), TP(3)
    )

    // Conjunctions.
    subformulas[Int](ConjunctT(Seq(TP(1), TP(2), TP(3))) :-> TP(4)).toSet shouldEqual Set(
      ConjunctT(Seq(TP(1), TP(2), TP(3))) :-> TP(4),
      ConjunctT(Seq(TP(1), TP(2), TP(3))),
      TP(1) :-> (TP(2) :-> (TP(3) :-> TP(4))),
      TP(2) :-> (TP(3) :-> TP(4)),
      TP(3) :-> TP(4),
      TP(1), TP(2), TP(3), TP(4)
    )
  }

}
