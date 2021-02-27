package example

import org.scalatest.{FlatSpec, Matchers}

class Chapter10_PHOAS2_Spec extends FlatSpec with Matchers {

  // Implementing the PHOAS described in the paper:
  // Bruno C. de S. Oliveira. Functional programming with structured graphs.
  // https://www.cs.utexas.edu/~wcook/Drafts/2012/graphs.pdf

  behavior of "simple PHOAS for untyped lambda calculus with mu binder"

  trait PHOAS2Term {
    def term[V]: PHOAS2AST[V]
  }

  object PHOAS2Term {
    def eval(term: PHOAS2Term): PHOASVals = PHOAS2AST.eval(term.term[PHOASVals])
  }

  sealed trait PHOAS2AST[V]

  object PHOAS2AST {

    final case class Var[V](value: V) extends PHOAS2AST[V]

    final case class IntVal[V](value: Int) extends PHOAS2AST[V]

    final case class BoolVal[V](value: Boolean) extends PHOAS2AST[V]

    final case class FuncVal[V](run: V ⇒ PHOAS2AST[V]) extends PHOAS2AST[V]

    final case class ApplyFunc[V](func: PHOAS2AST[V], arg: PHOAS2AST[V]) extends PHOAS2AST[V]

    final case class IntFunc2[V](intFunc: (Int, Int) ⇒ Int, arg1: PHOAS2AST[V], arg2: PHOAS2AST[V]) extends PHOAS2AST[V]

    final case class IntEquals[V](value1: PHOAS2AST[V], value2: PHOAS2AST[V]) extends PHOAS2AST[V]

    final case class If[V](cond: PHOAS2AST[V], thenThis: PHOAS2AST[V], elseThis: PHOAS2AST[V]) extends PHOAS2AST[V]

    final case class Mu1[V](run: LazyFunction1[V, PHOAS2AST[V]]) extends PHOAS2AST[V]

    def eval: PHOAS2AST[PHOASVals] ⇒ PHOASVals = {
      case PHOAS2AST.Var(value) ⇒ value
      case PHOAS2AST.IntVal(value) ⇒ PHOASVals.IntVal(value)
      case PHOAS2AST.BoolVal(value) ⇒ PHOASVals.BoolVal(value)
      case PHOAS2AST.FuncVal(run) ⇒ PHOASVals.FuncVal(run andThen PHOAS2AST.eval)
      case PHOAS2AST.ApplyFunc(func, arg) ⇒ eval(func) match {
        case PHOASVals.FuncVal(func) ⇒ func(eval(arg)) // Only this case should ever occur.
      }
      case PHOAS2AST.IntFunc2(intFunc, arg1, arg2) ⇒ PHOASVals.IntVal(
        (PHOAS2AST.eval(arg1), PHOAS2AST.eval(arg2)) match {
          case (PHOASVals.IntVal(value1), PHOASVals.IntVal(value2)) ⇒ intFunc(value1, value2) // Only this case should ever occur.
        })
      case PHOAS2AST.IntEquals(arg1, arg2) ⇒ PHOASVals.BoolVal(
        (PHOAS2AST.eval(arg1), PHOAS2AST.eval(arg2)) match {
          case (PHOASVals.IntVal(value1), PHOASVals.IntVal(value2)) ⇒ value1 == value2 // Only this case should ever occur.
        })
      case PHOAS2AST.If(cond, thenThis, elseThis) ⇒ PHOAS2AST.eval(cond) match {
        case PHOASVals.BoolVal(value) ⇒ if (value) PHOAS2AST.eval(thenThis) else PHOAS2AST.eval(elseThis)
      }
      case PHOAS2AST.Mu1(run) ⇒ LazyFunction1.fix(run andThen PHOAS2AST.eval)
    }
  }

  sealed trait PHOASVals

  object PHOASVals {

    final case class IntVal(value: Int) extends PHOASVals

    final case class BoolVal(value: Boolean) extends PHOASVals

    final case class FuncVal(func: PHOASVals ⇒ PHOASVals) extends PHOASVals

  }

  trait LazyFunction1[-A, +B] {
    outer ⇒
    def apply(a: ⇒ A): B

    final def andThen[C](g: B ⇒ C): LazyFunction1[A, C] = new LazyFunction1[A, C] {
      override def apply(a: ⇒ A): C = g(outer.apply(a))
    }
  }

  object LazyFunction1 {
    def fix[A](f: LazyFunction1[A, A]): A = {
      lazy val result: A = f(result)
      result
    }
  }

  it should "compute factorial in Scala using LazyFunction1.fix" in {
    val fact: Int ⇒ Int = LazyFunction1.fix[Int ⇒ Int](new LazyFunction1[Int ⇒ Int, Int ⇒ Int] {
      override def apply(f: ⇒ (Int ⇒ Int)): Int ⇒ Int = { x ⇒
        if (x <= 0) 1 else x * f(x - 1)
      }
    })
    fact(10) shouldEqual 3628800
  }

  it should "compute factorial using mu binder" in {
    // μ f. λ n → if (n ≡ 0) then 1 else n * f(n − 1)
    val factTerm: PHOAS2Term = new PHOAS2Term {

      import PHOAS2AST._

      override def term[V]: PHOAS2AST[V] = Mu1 {
        new LazyFunction1[V, PHOAS2AST[V]] {
          override def apply(f: ⇒ V): PHOAS2AST[V] = FuncVal { n ⇒
            If(
              IntEquals(Var(n), IntVal(0)),
              IntVal(1),
              IntFunc2(_ * _, Var(n), ApplyFunc(Var(f), IntFunc2(_ + _, Var(n), IntVal(-1))))
            )
          }
        }
      }
    }

    // test1 = ↓ (App fact (Int 7))
    val test1: PHOAS2Term = new PHOAS2Term {

      import PHOAS2AST._

      override def term[V]: PHOAS2AST[V] = ApplyFunc(factTerm.term, IntVal(7))
    }

    PHOAS2Term.eval(test1) shouldEqual PHOASVals.IntVal(5040)
  }

  it should "obtain stack overflow by computing expressions containing a junk term" in {
    // An example of a "junk term" is Mu1 Var. This term is meaningless but type-checks.
    // Let us perform a computation with it. It should give a stack overflow exception.
    val junk: PHOAS2Term = new PHOAS2Term {

      import PHOAS2AST._

      override def term[V]: PHOAS2AST[V] = Mu1(x ⇒ Var(x))
    }
    the[StackOverflowError] thrownBy PHOAS2Term.eval(junk) should have message null
    val junkLazy: PHOAS2Term = new PHOAS2Term {

      import PHOAS2AST._

      override def term[V]: PHOAS2AST[V] = Mu1(new LazyFunction1[V, PHOAS2AST[V]] {
        override def apply(a: ⇒ V): PHOAS2AST[V] = Var(a)
      })
    }
    the[StackOverflowError] thrownBy PHOAS2Term.eval(junkLazy) should have message null
  }

}
