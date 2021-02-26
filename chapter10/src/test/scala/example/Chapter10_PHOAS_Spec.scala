package example

import org.scalatest.{FlatSpec, Matchers}

class Chapter10_PHOAS_Spec extends FlatSpec with Matchers {

  // Implementing the PHOAS described in the paper:
  // Bruno C. de S. Oliveira. Functional programming with structured graphs.
  // https://www.cs.utexas.edu/~wcook/Drafts/2012/graphs.pdf

  behavior of "simple PHOAS for untyped lambda calculus with integers"

  trait PHOAS1Term {
    def term[V]: PHOAS1AST[V]
  }

  object PHOAS1Term {
    def eval(term: PHOAS1Term): PHOAS1Vals = PHOAS1AST.eval(term.term[PHOAS1Vals])
  }

  sealed trait PHOAS1AST[V]

  object PHOAS1AST {

    final case class Var[V](value: V) extends PHOAS1AST[V]

    final case class IntVal[V](value: Int) extends PHOAS1AST[V]

    final case class BoolVal[V](value: Boolean) extends PHOAS1AST[V]

    final case class FuncVal[V](run: V ⇒ PHOAS1AST[V]) extends PHOAS1AST[V]

    final case class ApplyFunc[V](func: PHOAS1AST[V], arg: PHOAS1AST[V]) extends PHOAS1AST[V]

    final case class IntFunc[V](intFunc: Int ⇒ Int, arg: PHOAS1AST[V]) extends PHOAS1AST[V]

    def eval: PHOAS1AST[PHOAS1Vals] ⇒ PHOAS1Vals = {
      case PHOAS1AST.Var(value) ⇒ value
      case PHOAS1AST.IntVal(value) ⇒ PHOAS1Vals.IntVal(value)
      case PHOAS1AST.BoolVal(value) ⇒ PHOAS1Vals.BoolVal(value)
      case PHOAS1AST.FuncVal(run) ⇒ PHOAS1Vals.FuncVal(run andThen PHOAS1AST.eval)
      case PHOAS1AST.ApplyFunc(func, arg) ⇒ eval(func) match {
        case PHOAS1Vals.FuncVal(func) ⇒ func(eval(arg)) // Only this case should ever occur.
      }
      case PHOAS1AST.IntFunc(intFunc, arg) ⇒ PHOAS1Vals.IntVal(PHOAS1AST.eval(arg) match {
        case PHOAS1Vals.IntVal(value) ⇒ intFunc(value) // Only this case should ever occur.
      })
    }
  }

  sealed trait PHOAS1Vals

  object PHOAS1Vals {

    final case class IntVal(value: Int) extends PHOAS1Vals

    final case class BoolVal(value: Boolean) extends PHOAS1Vals

    final case class FuncVal(func: PHOAS1Vals ⇒ PHOAS1Vals) extends PHOAS1Vals

  }

  it should "calculate a simple expression" in {
    // (x -> 3 + x)(4) = 7
    // For simplicity, implement 3 + x as an external function.
    val term: PHOAS1Term = new PHOAS1Term {

      import PHOAS1AST._

      override def term[V]: PHOAS1AST[V] = ApplyFunc(FuncVal { x ⇒ IntFunc(z ⇒ 3 + z, Var(x)) }, IntVal(4))
    }

    PHOAS1Term.eval(term) shouldEqual PHOAS1Vals.IntVal(7)
  }

  
}
