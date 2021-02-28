package example

import org.scalatest.{FlatSpec, Matchers}

class Chapter10_PHOAS3_Spec extends FlatSpec with Matchers {

  // Implementing the PHOAS described in the paper:
  // Bruno C. de S. Oliveira. Functional programming with structured graphs.
  // https://www.cs.utexas.edu/~wcook/Drafts/2012/graphs.pdf

  behavior of "simple PHOAS for untyped lambda calculus with mu and mu2 binders"

  trait PHOAS3Term {
    def term[V]: PHOAS3AST[V]
  }

  object PHOAS3Term {
    def eval(term: PHOAS3Term): PHOASVals = PHOAS3AST.eval(term.term[PHOASVals])
  }

  sealed trait PHOAS3AST[V]

  object PHOAS3AST {

    final case class Var[V](value: V) extends PHOAS3AST[V]

    final case class IntVal[V](value: Int) extends PHOAS3AST[V]

    final case class BoolVal[V](value: Boolean) extends PHOAS3AST[V]

    final case class FuncVal[V](run: V ⇒ PHOAS3AST[V]) extends PHOAS3AST[V]

    final case class ApplyFunc[V](func: PHOAS3AST[V], arg: PHOAS3AST[V]) extends PHOAS3AST[V]

    final case class IntFunc2[V](intFunc: (Int, Int) ⇒ Int, arg1: PHOAS3AST[V], arg2: PHOAS3AST[V]) extends PHOAS3AST[V]

    final case class IntEquals[V](value1: PHOAS3AST[V], value2: PHOAS3AST[V]) extends PHOAS3AST[V]

    final case class If[V](cond: PHOAS3AST[V], thenThis: PHOAS3AST[V], elseThis: PHOAS3AST[V]) extends PHOAS3AST[V]

    final case class Mu1[V](run: Lazy[V] ⇒ PHOAS3AST[V]) extends PHOAS3AST[V]

    final case class Mu2[V](run: ::[Lazy[V]] ⇒ ::[PHOAS3AST[V]]) extends PHOAS3AST[V]

    def fmapEvalNEL[A, B](f: A ⇒ B): ::[Lazy[A]] ⇒ ::[B] = {
      case a :: as ⇒ ::(a.mapEval(f), as.map(_.mapEval(f)))
    }

    def fmapNEL[A, B](f: A ⇒ B): ::[A] ⇒ ::[B] = {
      case a :: as ⇒ ::(f(a), as.map(f))
    }

    def eval: PHOAS3AST[PHOASVals] ⇒ PHOASVals = {
      case PHOAS3AST.Var(value) ⇒ value
      case PHOAS3AST.IntVal(value) ⇒ PHOASVals.IntVal(value)
      case PHOAS3AST.BoolVal(value) ⇒ PHOASVals.BoolVal(value)
      case PHOAS3AST.FuncVal(run) ⇒ PHOASVals.FuncVal(run andThen PHOAS3AST.eval)
      case PHOAS3AST.ApplyFunc(func, arg) ⇒ eval(func) match {
        case PHOASVals.FuncVal(func) ⇒ func(eval(arg)) // Only this case should ever occur.
      }
      case PHOAS3AST.IntFunc2(intFunc, arg1, arg2) ⇒ PHOASVals.IntVal(
        (PHOAS3AST.eval(arg1), PHOAS3AST.eval(arg2)) match {
          case (PHOASVals.IntVal(value1), PHOASVals.IntVal(value2)) ⇒ intFunc(value1, value2) // Only this case should ever occur.
        })
      case PHOAS3AST.IntEquals(arg1, arg2) ⇒ PHOASVals.BoolVal(
        (PHOAS3AST.eval(arg1), PHOAS3AST.eval(arg2)) match {
          case (PHOASVals.IntVal(value1), PHOASVals.IntVal(value2)) ⇒ value1 == value2 // Only this case should ever occur.
        })
      case PHOAS3AST.If(cond, thenThis, elseThis) ⇒ PHOAS3AST.eval(cond) match {
        case PHOASVals.BoolVal(value) ⇒ if (value) PHOAS3AST.eval(thenThis) else PHOAS3AST.eval(elseThis)
      }
      case PHOAS3AST.Mu1(run) ⇒ Lazy.fix(run andThen PHOAS3AST.eval)
      case PHOAS3AST.Mu2(run) ⇒ Lazy.fixNEL1(run andThen fmapNEL(PHOAS3AST.eval)).head // Safe because the `::` lists are not empty.
    }
  }

  sealed trait PHOASVals

  object PHOASVals {

    final case class IntVal(value: Int) extends PHOASVals

    final case class BoolVal(value: Boolean) extends PHOASVals

    final case class FuncVal(func: PHOASVals ⇒ PHOASVals) extends PHOASVals

  }

  final class Lazy[+A](a: ⇒ A) {
    lazy val value: A = a

    def map[B](f: A ⇒ B): Lazy[B] = new Lazy(f(a)) // Do not memoize `value` unnecessarily, although we could use `value` here instead of `a`.
    def mapEval[B](f: A ⇒ B): B = f(value)
  }

  object Lazy {
    def of[A](a: ⇒ A): Lazy[A] = new Lazy(a) // Important: the argument type must be lazy.

    def fix[A](f: Lazy[A] ⇒ A): A = {
      f(of(fix(f)))
      // Alternative implementation:
      //      lazy val result: Lazy[A] = of(f(result))
      //      result.value
    }

    def ofList[A](as: ⇒ List[A], length: Int): List[Lazy[A]] = if (length == 0) List() else of(as.head) :: ofList(as.tail, length - 1)

    def ofNEL1[A](as: ⇒ ::[A]): ::[Lazy[A]] = ::(
      of(as.head),
      as.tail.map(x => of(x))
    )

    def ofNEL2[A](length: Int)(as: ⇒ ::[A]): ::[Lazy[A]] = ::(
      of(as.head),
      ofList(as.tail, length - 1)
    )

    def fixNEL1[A](f: ::[Lazy[A]] ⇒ ::[A]): ::[A] = {
      f(ofNEL1(fixNEL1(f)))
    }

    def fixNEL2[A](length: Int)(f: ::[Lazy[A]] ⇒ ::[A]): ::[A] = {
      f(ofNEL2(length)(fixNEL2(length)(f)))
    }
  }

  it should "compute factorial in Scala using Lazy.fix" in {
    val fact: Int ⇒ Int = Lazy.fix[Int ⇒ Int] { lazyF: Lazy[Int ⇒ Int] ⇒
      x: Int ⇒
        if (x <= 0) 1 else x * lazyF.value.apply(x - 1)
    }

    fact(10) shouldEqual 3628800
  }

  it should "fail to compute mutually recursive even/odd functions in Scala using Lazy.fixNEL1" in {
    the[StackOverflowError] thrownBy {
      val is10odd: Int ⇒ Boolean = Lazy.fixNEL1[Int ⇒ Boolean] { case _ :: List(odd, even) ⇒ ::[Int ⇒ Boolean](
        _ ⇒ odd.value.apply(10),
        List(
          n ⇒ if (n == 0) false else even.value.apply(n - 1),
          n ⇒ if (n == 0) true else odd.value.apply(n - 1),
        )
      )
      }.head
      is10odd(0) shouldEqual false
    } should have message null

  }

  it should "compute mutually recursive even/odd functions in Scala using Lazy.fixNEL2" in {
    lazy val is10odd: Int ⇒ Boolean = Lazy.fixNEL2[Int ⇒ Boolean](3) { case _ :: List(odd, even) ⇒ ::[Int ⇒ Boolean](
      _ ⇒ odd.value.apply(10),
      List(
        n ⇒ if (n == 0) false else even.value.apply(n - 1),
        n ⇒ if (n == 0) true else odd.value.apply(n - 1),
      )
    )
    }.head

//    the[StackOverflowError] thrownBy {
      is10odd(0) shouldEqual false
//    } should have message null

  }

  it should "compute mutually recursive even/odd functions using mu2 binder" in {
    /*
    let rec
     odd = λ n → if (n ≡ 0) then False else even (n − 1)
     even = λ n → if (n ≡ 0) then True else odd (n − 1)
    in odd 10
     */
    lazy val evenodd: PHOAS3Term = new PHOAS3Term {

      import PHOAS3AST._

      override def term[V]: PHOAS3AST[V] = Mu2 {
        case _ :: List(odd, even)
        ⇒ ::(
          ApplyFunc(Var(odd.value), IntVal(10)),
          List(
            FuncVal { n ⇒ If(IntEquals(Var(n), IntVal(0)), BoolVal(false), ApplyFunc(Var(even.value), IntFunc2(_ - _, Var(n), IntVal(1)))) },
            FuncVal { n ⇒ If(IntEquals(Var(n), IntVal(0)), BoolVal(true), ApplyFunc(Var(odd.value), IntFunc2(_ - _, Var(n), IntVal(1)))) },
          )
        )
      }
    }

    the[StackOverflowError] thrownBy {
      PHOAS3Term.eval(evenodd) shouldEqual PHOASVals.BoolVal(false)
    } should have message null
  }
  // TODO: implement Mu2 correctly with laziness

  // TODO: prohibit invalid term Mu1(Var) by better typing
  // sealed trait NonVar[V] extends PHOAS3AST[V]
  // final case class Mu1[V](run: V => NonVar[V]) extends PHOAS3AST[V]

  // TODO: use GADT to control result types

  // TODO: use implicit conversions to make the DSL easier to use

  // TODO: create a better user-facing DSL for cyclic structures

  // TODO: use HList instead of :: for better typing
}
