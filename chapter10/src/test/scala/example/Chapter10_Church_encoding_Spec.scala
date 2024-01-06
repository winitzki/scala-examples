package example

import org.scalatest.{FlatSpec, Matchers}

class Chapter10_Church_encoding_Spec extends FlatSpec with Matchers {

  type F[A, R] = Option[(A, R)]

  trait Lst[A] {
    def run[R](alg: F[A, R] => R): R

    override def toString: String = "[" + run[String] {
      case None => ""
      case Some((a, prev)) => a.toString + (if (prev.nonEmpty) ", " + prev else prev)
    } + "]"

    def fold[R](init: R)(update: (A, R) => R): R = run[R] {
      case None => init
      case Some((a, prev)) => update(a, prev)
    }

    def unfix: F[A, Lst[A]] = fold[F[A, Lst[A]]](None) { (a, falsta) => Some((a, Lst.fix(falsta))) }
  }

  object Lst {
    def nil[A]: Lst[A] = new Lst[A] {
      override def run[R](alg: F[A, R] => R): R = alg(None)
    }

    def cons[A](a: A, r: Lst[A]): Lst[A] = new Lst[A] {
      override def run[R](alg: F[A, R] => R): R = alg(Some((a, r.run(alg))))
    }

    def fix[A]: F[A, Lst[A]] => Lst[A] = {
      case None => Lst.nil
      case Some((a, lsta)) => Lst.cons(a, lsta)
    }
  }

  val list123: Lst[Int] = Lst.cons(1, Lst.cons(2, Lst.cons(3, Lst.nil)))

  it should "use Church-encoded list" in {
    list123.toString shouldEqual "[1, 2, 3]"
    Lst.nil.toString shouldEqual "[]"

    list123.unfix.toString shouldEqual "Some((1,[2, 3]))"
    Lst.nil[Int].unfix shouldEqual None

    Lst.fix(list123.unfix).toString shouldEqual list123.toString


  }

}