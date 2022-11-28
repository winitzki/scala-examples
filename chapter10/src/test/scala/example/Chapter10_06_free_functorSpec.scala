package example

import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec

class Chapter10_06_free_functorSpec extends FlatSpec with Matchers {

  trait Runner[F[_]] {
    def run[A]: F[A] ⇒ A
  }

  it should "run an FF program" in {
    sealed trait FF[F[_], A] {
      def map[B](f: A => B): FF[F, B]
    }
    final case class FMap[F[_], X, Y](f: X => Y, p: F[X]) extends FF[F, Y] {
      def map[Z](g: Y => Z): FF[F, Z] = FMap[F, X, Z](f andThen g, p)
    }

    def runFF[F[_], A](runner: Runner[F]): FF[F, A] => A = {
      case FMap(f, p) => f(runner.run(p))
    }
  }

  it should "run an FFS program" in {
    final case class FuncSeq[X, Y](first: X ⇒ Any, funcs: Vector[Any => Any]) {
      def append[Z](g: Y => Z): FuncSeq[X, Z] = FuncSeq(first, funcs :+ g.asInstanceOf[Any => Any])
    }

    @tailrec def runSeq[X, Y](x: X, p: FuncSeq[X, Y]): Y = p.funcs.headOption match {
      case None => p.first(x).asInstanceOf[Y]
      case Some(second) => runSeq(p.first(x), FuncSeq(second, p.funcs.tail))
    }


    sealed trait FFS[F[_], A] {
      def map[B](f: A => B): FFS[F, B]
    }

    final case class FMap[F[_], X, Y](f: FuncSeq[X, Y], p: F[X]) extends FFS[F, Y] {
      def map[Z](g: Y => Z): FFS[F, Z] = FMap[F, X, Z](f append g, p)
    }

    def runFF[F[_], A](runner: Runner[F]): FFS[F, A] => A = {
      case FMap(f, p) => runSeq(runner.run(p), f)
    }

    // Verify that a large number of composed .map calls on the free functor do not cause stack overflows.
  }

}
