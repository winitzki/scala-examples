package example

import cats.Functor
import example.Monoid10._
import org.scalatest.{FlatSpec, Matchers}

object Monoid10 {

  final case class Monoid[T](combine: (T, T) ⇒ T, empty: T)

  object Monoid {
    def apply[T: Monoid]: Monoid[T] = implicitly[Monoid[T]]
  }

  implicit class Syntax[T: Monoid](x: T) {
    def |+|(y: T): T = implicitly[Monoid[T]].combine(x, y)
  }

  def foldMap[M: Monoid, A](f: A => M): Seq[A] => M =
    _.foldLeft(Monoid[M].empty) { (m, a) => m |+| f(a) }
}

object Data10 {

  sealed trait FMR[T]

  final case class Combine[T](left: FMR[T], right: FMR[T]) extends FMR[T]

  final case class Empty[T]() extends FMR[T]

  final case class Wrap[T](value: T) extends FMR[T]

  implicit def monoidFMR[T]: Monoid[FMR[T]] = Monoid((l, r) => Combine(l, r), Empty())

  val exampleFMR: FMR[Int] = Combine(Empty(), Combine(Combine(Wrap(456), Empty()), Wrap(123)))

  def runnerFMR[M: Monoid, T](runT: T => M)(fmr: FMR[T]): M = fmr match {
    case Combine(left, right) => runnerFMR(runT)(left) |+| runnerFMR(runT)(right)
    case Empty() => Monoid[M].empty
    case Wrap(value) => runT(value)
  }

  sealed trait Tree2[A]

  final case class Leaf[A](a: A) extends Tree2[A]

  final case class Branch[A](left: Tree2[A], right: Tree2[A]) extends Tree2[A]


  type F1[T] = Tree2[Option[T]]

  def wrapF1[T](t: T): F1[T] = Leaf(Some(t))

  implicit def monoidF1[T]: Monoid[F1[T]] = Monoid((l, r) => Branch(l, r), Leaf(None))

  def runnerF1[M: Monoid, T](runT: T => M)(fmr: F1[T]): M = fmr match {
    case Branch(left, right) => runnerF1(runT)(left) |+| runnerF1(runT)(right)
    case Leaf(None) => Monoid[M].empty
    case Leaf(Some(value)) => runT(value)
  }

  implicit val functorF1: Functor[F1] = cats.derive.functor[F1]


  type NEL[T] = (T, List[T])

  object NEL {
    def concat[T]: (NEL[T], NEL[T]) ⇒ NEL[T] = {
      case ((head1, tail1), (head2, tail2)) => (head1, tail1 ++ List(head2) ++ tail2)
    }

    def pure[T](t: T): NEL[T] = (t, Nil)

    implicit class ToList[T](nel: NEL[T]) {
      def toList: List[T] = nel._1 +: nel._2
    }

  }

  import NEL.ToList

  type F2[T] = NEL[Option[T]]

  def wrapF2[T](t: T): F2[T] = (Some(t), Nil)

  implicit def monoidF2[T]: Monoid[F2[T]] = Monoid(NEL.concat, (None, Nil))

  def runnerF2[M: Monoid, T](runT: T => M)(fmr: F2[T]): M = foldMap(runT).apply(fmr.toList.flatten)

  type F3[T] = Option[Tree2[T]]

  def wrapF3[T](t: T): F3[T] = Some(Leaf(t))

  def concatF3[T]: (F3[T], F3[T]) => F3[T] = {
    case (None, x) => x
    case (x, None) => x
    case (Some(a), Some(b)) => Some(Branch(a, b))
  }

  implicit def monoidF3[T]: Monoid[F3[T]] = Monoid(concatF3, None)

  def runnerTree2[M: Monoid, T](runT: T => M)(tree2: Tree2[T]): M = tree2 match {
    case Leaf(a) => runT(a)
    case Branch(left, right) => runnerTree2(runT)(left) |+| runnerTree2(runT)(right)
  }

  def runnerF3[M: Monoid, T](runT: T => M)(fmr: F3[T]): M = fmr match {
    case Some(t) => runnerTree2(runT)(t)
    case None => Monoid[M].empty
  }

  type F4[T] = List[T]

  def wrapF4[T](t: T): F4[T] = List(t)

  implicit def monoidF4[T]: Monoid[F4[T]] = Monoid(_ ++ _, Nil)

  def runnerF4[M: Monoid, T](runT: T => M)(fmr: F4[T]): M = foldMap(runT).apply(fmr)

}

class Chapter10_free_monoids extends FlatSpec with Matchers {

}
