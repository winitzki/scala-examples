package example

import cats.Functor
import cats.syntax.functor._
import org.scalatest.{FlatSpec, Matchers}
import io.chymyst.ch.implement

trait Zippable[F[_]] {
  def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)]
}

object Zippable {

  def apply[F[_]: Zippable]: Zippable[F] = implicitly[Zippable[F]]

  implicit class ZipOp[F[_] : Zippable, A](fa: F[A]) {
    def zip[B](fb: F[B]): F[(A, B)] = implicitly[Zippable[F]].zip(fa, fb)

    def zipLeft[B](fb: F[B])(implicit fn: Functor[F]): F[A] = (fa zip fb).map(_._1)

    def zipRight[B](fb: F[B])(implicit fn: Functor[F]): F[B] = (fa zip fb).map(_._2)
  }
}

class TraversalLawViolation extends FlatSpec with Matchers {

  // A standard traversal trav: (A => F[B]) => L[A] => F[L[B]] should invoke each F-effect once.
  // Define a traversal trav2: (A => F[B]) => L[A] => F[L[B]] that invokes each F-effect twice.
  // Show that it violates the composition law of traversals.

  type L[A] = (A, A) // A very simple but nontrivial traversable functor.

  implicit val functorL: Functor[L] = new Functor[L] {
    override def map[A, B](fa: (A, A))(f: A ⇒ B): (B, B) = implement
  }

  import Zippable.ZipOp

  // Given a Zippable functor F, define traverse and traverse2 for L:

  def travL[A, B, F[_] : Zippable](f: A ⇒ F[B])(la: L[A]): F[L[B]] = la.map(f) match {
    case (l1, l2) ⇒ l1 zip l2
  }

  def trav2[A, B, F[_] : Zippable : Functor](f: A ⇒ F[B])(la: L[A]): F[L[B]] = la.map(f) match {
    case (f1, f2) => (f1 zip f1 zip f2).map { case ((_, y), z) ⇒ (y, z)}
  }

  // Define State[Int, *] as a Zippable Functor
  final case class S[A](run: Int ⇒ (A, Int))

  implicit val FunctorS: Functor[S] = new Functor[S] {
    override def map[A, B](fa: S[A])(f: A ⇒ B): S[B] = implement
    /*S(i ⇒ fa.run(i) match {
    case (x, j) ⇒ (f(x), j)
  })*/
  }
  implicit val ZippableS: Zippable[S] = new Zippable[S] {
    override def zip[A, B](fa: S[A], fb: S[B]): S[(A, B)] = S { i ⇒
      val (a, j) = fa.run(i)
      val (b, k) = fb.run(j)
      ((a, b), k)
    }
  }

  val su: S[Unit]  = S { i ⇒ ((), i) }

  su.run(123) shouldEqual (((), 120))

  // Define F[A] as the composition S[S[A]] and implement Zippable and Functor instances.
  final case class F[A](run: S[S[A]]) {
    def eval(i: Int, j: Int): (A, Int, Int) = {
      val (sa, k) = run.run(i)
      val (a, l) = sa.run(j)
      (a, k, l)
    }
  }

  implicit val FunctorF: Functor[F] = new Functor[F] {
    override def map[A, B](fa: F[A])(f: A ⇒ B): F[B] = implement
  }

  implicit val ZippableF: Zippable[F] = new Zippable[F] {
    override def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)] = F {
      (fa.run zip fb.run).map { case (sa, sb) ⇒ sa zip sb }
    }
  }

  val f1: Int ⇒ S[Int] = i ⇒ S(j ⇒ (i + j, i + j))
  val f2: Int ⇒ S[Int] = f1 //  i ⇒ S(j ⇒ (i+j, i))
  val l: L[Int] = (1, 0)
  val f1f2: Int ⇒ F[Int] = i ⇒ F(f1(i).map(f2))

  {
    val result1: F[L[Int]] = trav2[Int, Int, F](f1f2)(l)
    val result2: F[L[Int]] = {
      val x: S[(Int, Int)] = trav2[Int, Int, S](f1)(l)
      val y: S[S[(Int, Int)]] = x.map(trav2[Int, Int, S](f2))
      F(y)
    }
    result1.eval(0, 0) should not be result2.eval(0, 0)
    result1.eval(0, 0) shouldEqual (((3, 5), 2, 5))
    result2.eval(0, 0) shouldEqual (((4, 6), 2, 6))
//    trav2L[Int, Int, S](f1)(l).run(0) shouldEqual (((2, 2), 2))
//    trav2L[Int, Int, S](f2)((2, 2)).run(0) shouldEqual (((4, 8), 8))
  }

  { // The standard `traverse` satisfies the law.
    val result1: F[L[Int]] = travL[Int, Int, F](f1f2)(l)
    val result2: F[L[Int]] = {
      val x: S[(Int, Int)] = travL[Int, Int, S](f1)(l)
      val y: S[S[(Int, Int)]] = x.map(travL[Int, Int, S](f2))
      F(y)
    }
    result1.eval(50, 300) shouldEqual result2.eval(50, 300)
    result1.eval(0, 0) shouldEqual (((1, 2), 1, 2))
    result2.eval(0, 0) shouldEqual (((1, 2), 1, 2))
    travL[Int, Int, S](f1)(l).run(0) shouldEqual (((1, 1), 1))
    travL[Int, Int, S](f2)((1, 1)).run(0) shouldEqual (((1, 2), 2))
  }

}
