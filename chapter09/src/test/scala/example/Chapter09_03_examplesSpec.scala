package example

import org.scalatest.{FlatSpec, Matchers}
import cats.{Applicative, Bifunctor, Bitraverse, Eval, Functor, Monoid, Traverse}
import org.scalatest.{FlatSpec, Matchers}
import cats.syntax.functor._
import cats.instances._
import cats.syntax.bifunctor._
import cats.syntax.traverse._
import cats.syntax.bitraverse._
import WuZip.WuZipSyntax
import Trav._
import io.chymyst.ch._

class Chapter09_03_examplesSpec extends FlatSpec with Matchers {

  behavior of "usage examples"

  it should "convert a traversable functor to List" in {
    def toList[L[_] : Trav : Functor, C](fa: L[C]): List[C] = {
      // Define List[A] as a monoid type and a constant functor.
      type Z[B] = List[C]
      implicit val functorZ: Functor[Z] = new Functor[Z] {
        override def map[A, B](fa: Z[A])(f: A => B): Z[B] = fa
      }
      implicit val wuZipZ: WuZip[Z] = new WuZip[Z]() {
        override def wu: Z[Unit] = Nil

        override def zip[A, B](fa: Z[A], fb: Z[B]): Z[(A, B)] = fb ++ fa // Note opposite order!
      }
      fa.trav[Z, C](c ⇒ List(c))
    }

    // Check that this works.
    type L[A] = (A, A, A)
    implicit val functorL: Functor[L] = new Functor[L] {
      override def map[A, B](fa: L[A])(f: A ⇒ B): L[B] =
        (f(fa._1), f(fa._2), f(fa._3))
    }
    implicit val travL: Trav[L] = new Trav[L] {
      override def seq[F[_] : WuZip : Functor, A](lfa: L[F[A]]): F[L[A]] =
        lfa._1 zip lfa._2 zip lfa._3 map { case ((x, y), z) ⇒ (x, y, z) }
    }

    val l1: L[Int] = (1, 2, 3)
    toList(l1) shouldEqual List(3, 2, 1)
  }

  it should "fold a tree to aggregate data" in {
    sealed trait Tree[A]
    final case class Leaf[A](x: A) extends Tree[A]
    final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

    implicit val functorTree: Functor[Tree] = new Functor[Tree] {
      override def map[A, B](fa: Tree[A])(f: A ⇒ B): Tree[B] = fa match {
        case Leaf(x) ⇒ Leaf(f(x))
        case Branch(left, right) ⇒ Branch(map(left)(f), map(right)(f))
      }
    }

    implicit val travTree: Trav[Tree] = new Trav[Tree] {
      override def seq[F[_] : WuZip : Functor, A](t: Tree[F[A]]): F[Tree[A]] = t match {
        case Leaf(fa) ⇒ fa.map(Leaf.apply)
        case Branch(left, right) ⇒
          seq[F, A](left) zip seq[F, A](right) map { case (x, y) ⇒ Branch(x, y) }
      }
    }

    // Product of squares of all integers in the tree.

    val t1: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))

    implicit val monoidInt: Monoid[Int] = new Monoid[Int] {
      override def empty: Int = 1

      override def combine(x: Int, y: Int): Int = x * y
    }

    t1.foldMap(x ⇒ x * x) shouldEqual 36
  }
}
