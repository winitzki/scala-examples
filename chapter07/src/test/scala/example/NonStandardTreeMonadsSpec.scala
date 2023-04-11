package example

import cats.{Functor, Monad}
import org.scalatest.{FlatSpec, Matchers}
import cats.syntax.functor._
import io.chymyst.ch._
import org.scalatest.FlatSpec
import org.scalacheck.ScalacheckShapeless._
import cats.derive
import CatsMonad.toCatsMonad

object Trees {
  sealed trait BTree[A]

  final case class Leaf[A](a: A) extends BTree[A]

  final case class Branch[A](left: BTree[A], right: BTree[A]) extends BTree[A]

  type OBTree[A] = Option[BTree[A]]

  implicit val obtreeFunctor: Functor[OBTree] = cats.derive.functor[OBTree]

  sealed trait Tree3[A]

  final case class Empty[A]() extends Tree3[A]

  final case class B3[A](top: A, left: Tree3[A], right: Tree3[A]) extends Tree3[A]

  implicit val tree3Functor: Functor[Tree3] = cats.derive.functor[Tree3]
}

class NonStandardTreeMonadsSpec extends FlatSpec with Matchers with CheckSemimonadLaws {

  implicit def toSemimonad[F[_] : Monad]: Semimonad[F] = {
    implicit val functorF: Functor[F] = Monad[F]
    new Semimonad[F] {
      override def flatMap[A, B](fa: F[A])(f: A ⇒ F[B]): F[B] = Monad[F].flatMap(fa)(f)
    }
  }

  behavior of "monad laws"

  it should "fail for 1 + A x A" in {
    type D[A] = Option[(A, A)]
    val catsMonadD1: CatsMonad[D] = new CatsMonad[D] {
      override def flatMap[A, B](fa: D[A])(f: A ⇒ D[B]): D[B] = fa match {
        case Some((x, y)) ⇒ (f(x), f(y)) match {
          case (None, None) ⇒ None
          case (None, r) ⇒ r
          case (r, None) ⇒ r
          case (Some((a, _)), Some((_, d))) ⇒ Some((a, d))
        }
        case None ⇒ None
      }

      override def pure[A](x: A): D[A] = Some((x, x))
    }

    {
      implicit val i = catsMonadD1
      checkSemimonadLaws[D, Int, Int, Int]
      checkMonadIdentityLaws[D, Int, Int]
    }
  }

  it should "hold for the standard List monad" in {
    import cats.instances.list.catsStdInstancesForList
    checkSemimonadLaws[List, Int, Int, Int]
    checkMonadIdentityLaws[List, Int, Int]
  }

  it should "hold for the non-standard (greedy) List monad" in {
    implicit val functorList: Functor[List] = new Functor[List] {
      override def map[A, B](fa: List[A])(f: A ⇒ B): List[B] = fa map f
    }
    implicit val catsMonadList: CatsMonad[List] = new CatsMonad[List] {
      override def flatMap[A, B](fa: List[A])(f: A ⇒ List[B]): List[B] = {
        val result: List[List[B]] = fa map f
        if (result forall (_.nonEmpty)) result.flatten else List()
      }

      override def pure[A](x: A): List[A] = List(x)
    }
    checkSemimonadLaws[List, Int, Int, Int]
    checkMonadIdentityLaws[List, Int, Int]
  }

  it should "fail for the truncating List monad" in {
    implicit val functorList: Functor[List] = new Functor[List] {
      override def map[A, B](fa: List[A])(f: A ⇒ B): List[B] = fa map f
    }
    implicit val catsMonadList: CatsMonad[List] = new CatsMonad[List] {
      override def flatMap[A, B](fa: List[A])(f: A ⇒ List[B]): List[B] = {
        val result: List[List[B]] = fa map f
        result.takeWhile(_.nonEmpty).flatten
      }

      override def pure[A](x: A): List[A] = List(x)
    }
    checkSemimonadLaws[List, Int, Int, Int]
    checkMonadIdentityLaws[List, Int, Int]
  }

  it should "hold for the monad 1 + BTree(A) in the greedy implementation" in {
    import Trees._
    implicit val catsMonadOBTree: CatsMonad[OBTree] = new CatsMonad[OBTree] {
      override def flatMap[A, B](fa: OBTree[A])(f: A ⇒ OBTree[B]): OBTree[B] = fa match {
        case Some(Leaf(x)) ⇒ f(x)
        case Some(Branch(left, right)) ⇒ (flatMap(Some(left))(f), flatMap(Some(right))(f)) match {
          case (None, _) ⇒ None
          case (_, None) ⇒ None
          case (Some(a), Some(b)) ⇒ Some(Branch(a, b))
        }
        case None ⇒ None
      }

      override def pure[A](x: A): OBTree[A] = Some(Leaf(x))
    }

    checkSemimonadLaws[OBTree, Int, Int, Int]
    checkMonadIdentityLaws[OBTree, Int, Int]
  }

  it should "hold for the monad 1 + BTree(A) in the non-greedy implementation" in {
    import Trees._
    implicit val catsMonadOBTree: CatsMonad[OBTree] = new CatsMonad[OBTree] {
      override def flatMap[A, B](fa: OBTree[A])(f: A ⇒ OBTree[B]): OBTree[B] = fa match {
        case Some(Leaf(x)) ⇒ f(x)
        case Some(Branch(left, right)) ⇒ (flatMap(Some(left))(f), flatMap(Some(right))(f)) match {
          case (None, None) ⇒ None
          case (None, a) ⇒ a
          case (a, None) ⇒ a
          case (Some(a), Some(b)) ⇒ Some(Branch(a, b))
        }
        case None ⇒ None
      }

      override def pure[A](x: A): OBTree[A] = Some(Leaf(x))
    }

    checkSemimonadLaws[OBTree, Int, Int, Int]
    checkMonadIdentityLaws[OBTree, Int, Int]
  }

  it should "hold for the monad Tree = 1 + A x Tree x Tree in some implementations" in {
    import Trees._
    implicit val catsMonadOBTree: CatsMonad[Tree3] = new CatsMonad[Tree3] {
      override def flatMap[A, B](fa: Tree3[A])(f: A ⇒ Tree3[B]): Tree3[B] = fa match {
        case Empty() ⇒ Empty()
        case B3(top, left, right) ⇒ (f(top), flatMap(left)(f), flatMap(right)(f)) match {
          case (result, Empty(), Empty()) ⇒ result
          case (B3(t, x, y), l, r) ⇒ B3(t, l, r)
        }
      }

      override def pure[A](x: A): Tree3[A] = B3(x, Empty(), Empty())
    }

    checkSemimonadLaws[Tree3, Int, Int, Int]
    checkMonadIdentityLaws[Tree3, Int, Int]
  }
}
