package example

import cats.{Functor, Monad}
import org.scalatest.{FlatSpec, Matchers, ParallelTestExecution}
import org.scalacheck.Arbitrary
import org.scalacheck.ScalacheckShapeless._
import CatsMonad.toCatsMonad

object DataTypes {
  sealed trait BTree[A]

  final case class Leaf[A](a: A) extends BTree[A]

  final case class Branch[A](left: BTree[A], right: BTree[A]) extends BTree[A]

  type OBTree[A] = Option[BTree[A]]

  implicit val obtreeFunctor: Functor[OBTree] = cats.derive.functor[OBTree]

  sealed trait Tree3[A]

  final case class Empty[A]() extends Tree3[A]

  final case class B3[A](top: A, left: Tree3[A], right: Tree3[A]) extends Tree3[A]

  implicit val tree3Functor: Functor[Tree3] = cats.derive.functor[Tree3]

  type R3[A] = Option[Either[A, (A, A)]]

  implicit val r3Functor: Functor[R3] = cats.derive.functor[R3]
}

class NonStandardTreeMonadsSpec extends FlatSpec with Matchers with CheckSemimonadLaws with ParallelTestExecution {

  implicit def toSemimonad[F[_] : Monad]: Semimonad[F] = {
    implicit val functorF: Functor[F] = Monad[F]
    new Semimonad[F] {
      override def flatMap[A, B](fa: F[A])(f: A ⇒ F[B]): F[B] = Monad[F].flatMap(fa)(f)
    }
  }

  type V0 = Boolean
  type V1 = Int
  type V2 = String
  type V3 = Float

  def testLaws[D[_] : CatsMonad](title: String)(implicit
                                                a: Arbitrary[D[V0]], b: Arbitrary[V0], c: Arbitrary[V0 ⇒ D[V0]],
                                                a1: Arbitrary[D[V1]], b1: Arbitrary[V1], c1: Arbitrary[V1 ⇒ D[V2]],
                                                a2: Arbitrary[D[V2]], b2: Arbitrary[V2], c2: Arbitrary[V2 ⇒ D[V3]],
  ) = {
    val repetitions1 = 40
    val repetitions2 = 20
    // First, test the laws with all types set to Boolean. This is fast and will catch some law violations.
    (1 to repetitions1).foreach { i ⇒
      println(s"$title: Iteration $i with type V0")
      checkMonadIdentityLaws[D, V0, V0]
      checkSemimonadLaws[D, V0, V0, V0]
    }
    (1 to repetitions2).foreach { i ⇒
      println(s"$title: Iteration $i with types V1, V2, V3")
      checkMonadIdentityLaws[D, V1, V2]
      checkSemimonadLaws[D, V1, V2, V3]
    }
    println(s"$title: all monad laws passed after ${repetitions1 + repetitions2} iterations")
  }

  //  behavior of "monad laws"

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
      testLaws[D]("1 + A x A conservative")
    }
  }

  it should "hold for the standard List monad" in {
    import cats.instances.list.catsStdInstancesForList
    implicit val catsMonadList: CatsMonad[List] = new CatsMonad[List] {
      override def flatMap[A, B](fa: List[A])(f: A ⇒ List[B]): List[B] = fa.flatMap(f)

      override def pure[A](x: A): List[A] = List(x)
    }
    testLaws[List]("standard List")
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
    testLaws[List]("greedy List")
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
    testLaws[List]("truncating List")
  }

  it should "hold for the monad 1 + BTree(A) in the greedy implementation" in {
    import DataTypes._
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

    testLaws[OBTree]("greedy OBTree")
  }

  it should "hold for the monad 1 + BTree(A) in the non-greedy implementation" in {
    import DataTypes._
    implicit val catsMonadOBTree: CatsMonad[OBTree] = new CatsMonad[OBTree] {
      override def flatMap[A, B](fa: OBTree[A])(f: A ⇒ OBTree[B]): OBTree[B] = fa match {
        case Some(Leaf(x)) ⇒ f(x)
        case Some(Branch(left, right)) ⇒ (flatMap(Some(left))(f), flatMap(Some(right))(f)) match {
          case (None, None) ⇒ None
          case (None, Some(a)) ⇒ Some(a)
          case (Some(a), None) ⇒ Some(a)
          case (Some(a), Some(b)) ⇒ Some(Branch(a, b))
        }
        case None ⇒ None
      }

      override def pure[A](x: A): OBTree[A] = Some(Leaf(x))
    }
    testLaws[OBTree]("non-greedy OBTree")
  }

  it should "fail for the monad Tree = 1 + A x Tree x Tree in some implementation" in {
    import DataTypes._
    implicit val catsMonadOBTree: CatsMonad[Tree3] = new CatsMonad[Tree3] {
      override def flatMap[A, B](fa: Tree3[A])(f: A ⇒ Tree3[B]): Tree3[B] = fa match {
        case Empty() ⇒ Empty()
        case B3(top, left, right) ⇒ (f(top), flatMap(left)(f), flatMap(right)(f)) match {
          case (result, Empty(), Empty()) ⇒ result
          case (Empty(), _, _) ⇒ Empty()
          case (B3(t, x, y), l, r) ⇒ B3(t, l, r)
        }
      }

      override def pure[A](x: A): Tree3[A] = B3(x, Empty(), Empty())
    }
    testLaws[Tree3]("Tree3")
  }

  it should "fail for the monad 1 + A + A x A with the first implementation" in {
    import DataTypes._
    implicit val catsMonadR3: CatsMonad[R3] = new CatsMonad[R3] {
      override def flatMap[A, B](fa: R3[A])(f: A ⇒ R3[B]): R3[B] = fa match {
        case Some(Left(a)) ⇒ f(a)
        case Some(Right((a, b))) ⇒ (f(a), f(b)) match {
          case (None, None) ⇒ None
          case (None, Some(x)) ⇒ Some(x)
          case (Some(x), None) ⇒ Some(x)
          case (Some(Left(x)), Some(Left(y))) ⇒ Some(Right((x, y)))
          case (Some(Left(x)), Some(Right((_, y)))) ⇒ Some(Right((x, y)))
          case (Some(Right((x, _))), Some(Left(y))) ⇒ Some(Right((x, y)))
          case (Some(Right((x, _))), Some(Right((_, y)))) ⇒ Some(Right((x, y)))
        }
        case None ⇒ None
      }

      override def pure[A](x: A): R3[A] = Some(Left(x))
    }
    testLaws[R3]("1 + A + A x A")
  }

  it should "fail for the monad 1 + A + A x A with the second implementation" in {
    import DataTypes._
    implicit val catsMonadR3: CatsMonad[R3] = new CatsMonad[R3] {
      override def flatMap[A, B](fa: R3[A])(f: A ⇒ R3[B]): R3[B] = fa match {
        case Some(Left(a)) ⇒ f(a)
        case Some(Right((a, b))) ⇒ (f(a), f(b)) match {
          case (None, None) ⇒ None
          case (None, Some(x)) ⇒ Some(x)
          case (Some(x), None) ⇒ Some(x)
          case (Some(Left(x)), Some(Left(y))) ⇒ Some(Right((x, y)))
          case (Some(Left(x)), Some(Right((_, y)))) ⇒ Some(Right((x, y)))
          case (Some(Right((x, _))), Some(Left(y))) ⇒ Some(Right((x, y)))
          case (Some(Right((x, _))), Some(Right((_, y)))) ⇒ Some(Right((x, y)))
        }
        case None ⇒ None
      }

      override def pure[A](x: A): R3[A] = Some(Right(x, x))
    }
    testLaws[R3]("1 + A + A x A")
  }
}
