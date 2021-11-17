package example

import cats.Functor
import cats.syntax.functor._
import example.Zippable.ZipOp
import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec

class TraversalForNestedRecursiveType extends FlatSpec with Matchers {

  {
    sealed trait SqSize[N, A]
    final case class Matrix[N, A](byIndex: ((N, N)) => A) extends SqSize[N, A]
    final case class Next[N, A](next: SqSize[Option[N], A]) extends SqSize[N, A]
    type Sq[A] = SqSize[Unit, A]

    val matrix2x2: Sq[Int] = Next(Matrix {
      case (None, None) => 11
      case (None, Some(_)) => 12
      case (Some(_), None) => 21
      case (Some(_), Some(_)) => 22
    })

  }

  type AllValues[N] = List[N] // A list of all possible values of type N.

  object AllValues {
    def apply[N: AllValues]: AllValues[N] = implicitly[AllValues[N]]
  }

  {
    sealed abstract class SqSize[N: AllValues, A]
    final case class Matrix[N: AllValues, A](byIndex: ((N, N)) => A) extends SqSize[N, A]
    final case class Next[N: AllValues, A](next: SqSize[Option[N], A]) extends SqSize[N, A]
    type Sq[A] = SqSize[Unit, A]

    implicit val allValuesUnit: AllValues[Unit] = List(())

    implicit def allValues[N: AllValues]: AllValues[Option[N]] = None +: AllValues[N].map(Some(_))

    // Access the matrix element at zero-based index (i, j).
    def access[N: AllValues, A](s: SqSize[N, A], i: Int, j: Int): A = s match {
      case Matrix(byIndex) ⇒ byIndex((AllValues[N].apply(i), AllValues[N].apply(j)))
      case Next(next) ⇒ access[Option[N], A](next, i, j)
    }

    // Compute the dimension of the matrix.
    @tailrec
    def dimension[N: AllValues, A](s: SqSize[N, A]): Int = s match {
      case Matrix(_) ⇒ AllValues[N].length
      case Next(next) ⇒ dimension[Option[N], A](next)
    }

    // Convert Sq[A] to a Seq[Seq[A]].
    def toSeq[N: AllValues, A](s: SqSize[N, A]): Seq[Seq[A]] = {
      val length = dimension(s)
      (0 until length).map(i ⇒ (0 until length).map(j ⇒ access(s, i, j)))
    }

    {
      def sequence[A, N, F[_] : Zippable : Functor]: SqSize[N, F[A]] => F[SqSize[N, A]] = {
        case Matrix(byIndex) => ??? // Base case.
        case Next(next) => ??? // Inductive step.
      }
    }

    val matrix2x2: Sq[Int] = Next(Matrix {
      case (None, None) => 11
      case (None, Some(_)) => 12
      case (Some(_), None) => 21
      case (Some(_), Some(_)) => 22
    })

    access(matrix2x2, 0, 0) shouldEqual 11
    access(matrix2x2, 0, 1) shouldEqual 12
    access(matrix2x2, 1, 0) shouldEqual 21
    access(matrix2x2, 1, 1) shouldEqual 22

    dimension(matrix2x2) shouldEqual 2

    toSeq(matrix2x2) shouldEqual List(
      List(11, 12),
      List(21, 22),
    )

    def sequenceNonEmptyList[F[_] : Zippable : Functor, A](l: List[F[A]]): F[List[A]] = l match {
      case head :: Nil => head.map(List(_))
      case head :: tail => head.zip(sequenceNonEmptyList(tail)).map { case (x, y) ⇒ x +: y }
    }

    def sequence[N: AllValues, F[_] : Zippable : Functor, A](sq: SqSize[N, F[A]]): F[SqSize[N, A]] = sq match {
      case Matrix(byIndex) =>
        val allValuesFa: List[F[((N, N), A)]] = for {
          i ← AllValues[N]
          j ← AllValues[N]
          fa = byIndex((i, j))
        } yield fa.map(a => ((i, j), a))

        val fList: F[List[((N, N), A)]] = sequenceNonEmptyList(allValuesFa)
        fList.map { values ⇒
          val valuesMap: ((N, N)) ⇒ A = values.toMap.apply
          Matrix[N, A](valuesMap)
        }

      case Next(next) =>
        implicit val finiteOptionN: AllValues[Option[N]] = AllValues(AllValues[N].map(Some(_)) :+ None)
        sequence[Option[N], F, A](next).map(Next(_))
    }

    // Test: use List as a Zippable Functor.
    import cats.instances.list.catsStdInstancesForList

    implicit val zippableList: Zippable[List] = new Zippable[List] {
      override def zip[A, B](fa: List[A], fb: List[B]): List[(A, B)] = fa zip fb
    }

    val matrix2x2List: Sq[List[Int]] = Next(Matrix {
      case (None, None) => List(0, 10, 100)
      case (None, Some(_)) => List(1, 11, 101)
      case (Some(_), None) => List(2, 12, 102)
      case (Some(_), Some(_)) => List(3, 13, 103)
    })

    val list2x2Matrix: List[Sq[Int]] = sequence(matrix2x2List)

    list2x2Matrix.length shouldEqual 3

    list2x2Matrix.map(x ⇒ toSeq(x)) shouldEqual Seq(
      Seq(
        Seq(0, 1),
        Seq(2, 3),
      ),
      Seq(
        Seq(10, 11),
        Seq(12, 13),
      ),
      Seq(
        Seq(100, 101),
        Seq(102, 103),
      )
    )

  }

}

class TraversalWithNestedRecursiveTypes2 extends FlatSpec with Matchers {

  final case class Finite[A, ListA](toList: ListA ⇒ List[A])

  object Finite {
    def apply[L, A](implicit f: Finite[L, A]): Finite[L, A] = f
  }

  implicit def finiteL[A]: Finite[A, A] = Finite(List(_))

  implicit def finiteLL[A, L](implicit f: Finite[A, L]): Finite[A, (A, L)] =
    Finite[A, (A, L)] { p: (A, L) ⇒
      p match {
        case (head, tail) ⇒ head +: f.toList(tail)
      }
    }

  type Id[A] = A

  sealed abstract class SqSize[L[_], A](implicit f: Finite[A, L[A]])

  final case class Matrix[L[_], A](data: L[L[A]])(implicit f: Finite[A, L[A]]) extends SqSize[L, A]

  final case class Next[L[_], A](next: SqSize[λ[A ⇒ (A, L[A])], A])(implicit f: Finite[A, L[A]]) extends SqSize[L, A]

  type Sq[A] = SqSize[Id, A]

  def toListList[L[_], A](implicit f: Finite[A, L[A]], g: Finite[L[A], L[L[A]]]): SqSize[L, A] ⇒ List[List[A]] = {
    case Matrix(data) ⇒ g.toList(data).map(f.toList)
    case Next(next: SqSize[λ[A ⇒ (A, L[A])], A]) ⇒
      val newF: Finite[A, (A, L[A])] = finiteLL(f)
      val h: Finite[(A, L[A]), L[(A, L[A])]] = ???
      val newG: Finite[(A, L[A]), ((A, L[A]), L[(A, L[A])])] = finiteLL(h)
      toListList[λ[A ⇒ (A, L[A])], A](newF, newG).apply(next)
  }

  val matrix2x2: Sq[Int] = Next[Id, Int](Matrix[λ[A ⇒ (A, A)], Int](
    (
      (1, 2),
      (3, 4),
    )
  ))

  val matrix3x3: Sq[Int] = Next[Id, Int](Next[λ[A ⇒ (A, A)], Int](Matrix[λ[A ⇒ (A, (A, A))], Int](
    (
      (1, (2, 3)),
      ((4, (5, 6)),
        (7, (8, 9))),
    )
  )))


}

class TraversalWithNestedRecursiveTypes3 extends FlatSpec with Matchers {

  trait Finite[L[_]] {
    def toList[A]: L[A] ⇒ List[A]
  }

  object Finite {
    def apply[L[_] : Finite]: Finite[L] = implicitly[Finite[L]]
  }

  type Id[A] = A

//  implicit def finiteId: Finite[Id] = new Finite[Id] {
//    override def toList[A]: Id[A] ⇒ List[A] = List(_)
//  }

  implicit def finiteA: Finite[λ[A ⇒ A]] = new Finite[λ[A ⇒ A]] {
    override def toList[A]: A ⇒ List[A] = List(_)
  }


  implicit def finiteNext[L[_]: Finite]: Finite[λ[A ⇒ (A, L[A])]] = new Finite[λ[A ⇒ (A, L[A])]] {
    override def toList[A]: ((A, L[A])) ⇒ List[A] = {
      case (head, tail) ⇒ head +: Finite[L].toList(tail)
    }
  }

  sealed abstract class SqSize[L[_] : Finite, A]

  final case class Matrix[L[_] : Finite, A](data: L[L[A]]) extends SqSize[L, A]

  final case class Next[L[_] : Finite, A](next: SqSize[λ[A ⇒ (A, L[A])], A])(implicit f: Finite[λ[A ⇒ (A, L[A])]]) extends SqSize[L, A]

  type Sq[A] = SqSize[Id, A]

  val matrix2x2: Sq[Int] = Next[Id, Int](Matrix[λ[A ⇒ (A, A)], Int](
    (
      (1, 2),
      (3, 4),
    )
  ))

  val matrix3x3: Sq[Int] = Next[Id, Int](Next[λ[A ⇒ (A, A)], Int](Matrix[λ[A ⇒ (A, (A, A))], Int](
    (
      (1, (2, 3)),
      ((4, (5, 6)),
        (7, (8, 9))),
    )
  )))
}
