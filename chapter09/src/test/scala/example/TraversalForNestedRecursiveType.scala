package example

import cats.Functor
import cats.syntax.functor._
import example.Zippable.ZipOp
import org.scalatest.{FlatSpec, Matchers}

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

    implicit def allValues[N: AllValues]: AllValues[Option[N]] = AllValues[N].map(Some(_)) :+ None

    // Access the matrix element at index (i, j).

    def accessN[N: AllValues, A](s: SqSize[N, A], i: N, j: N): A = s match {
      case Matrix(byIndex) ⇒ byIndex((i, j))
      case Next(next) ⇒ accessN[Option[N], A](next, i ,j)
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

    def sequenceNonEmptyList[F[_] : Zippable : Functor, A](l: List[F[A]]): F[List[A]] = l match {
      case head :: Nil => head.map(List(_))
      case head :: tail => head.zip(sequenceNonEmptyList(tail)).map { case (x, y) ⇒ x :: y }
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

  }

}
