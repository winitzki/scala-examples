package example

import org.scalatest.{FlatSpec, Matchers}

class TraversalForNestedRecursiveType  extends FlatSpec with Matchers{

  sealed trait SqSize[N, A]
  final case class Matrix[N, A](byIndex: ((N, N)) => A) extends SqSize[N, A]
  final case class Next[N, A](next: SqSize[Option[N], A])  extends SqSize[N, A]
  type Sq[A] = SqSize[Unit, A]

  val matrix2x2: Sq[Int] = Next(Matrix {
    case (None, None)         => 11
    case (None, Some(_))      => 12
    case (Some(_), None)      => 21
    case (Some(_), Some(_))   => 22
  })

}
