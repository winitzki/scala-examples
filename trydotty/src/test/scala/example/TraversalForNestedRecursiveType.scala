package example

/*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.annotation.tailrec

class TraversalWithNestedRecursiveTypes2 extends AnyFlatSpec with Matchers:

  final case class Finite[L[_]]()

  type Id[A] = A

  given finiteId: Finite[Id] = Finite()

  given finiteInc[L[_] : Finite]: Finite[[A] =>> (A, L[A])] = Finite()

  sealed abstract class SqSize[L[_] : Finite, A]

  final case class Matrix[L[_] : Finite, A](data: L[L[A]]) extends SqSize[L, A]

  final case class Next[L[_] : Finite, A](next: SqSize[[A] =>> (A, L[A]), A]) extends SqSize[L, A]

  type Sq[A] = SqSize[Id, A]

  given Finite[[A =>>(A, A)]]  = Finite()

  val matrix2x2: Sq[Int] = Next(Matrix(
    (
      (1, 2),
      (3, 4),
    )
  ))
*/