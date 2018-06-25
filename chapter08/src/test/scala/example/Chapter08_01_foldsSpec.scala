package example

import algebra.ring.Field
import cats.syntax.foldable._
import cats.{Applicative, Foldable, Functor, InvariantSemigroupal}
import io.chymyst.ch.implement
import org.scalatest.{FlatSpec, Matchers}
import spire.implicits._
import spire.math.Numeric

import scala.Predef.{any2stringadd => _, _} // ridiculous

class Chapter08_01_foldsSpec extends FlatSpec with Matchers {

  behavior of "folds"

  it should "implement applicative fusion for folds using plain product" in {

    // Fld[Z, R] = R × (R × Z ⇒ R)
    // This `Fold` will be applied to a sequence with items of type `Z`,
    // and will accumulate a result value of type `R`.
    case class Fld[Z, R](init: R, update: (R, Z) ⇒ R)

    // Syntax for Foldable.
    implicit class FldSyntax[F[_] : Foldable, Z](fa: F[Z]) {
      def fld[R](fld: Fld[Z, R]): R = fa.foldl(fld.init)(fld.update)
    }

    implicit def applyFld[Z]: InvariantSemigroupal[Fld[Z, ?]] = new InvariantSemigroupal[Fld[Z, ?]] {
      override def imap[A, B](fa: Fld[Z, A])(f: A ⇒ B)(g: B ⇒ A): Fld[Z, B] = implement

      override def product[A, B](fa: Fld[Z, A], fb: Fld[Z, B]): Fld[Z, (A, B)] = implement
    }

    // Syntax for combining the folds.
    implicit class FldCombine[Z, A](fld: Fld[Z, A]) {
      def ×[B](otherFld: Fld[Z, B]): Fld[Z, (A, B)] = applyFld.product(fld, otherFld)
    }


    // Define some useful folding operations for numeric data.
    def len[N: Numeric]: Fld[N, N] = Fld(0, (l, i) ⇒ l + 1)

    def sum[N: Numeric]: Fld[N, N] = Fld(0, (s, i) ⇒ s + i)

    def twoFold[N: Numeric]: Fld[N, (N, N)] = len × sum

    import cats.instances.list._

    // This fold is performed in a single traversal.
    val result = (1 to 10).toList.fld(twoFold)

    result shouldEqual(10, 55)

    val average = result._2 / result._1.toDouble

    average shouldEqual 5.5
  }

  // This is inconvenient: We would like to incorporate a final computation into the `Fld`,
  // rather than work with tupled results.
  // This `Fold` will be applied to a sequence with items of type `Z`, 
  // will accumulate a value of type `A`, and will output a result value of type `R`.
  it should "implement applicative fusion for folds" in {
    case class Fold[Z, A, R](init: A, update: (A, Z) ⇒ A, transform: A ⇒ R)

    // Syntax for Foldable.
    implicit class FoldSyntax[F[_] : Foldable, Z](fa: F[Z]) {
      def fldl[A, R](fld: Fold[Z, A, R]): R = fld.transform(fa.foldl(fld.init)(fld.update))
    }

    // Syntax for combining the folds.
    implicit class FoldCombine[Z, A, R](fld: Fold[Z, A, R]) {
      def ×[B, T](otherFld: Fold[Z, B, T]): Fold[Z, (A, B), (R, T)] = implement
    }

    // Now `Fold[Z, C, ?]` is a functor.
    implicit def functorFold[Z, C]: Functor[Fold[Z, C, ?]] = new Functor[Fold[Z, C, ?]] {
      override def map[A, B](fa: Fold[Z, C, A])(f: A ⇒ B): Fold[Z, C, B] = implement
    }

    // Syntax for appending an operation.
    implicit class FoldTransform[Z, A, R](fld: Fold[Z, A, R]) {
      def andThen[T](f: R ⇒ T): Fold[Z, A, T] = implement
    }

    // Define some useful folding operations for numeric data.
    def len[N: Numeric]: Fold[N, N, N] = Fold(0, (l, i) ⇒ l + 1, identity)

    def sumMap[N: Numeric](f: N ⇒ N): Fold[N, N, N] = Fold(0, (s, i) ⇒ s + f(i), identity)

    def sum[N: Numeric]: Fold[N, N, N] = sumMap(identity)

    // Note that the accumulator type has become `(N, N)`, showing that we have to accumulate two values.
    def average[N: Numeric]: Fold[N, (N, N), N] = (sum × len) andThen { case (s, l) ⇒ s / l }


    import cats.instances.list._
    // This fold is performed in a single traversal.
    val result = (1 to 10).map(_.toDouble).toList.fldl(average)

    result shouldEqual 5.5
  }

  // This is still cumbersome. We would like to write simply `sum / len`.
  // Let us implement a type class instance of spire.math.Field for Fold[Z, A, N].
  // But we find that the type `A` must change after binary operations.

  it should "implement fold fusion using arithmetic operators" in {
    // Make the type parameter `A` existential instead of universal.
    trait Fold[Z, R] { // This imitates a case class with an embedded type member.
    type A

      def init: A

      val update: (A, Z) ⇒ A
      val transform: A ⇒ R
    }

    // Helper function to create instances of this more easily.
    // The types may need to be specified explicitly in some cases.
    def mkFold[Z, A1, R](theInit: A1)(theUpdate: (A1, Z) ⇒ A1)(theTransform: A1 ⇒ R): Fold[Z, R] = new Fold[Z, R] {
      override type A = A1
      override val init: A = theInit
      override val update: (A, Z) ⇒ A = theUpdate
      override val transform: A ⇒ R = theTransform
    }

    // Syntax for Foldable.
    implicit class FoldSyntax[F[_] : Foldable, Z](fa: F[Z]) {
      def fldl[A, R](fld: Fold[Z, R]): R = fld.transform(fa.foldl(fld.init)(fld.update))
    }

    // Syntax for combining the folds.
    implicit class FoldCombine[Z, R](fld: Fold[Z, R]) {
      def ×[B, T](otherFld: Fold[Z, T]): Fold[Z, (R, T)] = mkFold[Z, (fld.A, otherFld.A), (R, T)] {
        (fld.init, otherFld.init)
      } {
        case ((a1, a2), z) ⇒ (fld.update(a1, z), otherFld.update(a2, z))
      } {
        case (a1, a2) ⇒ (fld.transform(a1), otherFld.transform(a2))
      }
    }

    // Syntax for appending another transformation.
    implicit class FoldTransform[Z, R](fld: Fold[Z, R]) {
      def andThen[T](f: R ⇒ T): Fold[Z, T] = mkFold(fld.init)(fld.update)(fld.transform andThen f)
    }

    // Now `Fold[Z, ?]` is a functor.
    implicit def functorFold[Z]: Functor[Fold[Z, ?]] = new Functor[Fold[Z, ?]] {
      override def map[R, T](fa: Fold[Z, R])(f: R ⇒ T): Fold[Z, T] = fa andThen f
    }

    // It is also applicative.
    implicit def applicativeFold[Z, C]: Applicative[Fold[Z, ?]] = new Applicative[Fold[Z, ?]] {
      override def pure[A](x: A): Fold[Z, A] = mkFold[Z, A, A](x)((_, _) ⇒ x)(_ ⇒ x)

      override def ap[A, B](ff: Fold[Z, A ⇒ B])(fa: Fold[Z, A]): Fold[Z, B] = (ff × fa) andThen { case (f, x) ⇒ f(x) }
    }

    // Type class instance for `spire.math.Field`, defining arithmetic operations on `Fold`s.
    implicit def numericFold[Z, N](implicit num: Numeric[N]): Field[Fold[Z, N]] = new Field[Fold[Z, N]] {
      private def binaryOp(x: Fold[Z, N], y: Fold[Z, N], op: (N, N) ⇒ N): Fold[Z, N] = (x × y) andThen {
        case (p, q) ⇒ op(p, q)
      }

      override def negate(x: Fold[Z, N]): Fold[Z, N] = x andThen (-_)

      override def plus(x: Fold[Z, N], y: Fold[Z, N]): Fold[Z, N] = binaryOp(x, y, _ + _)

      override def div(x: Fold[Z, N], y: Fold[Z, N]): Fold[Z, N] = binaryOp(x, y, _ / _)

      override def one: Fold[Z, N] = mkFold[Z, N, N](num.one)((_, _) ⇒ num.one)(_ ⇒ num.one)

      override def zero: Fold[Z, N] = mkFold[Z, N, N](num.zero)((_, _) ⇒ num.zero)(_ ⇒ num.zero)

      override def times(x: Fold[Z, N], y: Fold[Z, N]): Fold[Z, N] = binaryOp(x, y, _ * _)
    }

    // Define some useful folding operations for numeric data.
    def len[N: Numeric]: Fold[N, N] = mkFold[N, N, N](0)((l, i) ⇒ l + 1)(identity)

    def sumMap[N: Numeric](f: N ⇒ N): Fold[N, N] = mkFold[N, N, N](0)((l, i) ⇒ l + f(i))(identity)

    def sum[N: Numeric]: Fold[N, N] = sumMap(identity)

    // Note that the accumulator type `(N, N)` is now automatic. We do not need to take care about that.
    def average[N: Numeric]: Fold[N, N] = sum / len

    import cats.instances.list._

    // This fold is performed in a single traversal.
    val result = (1 to 10).map(_.toDouble).toList.fldl(average)

    result shouldEqual 5.5
  }

}
