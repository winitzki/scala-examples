package example

import cats.syntax.functor._
import cats.instances.list._
import cats.syntax.foldable._
import cats.{Foldable, Functor, InvariantSemigroupal}
import io.chymyst.ch.implement
import org.scalatest.{FlatSpec, Matchers}
import spire.implicits._
import spire.math.Numeric

import java.util.ArrayDeque
import scala.Predef.{any2stringadd ⇒ _, _}
import scala.collection.JavaConverters.asScalaIteratorConverter // ridiculous

class Chapter08_01_foldsSpec extends FlatSpec with Matchers {

  behavior of "folds"

  val list10 = (1 to 10).map(_.toDouble).toList

  it should "implement applicative fusion for folds using plain product" in {

    // Fold0[Z, R] = R × (R × Z ⇒ R)
    // This `Fold0` will be applied to a sequence with items of type `Z`,
    // and will accumulate a result value of type `R`.
    case class Fold0[Z, R](init: R, update: (R, Z) ⇒ R)

    // Syntax for Foldable.
    implicit class FldSyntax[F[_] : Foldable, Z](fa: F[Z]) {
      def fld[R](fld: Fold0[Z, R]): R = fa.foldLeft(fld.init)(fld.update)
    }

    // "zippable profunctor" = "invariant semigroupal"

    implicit def applyFld[Z]: InvariantSemigroupal[Fold0[Z, *]] = new InvariantSemigroupal[Fold0[Z, *]] {
      override def imap[A, B](fa: Fold0[Z, A])(f: A ⇒ B)(g: B ⇒ A): Fold0[Z, B] = implement

      override def product[A, B](fa: Fold0[Z, A], fb: Fold0[Z, B]): Fold0[Z, (A, B)] = implement
    }

    // Syntax for combining the folds. Use the `product` as defined above.
    implicit class FldCombine[Z, A](fld: Fold0[Z, A]) {
      def zip[B](otherFld: Fold0[Z, B]): Fold0[Z, (A, B)] = applyFld.product(fld, otherFld)
    }


    // Define some useful folding operations for numeric data.
    def len[N: Numeric]: Fold0[N, N] = Fold0(0, (l, _) ⇒ l + 1)

    def sum[N: Numeric]: Fold0[N, N] = Fold0(0, (s, i) ⇒ s + i)

    def twoFold[N: Numeric]: Fold0[N, (N, N)] = len zip sum

    // This fold is performed in a single traversal.
    val result = list10.fld(twoFold)

    result shouldEqual ((10, 55))

    val average = result._2 / result._1

    average shouldEqual 5.5
  }

  // This is inconvenient: We would like to incorporate a final computation into the `Fold`,
  // rather than work with tupled results. So, add a `transform: A ⇒ R` to `Fold0`.
  // The new `Fold1` will be applied to a sequence with items of type `Z`, 
  // will accumulate a value of type `A`, and will output a result value of type `R`.

  case class Fold1[Z, A, R](init: A, update: (A, Z) ⇒ A, transform: A ⇒ R)

  // Syntax for Foldable.
  implicit class Fold1Syntax[F[_] : Foldable, Z](fa: F[Z]) {
    def foldl1[A, R](fld: Fold1[Z, A, R]): R = fld.transform(fa.foldl(fld.init)(fld.update))
  }

  // Syntax for combining the folds.
  implicit class Fold1Combine[Z, A, R](fld: Fold1[Z, A, R]) {
    def zip[B, T](otherFld: Fold1[Z, B, T]): Fold1[Z, (A, B), (R, T)] = implement
  }

  // Now `Fold1[Z, C, *]` is a functor. (This is the "free functor" construction.)
  implicit def functorFold1[Z, C]: Functor[Fold1[Z, C, *]] = new Functor[Fold1[Z, C, *]] {
    override def map[A, B](fa: Fold1[Z, C, A])(f: A ⇒ B): Fold1[Z, C, B] = implement
  }

  // Syntax for appending an operation.
  implicit class Fold1Transform[Z, A, R](fld: Fold1[Z, A, R]) {
    def andThen[T](f: R ⇒ T): Fold1[Z, A, T] = implement
  }

  // Define some useful folding operations for numeric data.
  def len1[N: Numeric]: Fold1[N, N, N] = Fold1(0, (l, i) ⇒ l + 1, identity)

  def sumMap1[N: Numeric](f: N ⇒ N): Fold1[N, N, N] = Fold1(0, (s, i) ⇒ s + f(i), identity)

  def sum1[N: Numeric]: Fold1[N, N, N] = sumMap1(identity)

  // Note that the accumulator type has become `(N, N)`, showing that we have to accumulate two values.
  def average1[N: Numeric]: Fold1[N, (N, N), N] = (sum1 zip len1) andThen { case (s, l) ⇒ s / l }

  it should "implement applicative fusion for folds" in {

    // This fold is performed in a single traversal.
    val result = list10.foldl1(average1)

    result shouldEqual 5.5
  }

  // This is still cumbersome. We would like to write simply `sum / len`.
  // Let us implement arithmetic operations on `Fold1`.

  implicit class Fold1Arithmetic[Z, A, N](fold1: Fold1[Z, A, N])(implicit num: Numeric[N]) {

    def unary_- : Fold1[Z, A, N] = fold1 andThen (-_)

    def +[B](fold: Fold1[Z, B, N]): Fold1[Z, (A, B), N] = binaryOp(fold1, fold, _ + _)

    def *[B](fold: Fold1[Z, B, N]): Fold1[Z, (A, B), N] = binaryOp(fold1, fold, _ * _)

    def /[B](fold: Fold1[Z, B, N]): Fold1[Z, (A, B), N] = binaryOp(fold1, fold, _ / _)

    def one: Fold1[Z, N, N] = Fold1(num.one, (_, _) ⇒ num.one, _ ⇒ num.one)

    def zero: Fold1[Z, N, N] = Fold1(num.zero, (_, _) ⇒ num.zero, _ ⇒ num.zero)

    private def binaryOp[B](x: Fold1[Z, A, N], y: Fold1[Z, B, N], op: (N, N) ⇒ N): Fold1[Z, (A, B), N] = (x zip y) andThen {
      case (p, q) ⇒ op(p, q)
    }
  }

  it should "implement fold fusion using arithmetic operators" in {

    // This fold is performed in a single traversal.
    val result = list10.foldl1(sum1[Double] / len1[Double])

    result shouldEqual 5.5
  }

  object FoldChapter11 {
    final case class FoldOp[Z, R, A](init: R, update: (R, Z) => R, transform: R => A)

    import io.chymyst.ch._ // Import some symbols from the `curryhoward` library.

    //    def bad[A, B]: (() ⇒ A, A ⇒ B) ⇒  B = implement // curryhoward does not support function0[A]???
    //        def bad[A, B]: (Unit ⇒ A, A ⇒ B) ⇒  B = implement // curryhoward does not support Unit???

    implicit class FoldOpSyntax[Z](zs: Seq[Z]) {
      def runFold[R, A](op: FoldOp[Z, R, A]): A = op.transform(zs.foldLeft(op.init)(op.update))
    }

    implicit class FoldOpScan[Z](zs: Seq[Z]) {
      def runScan[R, A](op: FoldOp[Z, R, A]): Seq[A] = zs.scanLeft(op.init)(op.update).drop(1).map(op.transform)
    }

    implicit class FoldOpZip[Z, R, A](op: FoldOp[Z, R, A]) {
      def zip[S, B](other: FoldOp[Z, S, B]): FoldOp[Z, (R, S), (A, B)] = implement

      def map[B](f: A => B): FoldOp[Z, R, B] = implement

      def map2[S, B, C](other: FoldOp[Z, S, B])(f: (A, B) => C): FoldOp[Z, (R, S), C] = implement
    } // The type signatures unambiguously determine the implementations.

    implicit class FoldOpMath[Z, R](op: FoldOp[Z, R, Double]) {
      def binaryOp[S](other: FoldOp[Z, S, Double])(f: (Double, Double) => Double): FoldOp[Z, (R, S), Double] = op.map2(other) { case (x, y) => f(x, y) }

      def +[S](other: FoldOp[Z, S, Double]): FoldOp[Z, (R, S), Double] = op.binaryOp(other)(_ + _)

      def /[S](other: FoldOp[Z, S, Double]): FoldOp[Z, (R, S), Double] = op.binaryOp(other)(_ / _)
    } // May need to define more operations here.
  }

  it should "implement fold fusion with arithmetic syntax as in Section 11.2.5" in {
    import FoldChapter11._
    val sum = new FoldOp[Double, Double, Double](0, (s, i) => s + i, identity)
    val length = new FoldOp[Double, Double, Double](0, (s, _) => s + 1, identity)

    Seq(1.0, 2.0, 3.0).runFold(sum / length) shouldEqual 2.0

    val average = sum / length

    def constfold(x: Double): FoldOp[Double, Double, Double] = FoldOp(x, (_, _) ⇒ x, _ ⇒ x)

    def window[A](n: Int): FoldOp[A, IndexedSeq[A], IndexedSeq[A]] = FoldOp(init = Vector(),
      update = { (window, x) => (if (window.size < n) window else window.drop(1)) :+ x }, identity)

    def window_average(n: Int) = window[Double](n).map(_.sum / n)

    (0 to 10).map(_.toDouble).runFold(average) shouldEqual 5.0
    (0 to 10).map(_.toDouble).runScan(average) shouldEqual Vector(0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0)
    (0 to 10).map(_.toDouble).runFold(window_average(3)) shouldEqual 9.0
    (0 to 10).map(_.toDouble).runScan(window_average(3)) shouldEqual Vector(0.0, 0.3333333333333333, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0)
  }

  it should "implement flatMap for FoldOp as in Section 11.2.6" in {
    import FoldChapter11._
    val sum = new FoldOp[Double, Double, Double](0, (s, i) => s + i, identity)
    val length = new FoldOp[Double, Double, Double](0, (s, _) => s + 1, identity)
    val average = sum / length

    implicit class FoldFlatMap[Z, R, A](op: FoldOp[Z, R, A]) {
      def flatMap[S, B](f: A => FoldOp[Z, S, B]): FoldOp[Z, (R, S), B] = {
        // Create a new `FoldOp()` value. We need `init`, `update`, and `transform`.
        val init: (R, S) = (op.init, f(op.transform(op.init)).init)
        val update: ((R, S), Z) => (R, S) = {
          case ((r, s), z) =>
            val newR = op.update(r, z)
            val newOp: FoldOp[Z, S, B] = f(op.transform(newR)) // newR or r here?
            val newS = newOp.update(s, z)
            (newR, newS)
        }
        val transform: ((R, S)) => B = {
          case (r, s) =>
            val newOp: FoldOp[Z, S, B] = f(op.transform(r))
            newOp.transform(s)
        }
        FoldOp(init, update, transform)
      }
    }

    val expected = List(1.0, 1.25, 1.5, 1.75, 2.0, 2.25, 2.5, 2.75, 3.0, 3.25)
    list10.runScan(average).runScan(average) shouldEqual expected
    val average2 = average.flatMap(x ⇒ FoldOp[Double, Double, Double](0, (a, _) ⇒ a + x, identity) / length)
    list10.runScan(average2) shouldEqual expected
    val average2a = for {
      x ← average
      acc ← FoldOp[Double, Double, Double](0, (a, _) ⇒ a + x, identity)
      n ← length
    } yield acc / n
    list10.runScan(average2a) shouldEqual expected
  }

  behavior of "scans"

  // Syntax for scanLeft with Fold1.
  implicit class FoldScanLeft1[Z](fa: Seq[Z]) {
    def scanl1[A, R](fold: Fold1[Z, A, R]): Seq[R] = fa.scanLeft(fold.init)(fold.update).map(fold.transform)
  }

  it should "implement syntax for scans" in {
    list10.scanl1(average1).tail shouldEqual List(1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0, 5.5)
  }

  behavior of "monadic fold"

  // `Fold1[Z, A, R]` is actually a monad in R.
  // We can implement `flatMap` in a useful way for Fold1.
  // However, we need to change the type of `A` because we will need to accumulate more data. 
  implicit class Fold1FlatMap[Z, A, R](fold: Fold1[Z, A, R]) {
    def flatMap[B, T](f: R ⇒ Fold1[Z, B, T]): Fold1[Z, (A, B), T] = {
      // Create a new `Fold1()` value. We need `init`, `update`, and `transform`.
      val init: (A, B) = (fold.init, f(fold.transform(fold.init)).init)
      val update: ((A, B), Z) ⇒ (A, B) = {
        case ((a1, b1), z) ⇒
          val newA = fold.update(a1, z)
          val newFold: Fold1[Z, B, T] = f(fold.transform(newA)) // newA or a1?
          val newB = newFold.update(b1, z)
          (newA, newB)
      }
      val transform: ((A, B)) ⇒ T = {
        case (a1, b1) ⇒
          val newFold: Fold1[Z, B, T] = f(fold.transform(a1))
          newFold.transform(b1)
      }
      Fold1(init, update, transform)
    }
  }

  it should "implement monadic fold" in {
    // Compute running averages of running average.
    list10.scanl1(average1).tail shouldEqual List(1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0, 5.5)
    // This goes over the list twice.
    list10.scanl1(average1).tail.scanl1(average1).tail shouldEqual List(1.0, 1.25, 1.5, 1.75, 2.0, 2.25, 2.5, 2.75, 3.0, 3.25)

    def ave1Ave1[N: Numeric]: Fold1[N, _, N] = average1.flatMap(x ⇒ Fold1[N, N, N](0, (a, z) ⇒ a + x, identity) / len1)

    list10.scanl1(ave1Ave1).tail shouldEqual List(1.0, 1.25, 1.5, 1.75, 2.0, 2.25, 2.5, 2.75, 3.0, 3.25)

    // Use the for/yield syntax for the monadic folds. This depends on having both `map` and `flatMap`.
    // This syntax is much more visual.
    def ave1ave1forYield[N: Numeric]: Fold1[N, _, N] = for {
      x ← average1
      acc ← Fold1[N, N, N](0, (a, _) ⇒ a + x, identity)
      n ← len1
    } yield acc / n

    list10.scanl1(ave1ave1forYield).tail shouldEqual List(1.0, 1.25, 1.5, 1.75, 2.0, 2.25, 2.5, 2.75, 3.0, 3.25)
  }

  it should "implement code example 1 in new section 11.2.5" in {
    final case class Fold[Z, R](init: R, update: (R, Z) => R)

    def run[Z, R](as: Seq[Z], fold: Fold[Z, R]): R = as.foldLeft(fold.init)(fold.update)

    def zipFold[Z, R1, R2](op1: Fold[Z, R1], op2: Fold[Z, R2]): Fold[Z, (R1, R2)] =
      Fold((op1.init, op2.init), (r, z) => (op1.update(r._1, z), op2.update(r._2, z)))

    val sum = Fold[Double, Double](0, _ + _)
    val length = Fold[Double, Int](0, (n, _) ⇒ n + 1)
    val sumLength: Fold[Double, (Double, Int)] = zipFold(sum, length)

    val res: (Double, Int) = run(Seq(1.0, 2.0, 3.0), sumLength)

    val average = res._1 / res._2

    average shouldEqual 2.0
  }
}
