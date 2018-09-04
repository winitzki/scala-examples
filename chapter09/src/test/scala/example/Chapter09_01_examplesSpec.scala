package example

import WuZip.WuZipSyntax
import cats.Functor
import cats.syntax.functor._
import org.scalatest.{FlatSpec, Matchers}
import io.chymyst.ch._

class Chapter09_01_examplesSpec extends FlatSpec with Matchers {

  behavior of "traversable functors"

  it should "implement `sequence` for A × A × A" in {
    type L[A] = (A, A, A)

    // seq: (F[A], F[A], F[A]) ⇒ F[(A, A, A)]
    def seq[F[_] : WuZip : Functor, A]: L[F[A]] ⇒ F[L[A]] = {
      case (fa1, fa2, fa3) ⇒
        fa1 zip fa2 zip fa3 map { case ((x, y), z) ⇒ (x, y, z) }
    }
  }

  it should "implement `sequence` for Either" in {
    // seq: Either[Z, F[A]] ⇒ F[Either[Z, A]]
    // seq: Z + F[A] ⇒ F[Z + A]
    def seq[F[_] : WuZip : Functor, A, Z](t: Either[Z, F[A]]): F[Either[Z, A]] = t match {
      case Left(z) ⇒ WuZip[F].pure(Left(z))
      case Right(fa) ⇒ fa.map(Right.apply)
    }
  }

  it should "implement `sequence` for a tree" in {
    sealed trait Tree[A]
    final case class Leaf[A](x: A) extends Tree[A]
    final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

    def seq[F[_] : WuZip : Functor, A](t: Tree[F[A]]): F[Tree[A]] = t match {
      case Leaf(fa) ⇒ fa.map(Leaf.apply)
      case Branch(left, right) ⇒
        seq[F, A](left) zip seq[F, A](right) map { case (x, y) ⇒ Branch(x, y) }
    }
  }

  it should "show that non-polynomial functors are not traversable" in {
    def useTypeParams[E, R](): Unit = {
      type L[A] = E ⇒ A // Non-polynomial functor.
      type F[A] = Option[A] // Applicative functor.

      def seqs[A] = allOfType[L[F[A]] ⇒ F[L[A]]]

      seqs.length shouldEqual 1
      // There is one implementation of this type signature,
      // but it always returns `None`.
      seqs.head.lambdaTerm.prettyPrint shouldEqual "a ⇒ (None() + 0)"

      type F2[A] = (A, A) // Another applicative functor.

      def seqs2[A] = allOfType[L[F2[A]] ⇒ F2[L[A]]]

      seqs2.length shouldEqual 1 // There is exactly one implementation of this type signature.
      seqs2.head.lambdaTerm.prettyPrint shouldEqual "a ⇒ Tuple2(b ⇒ a b._1, c ⇒ a c._2)"

      // Check that Z × A is traversable.

      type L3[A] = (Int, A) // Polynomial functor.
      type F3[A] = R ⇒ A // Another applicative functor.

      def seqs3[A] = anyOfType[L3[F3[A]] ⇒ F3[L3[A]]]()

      seqs3.length shouldEqual 1 // There is exactly one implementation of this type signature.
      seqs3.head.lambdaTerm.prettyPrint shouldEqual "a ⇒ b ⇒ Tuple2(a._1, a._2 b)"
    }

    useTypeParams()
  }

  it should "show that infinite list is not traversable" in {
    final case class InfList[A](head: A, tail: () ⇒ InfList[A]) // `tail` is lazy

    // Try to define `seq` for `InfList`:
    // seq : (F[A], F[A], ...) ⇒ F[ (A, A, A, ...) ]
    def seq[F[_] : WuZip : Functor, A](infList: InfList[F[A]]): F[InfList[A]] = {
      infList.head zip seq[F, A](infList.tail()) map { case (head, tail) ⇒ InfList(head, () ⇒ tail) }
    }
    // But this is infinite recursion!

    type F[A] = A
    implicit val functorId: Functor[F] = new Functor[F] {
      override def map[A, B](fa: F[A])(f: A => B): F[B] = f(fa)
    }

    implicit val wuZipId: WuZip[F] = new WuZip[F]() {
      override def wu: F[Unit] = ()

      override def zip[A, B](fa: F[A], fb: F[B]): (A, B) = (fa, fb)
    }

    def infList: InfList[Int] = InfList(123, () ⇒ infList) // Infinite list: 123, 123, 123, ...

    // Try using `seq` on this value, get a stack overflow exception:
    the[java.lang.StackOverflowError] thrownBy seq[F, Int](infList) should have message null
  }

  it should "show that we cannot have `unseq`: F[L[A]] ⇒ L[F[A]]" in {
    type L[A] = Either[Int, A] // Traversable.
    type F[A] = Int ⇒ A // Applicative.
    
    def unseqs[A] = allOfType[F[L[A]] ⇒ L[F[A]]]
    
    unseqs.length shouldEqual 0 // No implementations for this type signature.
    // Can't map Int ⇒ Z + A into Z + (Int ⇒ A) because we can't extract a Z out of Int ⇒ Z + A.
  }
  
  it should "implement seq in another way for A × A × A" in{
    type L[A] = (A, A, A)

    // seq: (F[A], F[A], F[A]) ⇒ F[(A, A, A)]
    def seq[F[_] : WuZip : Functor, A]: L[F[A]] ⇒ F[L[A]] = {
      case (fa1, fa2, fa3) ⇒
        // Arbitrarily select the zipping order as (2, 3, 1).
        fa2 zip fa3 zip fa1 map { case ((x, y), z) ⇒ (x, y, z) }
    }
  }
  
}
