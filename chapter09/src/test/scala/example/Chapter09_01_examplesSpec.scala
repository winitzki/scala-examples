package example

import org.scalatest.{FlatSpec, Matchers}
import io.chymyst.ch._

class Chapter09_01_examplesSpec extends FlatSpec with Matchers {

  behavior of "traversable functors"

  it should "show that non-polynomial functors are not traversable" in {
    def useTypeParams[E, R](): Unit = {

      type L[A] = E ⇒ A // Non-polynomial functor.
      type F[A] = Option[A] // Applicative functor.

      def seqs[A] = allOfType[L[F[A]] ⇒ F[L[A]]]

      seqs.length shouldEqual 1 // There are no implementations of this type signature.
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
}
