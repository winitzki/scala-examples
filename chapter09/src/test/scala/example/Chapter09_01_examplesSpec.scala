package example

import org.scalatest.{FlatSpec, Matchers}
import io.chymyst.ch._

class Chapter09_01_examplesSpec extends FlatSpec with Matchers {

  behavior of "traversable functors"

  it should "show that non-polynomial functors are not traversable" in {
    def useTypeParams[E, R](): Unit = {

      type L[A] = E ⇒ A // Non-polynomial functor.
      type F[A] = R ⇒ A // Applicative functor.

      def seqs[A] = allOfType[L[F[A]] ⇒ F[L[A]]]

      seqs.length shouldEqual 0 // There are no implementations of this type signature.

      type F2[A] = (A, A) // Another applicative functor.

      def seqs2[A] = allOfType[L[F2[A]] ⇒ F2[L[A]]]

      seqs2.length shouldEqual 0 // There are no implementations of this type signature.

    }
  }
}
