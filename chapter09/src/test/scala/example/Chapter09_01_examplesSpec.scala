package example

import org.scalatest.{FlatSpec, Matchers}
import io.chymyst.ch._

class Chapter09_01_examplesSpec extends FlatSpec with Matchers {

  behavior of "traversable functors"

  it should "show that non-polynomial functors are not traversable" in {
    def useTypeParams[E, R](): Unit = {

      type L[A] = E ⇒ A
      type F[A] = R ⇒ A

      def seqs[A] = allOfType[L[F[A]] ⇒ F[L[A]]]

      seqs[Int].length shouldEqual 0 // There are no implementations of this type signature.
    }
  }
}
