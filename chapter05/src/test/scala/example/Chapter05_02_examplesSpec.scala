package example

import org.scalacheck.Arbitrary
import org.scalacheck.ScalacheckShapeless._
import org.scalatest.{Assertion, FlatSpec}

class Chapter05_02_examplesSpec extends FlatSpec with CatsLawChecking {

  behavior of "higher-order type classes"

  it should "implement Functor type class by hand" in {

    // PTTF containing `fmap` as data.
    trait Functor[F[_]] {
      def fmap[A, B](f: A ⇒ B): F[A] ⇒ F[B]
    }

    final case class Data1[T](t1: T, t2: T)

    // Provide evidence that the type domain of Functor[] includes Data1.
    implicit object functorData1Evidence extends Functor[Data1] {
      def fmap[A, B](f: A ⇒ B): Data1[A] ⇒ Data1[B] = {
        case Data1(t1, t2) ⇒ Data1(f(t1), f(t2))
      }
    }

    // Define `map` as a PTVF using the Functor type class with short syntax.
    def map[F[_] : Functor, A, B](fa: F[A])(f: A ⇒ B): F[B] = {
      val fmapForF: F[A] ⇒ F[B] = implicitly[Functor[F]].fmap(f)
      fmapForF(fa)
    }

    // Use `map` on a Data1 value.
    val data1 = Data1(1, 2) // of type Data1[Int]
    val result = map(data1) { (x: Int) ⇒ x.toString + "abc" } // transformed into Data1[String]
    result shouldEqual Data1("1abc", "2abc")

    // A function that can check functor laws for *any* type constructor F[_].
    def checkFunctorLaws[F[_], A, B, C](implicit
      ff: Functor[F],
      fa: Arbitrary[F[A]],
      ab: Arbitrary[A ⇒ B],
      bc: Arbitrary[B ⇒ C]
    ): Assertion = {
      // Identity law.
      forAll { (fa: F[A]) ⇒ ff.fmap(identity[A])(fa) shouldEqual fa }

      // Composition law.
      forAll { (f: A ⇒ B, g: B ⇒ C, fa: F[A]) ⇒
        (ff.fmap(f) andThen ff.fmap(g)) (fa) shouldEqual ff.fmap(f andThen g)(fa)
      }
    }

    checkFunctorLaws[Data1, Int, String, Double]
  }

  it should "implement Functor type class instance for Data1 using Cats library" in {

    final case class Data1[T](t1: T, t2: T)

    // Put the type class instance into the companion object of Data1.
    object Data1 {

      implicit object CatsFunctorData1Evidence extends cats.Functor[Data1] {
        def map[A, B](fa: Data1[A])(f: A ⇒ B): Data1[B] = Data1(f(fa.t1), f(fa.t2))
      }

    }

    checkCatsFunctorLaws[Data1, Int, String, Double]()
  }
}
