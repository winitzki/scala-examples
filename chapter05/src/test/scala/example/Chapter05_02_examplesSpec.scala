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

    final case class Data1[X](x1: X, x2: X)

    // Provide evidence that the type domain of Functor[] includes Data1.
    implicit object functorData1Evidence extends Functor[Data1] {
      override def fmap[A, B](f: A ⇒ B): Data1[A] ⇒ Data1[B] = {
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
        override def map[A, B](fa: Data1[A])(f: A ⇒ B): Data1[B] = Data1(f(fa.t1), f(fa.t2))
      }

    }

    checkCatsFunctorLaws[Data1, Int, String, Double]()
  }

  behavior of "higher-order types"

  it should "fail to typecheck with wrong kinds" in {

    type G[A] = Either[(A, A, String), A] // Some type constructor.

    type Ap[F[_], A] = F[A]

    type X = Ap[G, Int] // OK; X is now Either[(Int, Int, String), Int]

    val x: X = Left((123, 456, "abc")) // OK

    "type Y = Ap[Int, Int]" shouldNot typeCheck

    "type Z = Ap[G, G]" shouldNot typeCheck
  }

  it should "correctly handle higher-order constructor kinds" in {

    // `G` is a first-order type constructor.
    // The **kind** notation:
    // G: * → *
    // More verbosely: G: (A: *) → *
    type G[A] = Either[(A, A, String), A]

    // `Ap` is a second-order type constructor with two arguments.
    // Ap: (* → *, *) → *
    // More verbosely: Ap: (F: * → *, A: *) → *
    type Ap[F[_], A] = F[A]

    // `Ap2` is a third-order type constructor with three arguments.
    // Ap2: ((* → *, *) → *, * → *, *) → *
    // More verbosely: Ap2: (P: (* → *, *) → *, Q: * → *, R: *) → *
    type Ap2[P[_[_], _], Q[_], R] = P[Q, R]

    // Use them all.
    // X: *  -- i.e. X is a basic type, not a type constructor
    type X = Ap2[Ap, G, Int] // OK; X is now Either[(Int, Int, String), Int]

    val x: X = Left((123, 456, "abc")) // OK

    "type Y = Ap2[Ap, Ap, Int]" shouldNot typeCheck

    "type Z = Ap2[G, G, Int]" shouldNot typeCheck
  }

  // see https://github.com/non/kind-projector
  it should "handle higher-order constructor kinds with the kind projector syntax" in {

    // O2: * → *
    type O2[A] = Option[(A, A)]

    // Ap: (* → *, *) → *
    type Ap[F[_], A] = F[A]

    // App2 will apply the type constructor Q twice to its type argument.
    // The **kind** of App2 is the same as the kind of App in the previous test.
    // App2: ((* → *, *) → *, * → *, *) → *
    type App2[P[_[_], _], Q[_], R] = P[λ[X ⇒ Q[Q[X]]], R]

    type X2 = App2[Ap, O2, Int] // X2 is now Option[(Option[(Int, Int)], Option[(Int, Int)])]

    val x: X2 = Some((Some((1, 2)), Some((3, 4)))) // OK
  }

  behavior of "type class syntax - implicit methods"

  it should "implement Monoid type class with syntax" in {

    // PTTF.
    trait Monoid[T] {
      def empty: T

      def combine: (T, T) ⇒ T
    }

    // Implementation of the "implicit method" syntax.
    object Monoid {

      // This code is in the `Monoid` companion object because the code is generic, not specific to `MyLogData`.
      implicit class MonoidSyntax[M](x: M)(implicit evM: Monoid[M]) {
        // This is an "implicit method" syntax for `combine`. Let's rename it to `+++`.
        def +++(y: M): M = evM.combine(x, y)
      }

    }

    // Use the implicit method syntax now.
    final case class MyLogData(log: String = MyLogData.emptyLog)

    // Declare that MyLogData belongs to the Monoid type class.
    object MyLogData {
      val emptyLog = "no logs so far"

      // Type class instance implemented as an implicit in the companion object.
      implicit val myLogDataMonoidInstance: Monoid[MyLogData] = new Monoid[MyLogData] {
        override def empty = MyLogData()

        // Add to log, with some formatting.
        override def combine: (MyLogData, MyLogData) ⇒ MyLogData = (x, y) ⇒ if (x.log == emptyLog)
          y
        else if (y.log == emptyLog)
          x
        else
          MyLogData(x.log + "\n" + y.log)
        // Note added: This definition of `combine` violates the associativity law when some log strings are empty!
      }
    }

    import Monoid.MonoidSyntax // Import is required for this to work.

    val initialLog = MyLogData()

    val logData1 = MyLogData("all is well")
    val logData2 = MyLogData("error code 12345 found")

    val logResult = initialLog +++ logData1 +++ logData2

    logResult shouldEqual MyLogData("all is well\nerror code 12345 found")
  }
}
