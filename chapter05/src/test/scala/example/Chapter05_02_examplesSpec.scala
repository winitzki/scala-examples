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

  it should "implemented Pointed typeclass with examples for functors and contrafunctors" in {
    trait Functor[F[_]] {
      def map[A, B](fa: F[A])(f: A => B): F[B]
    }
    implicit class FunctorOp[F[_] : Functor, A](fa: F[A]) {
      def map[B](f: A => B): F[B] = implicitly[Functor[F]].map(fa)(f)
    }
    trait Contrafunctor[F[_]] {
      def cmap[A, B](fa: F[A])(f: B => A): F[B]
    }
    implicit class ContrafunctorOp[F[_] : Contrafunctor, A](fa: F[A]) {
      def cmap[B](f: B => A): F[B] = implicitly[Contrafunctor[F]].cmap(fa)(f)
    }

    final case class Pointed[F[_]](wu: F[Unit])
    def pure[F[_] : Pointed : Functor, A](a: A): F[A] = implicitly[Pointed[F]].wu.map(_ => a)

    def purec[F[_] : Pointed : Contrafunctor, A]: F[A] = implicitly[Pointed[F]].wu.cmap(_ ⇒ ())

    type Const[Z, A] = Z

    def pointedOption[U]: Pointed[Const[Option[U], ?]] = Pointed(None: Const[Option[U], Unit])

    type Id[A] = A

    def pointedId: Pointed[Id] = Pointed[Id](())

    def pointedFoG[F[_] : Pointed : Functor, G[_] : Pointed]: Pointed[Lambda[X ⇒ F[G[X]]]] =
      Pointed[Lambda[X ⇒ F[G[X]]]](pure[F, G[Unit]](implicitly[Pointed[G]].wu))

    def pointedCFoG[F[_] : Pointed : Contrafunctor, G[_]]: Pointed[Lambda[X ⇒ F[G[X]]]] =
      Pointed[Lambda[X ⇒ F[G[X]]]](purec[F, G[Unit]])

    def pointedFxG[F[_] : Pointed, G[_] : Pointed]: Pointed[Lambda[X => (F[X], G[X])]] =
      Pointed[Lambda[X => (F[X], G[X])]]((implicitly[Pointed[F]].wu, implicitly[Pointed[G]].wu))

    def pointedEitherFG[F[_] : Pointed, G[_]]: Pointed[Lambda[X => Either[F[X], G[X]]]] =
      Pointed[Lambda[X => Either[F[X], G[X]]]](Left(implicitly[Pointed[F]].wu))

    def pointedFuncFG[F[_] : Pointed, C[_]]: Pointed[Lambda[X => C[X] => F[X]]] =
      Pointed[Lambda[X => C[X] => F[X]]](_ => implicitly[Pointed[F]].wu)

    def pointedCoF[C[_] : Pointed : Contrafunctor, F[_]]: Pointed[Lambda[X ⇒ C[F[X]]]] =
      Pointed[Lambda[X ⇒ C[F[X]]]](purec[C, F[Unit]])

    def pointedFoC[C[_] : Pointed, F[_] : Pointed : Functor]: Pointed[Lambda[X => F[C[X]]]] =
      Pointed[Lambda[X => F[C[X]]]](pure[F, C[Unit]](implicitly[Pointed[C]].wu))

    def pointedFuncFC[C[_] : Pointed, F[_]]: Pointed[Lambda[X => F[X] => C[X]]] =
      Pointed[Lambda[X => F[X] => C[X]]](_ => implicitly[Pointed[C]].wu)

    // Recursive construction.
    type S[A, R] = Either[A, (A, R)]
    final case class F[A](s: S[A, F[A]])
    implicit val pointedF: Pointed[F] = Pointed(F(Left(())))

    // Co-pointed typeclass.

    trait Copointed[F[_]] {
      def ex[A]: F[A] => A
    }
    def extract[F[_] : Copointed, A](f: F[A]): A = implicitly[Copointed[F]].ex(f)

    //    type Id[A] = A
    def copointedId: Copointed[Id] = new Copointed[Id] {
      def ex[A]: Id[A] ⇒ A = identity
    }

    def copointedFoG[F[_] : Copointed, G[_] : Copointed]: Copointed[Lambda[X => F[G[X]]]] =
      new Copointed[Lambda[X => F[G[X]]]] {
        def ex[A]: F[G[A]] => A = extract[F, G[A]] _ andThen extract[G, A]
      }

    def copointedFxG[F[_] : Copointed, G[_]]: Copointed[Lambda[X => (F[X], G[X])]] =
      new Copointed[Lambda[X => (F[X], G[X])]] {
        def ex[A]: ((F[A], G[A])) => A = {
          case (f, g) => extract(f)
        } // ((_._1) : ((F[A], G[A])) => F[A]) andThen extract[F,A]
      }

    def copointedEitherFG[F[_] : Copointed, G[_] : Copointed]: Copointed[Lambda[X => Either[F[X], G[X]]]] =
      new Copointed[Lambda[X => Either[F[X], G[X]]]] {
        def ex[A]: Either[F[A], G[A]] ⇒ A = {
          case Left(f) ⇒ extract(f)
          case Right(g) ⇒ extract(g)
        }
      }

    import PipeOps._
    def copointedFunc[C[_] : Pointed : Contrafunctor, P[_] : Copointed]: Copointed[Lambda[X => C[X] => P[X]]] = new Copointed[Lambda[X => C[X] => P[X]]] {
      def ex[A]: (C[A] => P[A]) => A = h ⇒ purec[C, A] pipe h pipe extract[P, A] // or  h => extract[P, A](h(purec[C, A]))
    }

    type S1[A, R] = Either[A, (R, R)]

    def exS1[A]: S1[A, A] => A = {
      case Left(a) => a
      case Right((a1, a2)) => a1 // Could be a2.
    }

    def bimap_S[A, B, P, Q](f: A => B, g: P => Q): S1[A, P] => S1[B, Q] = {
      case Left(a) => Left(f(a))
      case Right((x, y)) => Right((g(x), g(y)))
    }

    final case class F1[A](s: S1[A, F1[A]])
    val copointedF: Copointed[F1] = new Copointed[F1] {
      override def ex[A]: F1[A] ⇒ A = { case F1(s) ⇒ exS1(bimap_S(identity[A], ex[A])(s)) }
    }


  }
}
