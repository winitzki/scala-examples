package example

import cats.{Bifunctor, Contravariant, Functor, Monoid, Semigroup}
import io.chymyst.ch._
import org.scalacheck.Arbitrary
import org.scalacheck.ScalacheckShapeless._
import org.scalatest.{Assertion, FlatSpec}

import scala.util.Try

class Chapter05_03_workedExamplesSpec extends FlatSpec with CatsLawChecking {

  behavior of "worked examples"

  it should "ex01" in {
    // PTTF contains data.
    final case class BitSized[T](size: Int)

    // Cannot use `type BitSized[T] = Int` because of type collision ("ambiguous implicit values").

    // Define type domain and the relevant data.
    implicit val bitSizedIntEvidence: BitSized[Int] = BitSized(32)
    implicit val bitSizedLongEvidence: BitSized[Long] = BitSized(64)

    // PTVF.
    def bitsize[T](implicit ev: BitSized[T]): Int = ev.size

    bitsize[Int] shouldEqual 32
    bitsize[Long] shouldEqual 64
    "bitsize[Float]" shouldNot compile
  }

  it should "ex02" in {
    type Data = Option[String ⇒ String]

    // Boring instance: often produce None
    val monoidInstance1 = new Monoid[Data] {
      def empty: Data = Some(identity[String])

      def combine(x: Data, y: Data): Data = x match {
        case Some(s2sX) ⇒ y match {
          case Some(s2sY) ⇒ Some(s2sX andThen s2sY)
          case None ⇒ None
        }
        case None ⇒ None
      }
    }

    // Interesting instance: rarely produce None
    val monoidInstance2 = new Monoid[Data] {
      def empty: Data = None

      def combine(x: Data, y: Data): Data = x match {
        case Some(s2sX) ⇒ y match {
          case Some(s2sY) ⇒ Some(s2sX andThen s2sY)
          case None ⇒ x
        }
        case None ⇒ y
      }
    }

    // Define comparison for functions.
    def dataIsEqual(d1: Data, d2: Data): Assertion = d1 match {
      case Some(s2sX) ⇒ d2 match {
        case Some(s2sY) ⇒ forAll { x: String ⇒ s2sX(x) shouldEqual s2sY(x) }
        case None ⇒ d1 shouldEqual d2
      }
      case None ⇒ d1 shouldEqual d2
    }

    // Check the laws for each instance.
    checkCatsMonoidLaws[Data](dataIsEqual)(implicitly[Arbitrary[Data]], monoidInstance1)
    checkCatsMonoidLaws[Data](dataIsEqual)(implicitly[Arbitrary[Data]], monoidInstance2)
  }

  //    If A and B are monoids, define monoid instance for A × B
  it should "ex03" in {
    implicit def monoidABInstance[A, B](implicit evA: Monoid[A], evB: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
      def empty: (A, B) = (evA.empty, evB.empty)

      def combine(x: (A, B), y: (A, B)): (A, B) = (evA.combine(x._1, y._1), evB.combine(x._2, y._2))
    }

    // Testing.

    // Initially, we have no Int or Double monoid instances in scope.

    "implicitly[Monoid[Int]]" shouldNot compile
    "implicitly[Monoid[Double]]" shouldNot compile

    // Declare these instances now.

    implicit val monoidIntInstance = new Monoid[Int] {
      def empty: Int = 1

      def combine(x: Int, y: Int): Int = x * y
    }

    // Multiplication for Double is not precisely associative.
    implicit val monoidDoubleInstance = new Monoid[Double] {
      def empty: Double = 0.0

      def combine(x: Double, y: Double): Double = x + y
    }

    // After this, we have both Int and Double monoid instances in scope.

    implicitly[Monoid[Int]]
    implicitly[Monoid[Double]]

    // Should be able to derive monoid instance for (Int, Double) automatically now.
    checkCatsMonoidLaws[(Int, Double)]()
  }

  //    If A is a monoid and B is a semigroup then A+B is a monoid
  it should "ex04" in {
    implicit def monoidABInstance[A, B](implicit evA: Monoid[A], evB: Semigroup[B]): Monoid[Either[A, B]] = new Monoid[Either[A, B]] {
      override def empty: Either[A, B] = Left(evA.empty) // No choice since we don't have a chosen element in B.
      override def combine(x: Either[A, B], y: Either[A, B]): Either[A, B] = x match {        case Left(xa) ⇒ y match {
          case Left(ya) ⇒ Left(evA.combine(xa, ya))
          case Right(yb) ⇒ y // Laws do not hold if we put x here.
        }
        case Right(xb) ⇒ y match {
          case Left(ya) ⇒ x // Laws do not hold if we put y here.
          case Right(yb) ⇒ Right(evB.combine(xb, yb))
        }
      }
    }

    // By default, have no Int semigroup or Double monoid instances in scope.

    "implicitly[Semigroup[Int]]" shouldNot compile
    "implicitly[Monoid[Double]]" shouldNot compile

    // Declare these instances now.
    implicit val semigroupIntInstanceNonCommutAssoc = new Semigroup[Int] {
      def combine(x: Int, y: Int): Int = nonCommutAssoc(x, y)

      // Budden's function: see F. J. Budden, A Non-Commutative, Associative Operation on the Reals.
      //   The Mathematical Gazette, Vol. 54, No. 390 (Dec., 1970), pp. 368-372
      private def nonCommutAssoc(x: Int, y: Int): Int =
        if (x % 2 == 0) x + y else x - y
    }

    // Multiplication for Double is not precisely associative.
    implicit val monoidDoubleInstance = new Monoid[Double] {
      def empty: Double = 0.0

      def combine(x: Double, y: Double): Double = x + y
    }

    // Should be able to derive monoid instance for (Int, Double) now
    checkCatsMonoidLaws[Either[Double, Int]]()
  }

  // Define a functor instance for type F[T] = Seq[Try[T]]
  it should "ex05" in {
    type F[T] = Seq[Try[T]]

    implicit val fFunctorInstance: Functor[F] = new Functor[F] {
      def map[A, B](fa: F[A])(f: A ⇒ B): F[B] = fa.map(_.map(f))
    }
  }


  // Define a Cats’ Bifunctor instance for Q[X,Y] ≡ X + X × Y
  it should "ex06" in {
    // Define a bifunctor as a type constructor.
    type Q[X, Y] = Either[X, (X, Y)]

    implicit val bifunctorQ = new Bifunctor[Q] {
      def bimap[A, B, C, D](fab: Either[A, (A, B)])(f: A ⇒ C, g: B ⇒ D): Either[C, (C, D)] = implement
    }

    checkCatsBifunctorLaws[Q, Int, String, Boolean, Char, Long, Double]()
  }
  
  it should "define bifunctor by hand as a case class" in {
    final case class Q[X, Y](e: Either[X, (X, Y)])
    implicit val bifunctorQ = new Bifunctor[Q] {
      def bimap[A, B, C, D](fab: Q[A, B])(f: A => C, g: B => D): Q[C, D] = fab.e match {
        case Left(a) => Q(Left(f(a)))
        case Right((a, b)) => Q(Right((f(a), g(b))))
      }
    }
    
    checkCatsBifunctorLaws[Q, Int, String, Boolean, Char, Long, Double]()
  }

  // Define a ContraFunctor instance for type constructor A ⇒ Int

  // The given contrafunctor.
  type C[A] = A ⇒ Int

  it should "ex07" in {
    // First, we define a type class.
    // PTTF with data.
    trait ContraFunctor[Co[_]] {
      def contrafmap[A, B](f: B ⇒ A): Co[A] ⇒ Co[B]
    }
    // Type domain includes C.
    implicit val ReaderCF = new ContraFunctor[C] {
      def contrafmap[A, B](f: B ⇒ A): (A ⇒ Int) ⇒ B ⇒ Int = implement
    }

  }

  it should "do ex07 with Cats" in {

    implicit val cContraFunctorInstance = new Contravariant[C] {
      override def contramap[A, B](fa: A ⇒ Int)(f: B ⇒ A): B ⇒ Int = implement
    }

    def cEqual[T: Arbitrary](c1: C[T], c2: C[T]): Assertion = forAll { t: T ⇒ c1(t) shouldEqual c2(t) }

    checkCatsContraFunctorLaws[C, Int, String, Boolean](cEqual)
  }


  // Define a functor instance for recursive type Q[A] ≡ (Int ⇒ A) + Int + Q[A]
  it should "ex08" in {
    sealed trait Q[A]
    final case class C1[A](i: Int ⇒ A) extends Q[A]
    final case class C2[A](x: Int) extends Q[A]
    final case class C3[A](q: Q[A]) extends Q[A]

    implicit val qFunctorInstance: Functor[Q] = new Functor[Q] {
      // This function is recursive.
      override def map[A, B](qa: Q[A])(f: A ⇒ B): Q[B] = qa match {
        case C1(i) ⇒ C1(i andThen f)
        case C2(x) ⇒ C2(x)
        case C3(q) ⇒ C3(map(q)(f)) // Recursive case.
      }
    }

    // Apply to a value.
    val res: Q[Int] = implicitly[Functor[Q]].map(C3(C2(10)))(identity[Int])
    res shouldEqual C3(C2(10))

    // A comparison function.
    def cEqual[T](c1: Q[T], c2: Q[T]): Assertion = c1 match {
      case C1(i1) ⇒ c2 match {
        case C1(i2) ⇒ forAll { t: Int ⇒ i1(t) shouldEqual i2(t) }
        case _ ⇒ c1 shouldEqual c2
      }
      case C3(q1) ⇒ c2 match {
        case C3(q2) ⇒ cEqual(q1, q2)
        case _ ⇒ c1 shouldEqual c2
      }
      case _ ⇒ c1 shouldEqual c2
    }

    cEqual(res, C3(C2(10)))

    checkCatsFunctorLaws[Q, Int, String, Long](cEqual)
  }

  // * If F and G are functors, define functor instance for F + G
  // Using the "kind projector" plugin, see https://github.com/non/kind-projector
  it should "ex09" in {
    implicit def fgFunctorInstance[F[_], G[_]](implicit evF: Functor[F], evG: Functor[G]): Functor[λ[X ⇒ Either[F[X], G[X]]]] = {
      // Define the new functor as a type here, for convenience.
      type EitherFG[T] = Either[F[T], G[T]]

      new Functor[EitherFG] {
        override def map[A, B](fga: EitherFG[A])(t: A ⇒ B): EitherFG[B] = fga match {
          case Left(fa) ⇒ Left(evF.map(fa)(t))
          case Right(ga) ⇒ Right(evG.map(ga)(t))
        }
      }
    }

    // Create Functor instances for some simple data types.
    type D1[T] = (T, Int)
    type D2[T] = Either[T, String]
    implicit val d1FunctorInstance = new Functor[D1] {
      def map[A, B](fa: (A, Int))(f: A ⇒ B): (B, Int) = implement
    }
    implicit val d2FunctorInstance = new Functor[D2] {
      def map[A, B](fa: Either[A, String])(f: A ⇒ B): Either[B, String] = implement
    }

    // Check functor laws for D1 + D2.
    type D1orD2[T] = Either[D1[T], D2[T]]

    checkCatsFunctorLaws[D1orD2, Double, Boolean, Short]()
  }

  it should "do ex09 in a limited way but without type-level anonymous functions" in {

    def checkEx06[F[_], G[_], A, C](implicit
      evF: Functor[F],
      evG: Functor[G],
      evFGa: Arbitrary[Either[F[A], G[A]]],
      evFGc: Arbitrary[Either[F[C], G[C]]],
      evab: Arbitrary[A ⇒ String],
      evbc: Arbitrary[String ⇒ C]
    ): Assertion = {
      // Define the new functor as a type here.
      type EitherFG[T] = Either[F[T], G[T]]

      implicit val d1ord2Instance = new Functor[EitherFG] {
        override def map[A, B](fga: EitherFG[A])(t: A ⇒ B): EitherFG[B] = fga match {
          case Left(fa) ⇒ Left(evF.map(fa)(t))
          case Right(ga) ⇒ Right(evG.map(ga)(t))
        }
      }
      // Check functor laws for F + G.

      checkCatsFunctorLaws[EitherFG, A, String, C]()
    }

    // Create Functor instances for some simple data types.
    type D1[T] = (T, Int)
    type D2[T] = Either[T, String]
    implicit val d1FunctorInstance = new Functor[D1] {
      override def map[A, B](fa: (A, Int))(f: A ⇒ B): (B, Int) = implement
    }
    implicit val d2FunctorInstance = new Functor[D2] {
      override def map[A, B](fa: Either[A, String])(f: A ⇒ B): Either[B, String] = implement
    }

    checkEx06[D1, D2, Double, Int]
  }

  it should "define Functor instance for a type alias" in {
    type F[T] = Seq[Try[T]]
    implicit val functorF: Functor[F] = new Functor[F] {
      def map[A, B](fa: F[A])(f: A => B): F[B] = fa.map(_.map(f))
    }
    import cats.syntax.functor._
    val s: F[Int] = Seq(Try(1), Try(2), Try(3))
    
    // Have typeclass instance? Yes.
    implicitly[Functor[F]]
    
    implicitly[Functor[Lambda[X ⇒ Seq[Try[X]]]]]
    
    // But the extension method `.map` does not work for `s`.
    "s.map(_ * 2)" shouldNot compile
  }

}
