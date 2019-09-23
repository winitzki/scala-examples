package example

import org.scalatest.{FlatSpec, Matchers}

class RecursiveMonoidTrait extends FlatSpec with Matchers {

  behavior of "recursive monoid"

  it should "define a recursive MonoidT instance and run a computation" in {
    import MonoidT._

    implicit class MonoidTOps[T: MonoidT](t: T) {
      def |+|(a: T): T = implicitly[MonoidT[T]].combine(t, a)
    }

    1 |+| 1 shouldEqual 1 + 1

    // Structural combinators.

    def monoidPair[A: MonoidT, B: MonoidT]: MonoidT[(A, B)] = new MonoidT[(A, B)] {
      override def empty: (A, B) = (implicitly[MonoidT[A]].empty, implicitly[MonoidT[B]].empty)

      override def combine: ((A, B), (A, B)) ⇒ (A, B) = {
        case ((a1, b1), (a2, b2)) ⇒ (a1 |+| a2, b1 |+| b2)
      }
    }

    def monoidEitherPreferB[A: MonoidT, B: MonoidT] = new MonoidT[Either[A, B]] {
      override def empty: Either[A, B] = Left(implicitly[MonoidT[A]].empty)

      override def combine: (Either[A, B], Either[A, B]) ⇒ Either[A, B] = {
        case (Left(a1), Left(a2)) ⇒ Left(a1 |+| a2)
        case (Left(a), Right(b)) ⇒ Right(b) // "Take B".
        case (Right(b), Left(a)) ⇒ Right(b)
        case (Right(b1), Right(b2)) ⇒ Right(b1 |+| b2)
      }
    }

    def monoidFunc[A: MonoidT, E] = new MonoidT[E ⇒ A] {
      override def empty: E ⇒ A = _ ⇒ implicitly[MonoidT[A]].empty

      override def combine: (E ⇒ A, E ⇒ A) ⇒ E ⇒ A = {
        case (f, g) ⇒ e ⇒ f(e) |+| g(e)
      }
    }

    type S[A] = Either[(Either[Int, A], Int), (String, A ⇒ (A ⇒ Int) ⇒ A)]

    def monoidS[A](implicit ti: MonoidT[A]): MonoidT[S[A]] = {
      implicit val m0 = monoidEitherPreferB[Int, A]
      implicit val m1 = monoidPair[Either[Int, A], Int]
      implicit val m2 = monoidFunc[A, A => Int]
      implicit val m3 = monoidFunc[(A => Int) => A, A]
      implicit val m4 = monoidPair[String, A => (A => Int) => A]
      monoidEitherPreferB[(Either[Int, A], Int), (String, A => (A => Int) => A)]
    }

    final case class T(s: S[T])

    implicit def monoidT: MonoidT[T] = new MonoidT[T] {
      override def empty: T = T(monoidS[T](monoidT).empty) // Will cause StackOverflowError if defined as a `val` here.

      override def combine: (T, T) ⇒ T = (x, y) ⇒ T(monoidS[T](monoidT).combine(x.s, y.s))
    }

    val t = T(Right(("a", t => f => T(Left((Left(f(t)), 10))))))

    (t |+| t).s.right.get._1 shouldEqual "aa"
    val e = implicitly[MonoidT[T]].empty
    t |+| e shouldEqual t
  }
  
  
}

trait MonoidT[A] {
  def empty: A

  def combine: (A, A) ⇒ A
}

object MonoidT {
  implicit val monoidTInt: MonoidT[Int] = new MonoidT[Int] {
    override def empty: Int = 0

    override def combine: (Int, Int) ⇒ Int = _ + _
  }
  implicit val monoidTString: MonoidT[String] = new MonoidT[String] {
    override def empty: String = ""

    override def combine: (String, String) ⇒ String = _ + _
  }
}
