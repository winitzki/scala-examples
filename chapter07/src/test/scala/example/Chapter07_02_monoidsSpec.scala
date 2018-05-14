package example

import cats.syntax.functor._
import cats.syntax.monoid._
import cats.{Functor, Monad, Monoid, Semigroup, derive}
import org.scalatest.FlatSpec
import Semimonad.SemimonadSyntax

class Chapter07_02_monoidsSpec extends FlatSpec with FlattenableLawChecking with CatsLawChecking {

  behavior of "monoid and semigroup constructions"

  it should "check associativity for left-trivial semigroup" in {
    // Delete the right:
    implicit def semigroupLeft[A]: Semigroup[A] = new Semigroup[A] {
      override def combine(x: A, y: A): A = x
    }

    1 |+| 2 shouldEqual 1
    "abc" |+| "def" shouldEqual "abc"
    // The operation always returns the left-most value. Thus, associativity is assured.
    ('a |+| 'b) |+| 'c shouldEqual 'a
    'a |+| ('b |+| 'c) shouldEqual 'a
  }

  it should "check associativity for right-trivial semigroup" in {
    // Delete the left:
    implicit def semigroupRight[A]: Semigroup[A] = new Semigroup[A] {
      override def combine(x: A, y: A): A = y
    }

    1 |+| 2 shouldEqual 2
    "abc" |+| "def" shouldEqual "def"
    // The operation always returns the right-most value. Thus, associativity is assured.
    ('a |+| 'b) |+| 'c shouldEqual 'c
    'a |+| ('b |+| 'c) shouldEqual 'c
  }

  it should "check monoid laws for function monoid with left composition" in {
    implicit def monoidAA[A]: Monoid[A ⇒ A] = new Monoid[A ⇒ A] {
      override def empty: A ⇒ A = identity

      override def combine(x: A ⇒ A, y: A ⇒ A): A ⇒ A = x andThen y
    }
    // Note: (x andThen y)(a) = y(x(a))
    // Identity laws are obvious.
    // Associativity is easy: `x andThen y andThen z` is associative.
  }

  it should "check monoid laws for function monoid with right composition" in {
    implicit def monoidAA[A]: Monoid[A ⇒ A] = new Monoid[A ⇒ A] {
      override def empty: A ⇒ A = identity

      override def combine(x: A ⇒ A, y: A ⇒ A): A ⇒ A = x compose y
    }
    // Note: (x compose y)(a) = x(y(a)) = (y andThen x)(a)
    // Identity laws are obvious.
    // Associativity is easy: `x compose y compose z` is associative because it is x(y(z(a))).
  }

  it should "check monoid laws for monoid product" in {
    implicit def monoidProduct[S1: Monoid, S2: Monoid]: Monoid[(S1, S2)] = new Monoid[(S1, S2)] {
      override def empty: (S1, S2) = (Monoid.empty[S1], Monoid.empty[S2])

      override def combine(x: (S1, S2), y: (S1, S2)): (S1, S2) = (x._1 |+| y._1, x._2 |+| y._2)
    }

    // The monoid operations are performed separately in each part of the tuple.
    // So, it is clear that each part of the tuple will satisfy the laws separately.
    // Therefore, the entire tuple will also satisfy the laws.
  }

  it should "check semigroup laws for M[S]" in {
    implicit def semigroupMS[M[_] : Semimonad : Functor, S: Semigroup]: Semigroup[M[S]] = new Semigroup[M[S]] {
      override def combine(x: M[S], y: M[S]): M[S] = for {
        s ← x
        t ← y
      } yield s |+| t
    }
    
    /* Associativity: ms1 |+| ms2 |+| ms3 is computed as 
      for {
        s1 ← ms1
        s2 ← ms2
        s3 ← ms3
      } yield s1 |+| s2 |+| s3
      So, associativity of M[S] follows directly from the semimonad's associativity law and the semigroup's associativity law.    
     */
  }

  // When M is a full monad, we can use a monoid for S. Proof of identity laws is in Exercise 1.
  // As an example, consider the Reader monad as M.
  
  it should "check monoid laws for reader monoid" in {
    implicit def monoidReader[Z, S: Monoid]: Monoid[Z ⇒ S] = new Monoid[Z ⇒ S] {
      override def empty: Z ⇒ S = { _ ⇒ Monoid.empty[S] }

      override def combine(x: Z ⇒ S, y: Z ⇒ S): Z ⇒ S = { z ⇒ x(z) |+| y(z) }
    }

    // The monoid operations are performed in the monoid (semigroup) S after applying the function to some z: Z.
    // Therefore, the monoid laws will hold separately for each fixed value of z. Hence they always hold.
  }


  it should "verify that right-biased Either of monoids is a monoid" in {
    // The monoid operation on `Either[A, B]` works by the following rules:
    // - If all operands are `Left(A)`, combine their values into another `Left(A)`.
    // - If even one operand is a `Right(B)`, discard all `Left()` operands and combine all remaining `Right(B)` operands into one `Right(B)`.

    // This is associative, and respects identity laws if the `empty` element is a `Left()`.

    implicit def monoidRightEither[A: Monoid, B: Monoid]: Monoid[Either[A, B]] = new Monoid[Either[A, B]] {
      override def empty: Either[A, B] = Left(Monoid.empty[A]) // We need to choose one! Let's choose `A`.

      override def combine(p: Either[A, B], q: Either[A, B]): Either[A, B] = (p, q) match {
        case (Left(x), Left(y)) ⇒ Left(x |+| y) // This is the only way to combine `x` and `y`. No other choice here.
        case (Right(x), Right(y)) ⇒ Right(x |+| y) // No other choice here. If we omit `x` or `y`, we'd violate identity laws.
        case (Left(x), Right(y)) ⇒ Right(y) // If x = empty[A] then we must return `y` here. No other choice.
        case (Right(x), Left(y)) ⇒ Right(x) // If y = empty[A] then we must return `x` here. No other choice.
      }
    }

    implicit val monoidInt: Monoid[Int] = new Monoid[Int] {
      override def empty: Int = 0

      override def combine(x: Int, y: Int): Int = x + y
    }
    {
      val q1: Either[Int, Int] = Left(1)
      val q2: Either[Int, Int] = Right(2)
      val q3: Either[Int, Int] = Left(3)
      (q1 |+| q2) |+| q3 shouldEqual Right(2)
      q1 |+| (q2 |+| q3) shouldEqual Right(2)
    }
    {
      val q1: Either[Int, Int] = Right(1)
      val q2: Either[Int, Int] = Right(2)
      val q3: Either[Int, Int] = Left(3)
      (q1 |+| q2) |+| q3 shouldEqual Right(3)
      (q2 |+| q3) shouldEqual Right(2)
      q1 |+| (q2 |+| q3) shouldEqual Right(3)
    }
    {
      val q1: Either[Int, Int] = Right(1)
      val q2: Either[Int, Int] = Left(2)
      val q3: Either[Int, Int] = Right(3)
      (q1 |+| q2) |+| q3 shouldEqual Right(4)
      (q2 |+| q3) shouldEqual Right(3)
      q1 |+| (q2 |+| q3) shouldEqual Right(4)
    }
  }

  it should "check semigroup laws for a right-twisted product" in {
    def semigroupRightTwisted[S: Semigroup, P](action: S ⇒ P ⇒ P): Semigroup[(S, P)] = new Semigroup[(S, P)] {
      override def combine(x: (S, P), y: (S, P)): (S, P) = (x._1 |+| y._1, action(x._1)(y._2))
    }

    // Verify the semigroup law (associativity):
    // (s1, p1) |+| (s2, p2)  = (s1 |+| s2, a(s1)(p2)).
    // The first part of the tuple is the semigroup S, which we assume satisfies the law.
    // The second part of the tuple for ((s1, p1) |+| (s2, p2)) |+| (s3, p3) is a(s1 |+| s2)(p3).
    // The second part of the tuple for (s1, p1) |+| ((s2, p2) |+| (s3, p3)) is a(s1)(a(s2)(p3)).
    // They are the same as long as a(s2) andThen a(s1) is the same as a(s1 |+| s2). 

    // However, a monoid is impossible since we are losing information about `x._2` in `combine`.

    // Example: S = Boolean, P = Option[A].
    implicit val semigroupBoolean: Semigroup[Boolean] = new Semigroup[Boolean] {
      override def combine(x: Boolean, y: Boolean): Boolean = x && y
    }

    type Q = (Boolean, Option[Int])

    implicit val semigroupQ = semigroupRightTwisted[Boolean, Option[Int]](b ⇒ oi ⇒ oi.filter(_ ⇒ b))

    val q1: Q = (true, Some(1))
    val q2: Q = (false, Some(2))
    val q3: Q = (true, Some(3))

    (q1 |+| q2) shouldEqual ((false, Some(2)))
    (q1 |+| q2) |+| q3 shouldEqual ((false, None))

    q2 |+| q3 shouldEqual ((false, None))
    q1 |+| (q2 |+| q3) shouldEqual ((false, None))
  }
}
