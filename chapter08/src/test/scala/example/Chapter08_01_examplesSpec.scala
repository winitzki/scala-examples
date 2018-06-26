package example

import cats.Functor
import cats.syntax.functor._
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration.Duration

class Chapter08_01_examplesSpec extends FlatSpec with Matchers {

  behavior of "examples"

  it should "implement map2 and map3 for Either" in {
    type Op[A] = Either[String, A]

    def safeDivide(x: Double, y: Double): Op[Double] = if (y == 0.0)
      Left(s"Error: dividing $x by 0\n")
    else Right(x / y)

    // Want to perform operations and collect all errors.
    def map2[A, B, Z](a: Op[A], b: Op[B])(f: (A, B) ⇒ Z): Op[Z] = (a, b) match {
      case (Left(s1), Left(s2)) ⇒ Left(s1 + s2) // Concatenate the two error messages.
      case (Left(s), Right(_)) ⇒ Left(s)
      case (Right(_), Left(s)) ⇒ Left(s)
      case (Right(x1), Right(x2)) ⇒ Right(f(x1, x2))
    }

    // We can now collect all error messages.
    map2(
      safeDivide(1, 0),
      safeDivide(2, 0)
    ) { (x, y) ⇒ x - y } shouldEqual Left("Error: dividing 1.0 by 0\nError: dividing 2.0 by 0\n")

    // Note that this definition of `map2` is not equivalent to the monadic definition:
    (for {
      x ← safeDivide(1, 0)
      y ← safeDivide(2, 0)
    } yield x - y) shouldEqual Left("Error: dividing 1.0 by 0\n")

    // Now let's define map3:
    def map3[A, B, C, Z](a: Op[A], b: Op[B], c: Op[C])(f: (A, B, C) ⇒ Z): Op[Z] = {
      // We would like to avoid listing 8 possible cases now.
      // Let's begin by applying map2() to (a, b).
      val opab: Op[(A, B)] = map2(a, b) { (x, y) ⇒ (x, y) } // Almost an identity function here...
      // Now we can use map2 again on opab and c: 
      map2(opab, c) { case ((aa, bb), cc) ⇒ f(aa, bb, cc) }
    }
    // This is still awkward to generalize.

    map3(
      safeDivide(1, 0),
      safeDivide(2, 0),
      safeDivide(3, 1)
    ) { (x, y, z) ⇒ x - y } shouldEqual Left("Error: dividing 1.0 by 0\nError: dividing 2.0 by 0\n")

    // Create a case class with validated values:
    case class C(x: Double, y: Double, z: Double)

    val result = map3(
      safeDivide(10, 5),
      safeDivide(20, 5),
      safeDivide(30, 5)
    )(C.apply)

    result shouldEqual Right(C(2, 4, 6))
  }

  it should "apply a function to the results of Future" in {
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent._

    // Define map2 for Future by hand.
    def map2[A, B, Z](fa: Future[A], fb: Future[B])(f: (A, B) ⇒ Z): Future[Z] = for {
      x ← fa // These `Future`s were already started, so the execution is not really sequential.
      y ← fb
    } yield f(x, y)

    val resultFuture = map2(
      Future(1 + 2),
      Future(3 + 4)
    )(_ + _)

    Await.result(resultFuture, Duration.Inf) shouldEqual 10

    // scala.concurrent.Future already defines Future.sequence(), so let's use that.
    def mapN[A, B](fa: List[Future[A]])(f: List[A] ⇒ B): Future[B] = Future.sequence(fa).map(f)
  }

  it should "use map2 with reader monad" in {
    // Reader monad always has independent and commutative effects.

    // Example: computations that use a logger.
    case class Logger(print: Any ⇒ Unit)

    val emptyLogger = Logger { _ ⇒ () }

    type Lo[A] = Logger ⇒ A
    import cats.Monad
    import cats.instances.function._
    import cats.syntax.flatMap._
    import cats.syntax.functor._

    implicitly[Monad[Lo]]

    def logPlus(x: Int, y: Int): Lo[Int] = { logger ⇒
      val z = x + y
      logger.print(s"Got $z")
      z
    }

    // Combine computations monadically.
    val res1 = for {
      x ← logPlus(1, 2)
      y ← logPlus(10, 20)
    } yield x + y

    // Changing the order of computations gives the same result (except for the side effect of printing!).
    val res2 = for {
      y ← logPlus(10, 20)
      x ← logPlus(1, 2)
    } yield x + y

    res1(emptyLogger) shouldEqual res2(emptyLogger)

    // So we can define `map2` via `flatMap` and get the same results as when we define map2 as:
    def map2[A, B, Z](a: Lo[A], b: Lo[B])(f: (A, B) ⇒ Z): Lo[Z] = logger ⇒ f(a(logger), b(logger))

    def map2A[A, B, Z](a: Lo[A], b: Lo[B])(f: (A, B) ⇒ Z): Lo[Z] = for {
      x ← a
      y ← b
    } yield f(x, y)

    // Let us verify that the code of map2A() comes out to be identical to map2().
    def map[A, B](fa: Lo[A])(f: A ⇒ B): Lo[B] = logger ⇒ f(fa(logger))

    def flatMap[A, B](fa: Lo[A])(f: A ⇒ Lo[B]): Lo[B] = logger ⇒ f(fa(logger))(logger)
    /* Now we can symbolically calculate
    map2A(a, b)(f) = a.flatMap(x ⇒ b.map(y ⇒ f(x, y)) = ?
    
    First consider `b.map(y ⇒ f(x, y)`.
    This is logger ⇒ (y ⇒ f(x, y))(b(logger))
    = logger ⇒ f(x, fa(logger)).
    Now consider a.flatMap(x ⇒ b.map(y ⇒ f(x, y))
    = logger ⇒ (x ⇒ logger ⇒ f(x, b(logger)))(a(logger))(logger)
    = logger ⇒ f(a(logger), b(logger)).
    
    This code is the same as for map2(a, b)(f).
    */
  }

  it should "transpose matrices using mapN" in {
    // Define map2 for List.
    def map2[A, B, Z](a: List[A], b: List[B])(f: (A, B) ⇒ Z): List[Z] = (a zip b).map { case (x, y) ⇒ f(x, y) }

    map2(List(1, 2), List(10, 20))(_ + _) shouldEqual List(11, 22)

    def transpose[A](m: List[List[A]]): List[List[A]] = m match {
      case Nil ⇒ Nil
      case heads :: tails ⇒
        val transposedTails = tails match {
          case Nil ⇒
            // Special case: no tails, transposing a single row, heads :: List().
            // Need to produce a list of single-element lists.
            heads.map(_ ⇒ Nil)
          // General case: transposing tails recursively.
          case _ ⇒ transpose(tails)
        }
        map2(heads, transposedTails) { (x, xs) ⇒ x :: xs }
    }

    transpose(List(List(1))) shouldEqual List(List(1))
    transpose(List(List(1), List(2))) shouldEqual List(List(1, 2))
    transpose(List(List(1, 2))) shouldEqual List(List(1), List(2))

    val matrix = List(
      List(1, 10),
      List(2, 20),
      List(3, 30)
    )

    val matrixT = List(
      List(1, 2, 3),
      List(10, 20, 30)
    )
    transpose(matrix) shouldEqual matrixT
    transpose(matrixT) shouldEqual matrix
  }

  behavior of "zippable profunctors"

  it should "define Semigroup for a pair" in {

    // Semigroup type class.
    trait Semigroup[S] {
      def combine(x: S, y: S): S
    }

    // Syntax.
    implicit class SemigroupSyntax[A: Semigroup](x: A) {
      def |+|(y: A): A = implicitly[Semigroup[A]].combine(x, y)
    }

    // As a data type, this is S × S ⇒ S. This is not a functor and not a contrafunctor in S.
    // This is a profunctor (has S in both covariant and contravariant positions).

    // We can define `zip` for it.
    def zip[A, B](p: Semigroup[A], q: Semigroup[B]): Semigroup[(A, B)] = new Semigroup[(A, B)] {
      override def combine(x: (A, B), y: (A, B)): (A, B) =
        (p.combine(x._1, y._1), q.combine(x._2, y._2))
    }

    // Use this to define semigroup for pairs.
    implicit def pairSemigroup[A: Semigroup, B: Semigroup]: Semigroup[(A, B)] = new Semigroup[(A, B)] {
      override def combine(x: (A, B), y: (A, B)): (A, B) = (x._1 |+| y._1, x._2 |+| y._2)
    }

    // Check that this works.
    // Semigroup instances for Int and for Double.
    implicit val semigroupInt: Semigroup[Int] = _ * _
    implicit val semigroupDouble: Semigroup[Double] = _ + _

    val pair1: (Int, Double) = (10, 1.0)
    val pair2: (Int, Double) = (20, 2.0)

    // Semigroup syntax should work now for pairs.
    pair1 |+| pair2 shouldEqual ((200, 3.0))
  }

  it should "implement imap2 for Z × A ⇒ A × A" in {
    type F[A, Z] = (Z, A) ⇒ (A, A)

    def imap2[Z, A, B, C](f: (A, B) ⇒ C)(g: C ⇒ (A, B))(faz: F[A, Z], fbz: F[B, Z]): F[C, Z] = { (z, c) ⇒
      // Need to return a tuple (C, C).
      val (newA, newB) = g(c) // Need to back-transform C into (A, B) and substitute them into faz and fbz.

      val newAA = faz(z, newA)
      val newBB = fbz(z, newB)
      // Now apply f to all this data.
      (f(newAA._1, newBB._1), f(newAA._2, newBB._2))
    }

    def zip[A, B, Z](faz: F[A, Z], fbz: F[B, Z]): F[(A, B), Z] = {
      case (z, (a, b)) ⇒
        // Need to return ((A, B), (A, B)).
        val newAA = faz(z, a)
        val newBB = fbz(z, b)
        ((newAA._1, newBB._1), (newAA._2, newBB._2))
    }
  }

}
