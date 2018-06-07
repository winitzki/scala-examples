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
    // This is certainly better but still awkward to generalize.

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

  it should "define mapN for Either" in {
    type Op[A] = Either[String, A]

    def map2[A, B, Z](a: Op[A], b: Op[B])(f: (A, B) ⇒ Z): Op[Z] = (a, b) match {
      case (Left(s1), Left(s2)) ⇒ Left(s1 + s2) // Concatenate the two error messages.
      case (Left(s), Right(_)) ⇒ Left(s)
      case (Right(_), Left(s)) ⇒ Left(s)
      case (Right(x1), Right(x2)) ⇒ Right(f(x1, x2))
    }

    // Generalize to mapN in two ways:
    // 1. Use a list of arguments.
    // 2. Use curried arguments in some clever way.

    // 1. Recursively deconstruct a list of arguments.
    def mapN1[A](xs: List[Op[A]]): Op[List[A]] = xs match {
      case Nil ⇒ Right(Nil)
      case head :: tl ⇒ map2(head, mapN1(tl)) { (x, t) ⇒ x :: t }
    }

    // 2. Define a curried version of `map2`, called `fmap2`:
    /*
        def fmap2[A, B, Z](f: A ⇒ B ⇒ Z): Op[A] ⇒ Op[B] ⇒ Op[Z] = {
          opa ⇒
            opb ⇒
              map2(opa, opb) { (x, y) ⇒ f(x)(y) }
        }
  
 Why can't the usual `fmap` work this way? Here's the definition of `fmap` for `Op`:
    */
    def fmap[A, B](f: A ⇒ B): Op[A] ⇒ Op[B] = _.map(f) // Just use .map() on Either.

    // For convenience, define an infix syntax for `fmap`:
    implicit class FmapSyntax[A, B](val f: A ⇒ B) {
      def <@>[F[_] : Functor](fa: F[A]): F[B] = fa.map(f)
    }

    import cats.instances.either._ // Enable the functor instance for Either.

    /* 
  Now, if we apply `fmap` to `f: A ⇒ (B ⇒ Z)` then we get `fmap(f): Op[A] ⇒ Op[B ⇒ Z].
  But we want Op[A] ⇒ (Op[B] ⇒ Op[Z]) instead.
  This can be obtained if we could somehow transform Op[B ⇒ Z] into Op[B] ⇒ Op[Z].
  This function is usually called `ap`: 
    */
    def ap[A, B]: Op[A ⇒ B] ⇒ Op[A] ⇒ Op[B] = {
      opab ⇒
        opa ⇒
          (opab, opa) match {
            case (Left(s1), Left(s2)) ⇒ Left(s1 + s2) // Concatenate the two error messages.
            case (Left(s), Right(_)) ⇒ Left(s)
            case (Right(_), Left(s)) ⇒ Left(s)
            case (Right(x1), Right(x2)) ⇒ Right(x1(x2))
          }
    }

    // For convenience, define an infix syntax for `ap`:
    implicit class ApSyntax[A, B](val opab: Op[A ⇒ B]) {
      def <*>(opa: Op[A]): Op[B] = ap(opab)(opa)
    }

    // Define fmap2 through ap and fmap:
    def fmap2[A, B, Z](f: A ⇒ B ⇒ Z): Op[A] ⇒ Op[B] ⇒ Op[Z] = {
      opa ⇒
        opb ⇒
          // Here we need to return an Op[Z].
          //    val x: Op[B ⇒ Z] = fmap(f)(opa)
          // Use `ap` on that x and obtain Op[B] ⇒ Op[Z].
          // So then `ap(x)(opb)` will be of type Op[Z] as required.
          // Use the infix syntax  to write `f <@> opa` instead of `fmap(f)(opa)`
          // and `x <*> opb` instead of `ap(x)(opb)`.
          //
          // Note that <@> and <*> associate to the left, so we can simply write this:
          f <@> opa <*> opb // This syntax now looks similar to `f (opa) (opb)` with special separators.
    }

    def fmap3[A, B, C, Z](f: A ⇒ B ⇒ C ⇒ Z): Op[A] ⇒ Op[B] ⇒ Op[C] ⇒ Op[Z] = {
      opa ⇒ opb ⇒ opc ⇒ f <@> opa <*> opb <*> opc
    }

    def fmap4[A, B, C, D, Z](f: A ⇒ B ⇒ C ⇒ D ⇒ Z): Op[A] ⇒ Op[B] ⇒ Op[C] ⇒ Op[D] ⇒ Op[Z] = {
      opa ⇒ opb ⇒ opc ⇒ opd ⇒ f <@> opa <*> opb <*> opc <*> opd
    }

    // Instead of calling `mapN`, we could just use <@> and <*> directly:
    def safeDivide(x: Double, y: Double): Op[Double] = if (y == 0.0)
      Left(s"Error: dividing $x by 0\n")
    else Right(x / y)

    val f: Double ⇒ Double ⇒ Double = { x ⇒ y ⇒ x + y }

    val res: Op[Double] = f <@> safeDivide(2, 1) <*> safeDivide(4, 2)

    res shouldEqual Right(4.0)

    // Create a validated case class.
    case class C2(x: Double, y: Double)

    val result: Op[C2] = (C2.apply _).curried <@> safeDivide(2, 1) <*> safeDivide(4, 2)
    result shouldEqual Right(C2(2.0, 2.0))
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
    def mapN[A](fa: List[Future[A]]): Future[List[A]] = Future.sequence(fa)
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

    // Changing the order of computations gives the same result.
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
            // Special case: transposing a single row, heads :: List().
            // Need to produce a list of single-element lists.
            heads.map(_ ⇒ Nil)
          case _ ⇒ transpose(tails)
        }
        map2(heads, transposedTails) { (x, xs) ⇒ x :: xs };
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

}
