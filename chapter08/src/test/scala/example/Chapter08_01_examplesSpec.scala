package example

import org.scalatest.{FlatSpec, Matchers}

class Chapter08_01_examplesSpec extends FlatSpec with Matchers {

  behavior of "examples"

  it should "implement map2 and map3 for Either" in {
    type Op[A] = Either[String, A]

    implicit def pure[A](x: A): Op[A] = Right(x)

    def safeDivide(x: Double, y: Double): Op[Double] = if (y == 0.0)
      Left(s"Error: dividing $x by 0\n")
    else x / y

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
      val q: Op[(A, B)] = map2(a, b) { (x, y) ⇒ (x, y) }
      map2(q, c) { case ((aa, bb), cc) ⇒ f(aa, bb, cc) }
    }
    // This is certainly better but still awkward to generalize.
  }
}
