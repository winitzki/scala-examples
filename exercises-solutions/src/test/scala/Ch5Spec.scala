package swscala.unit

import scala.concurrent.{Await, Future}
//import scala.concurrent.duration._
import org.scalacheck.Arbitrary
import org.scalacheck.ScalacheckShapeless._
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures
import cats.{Contravariant, Functor, Monoid, Semigroup}
import io.chymyst.ch._
import swscala._

import example.CatsLawChecking

class Ch5Spec extends FlatSpec with Matchers with CatsLawChecking with ScalaFutures {
  import Ch5._

  it should "Problem 1" in {
    import Problem1._

    isLong[Long] shouldEqual true
    isLong[Double] shouldEqual true
    isLong[Int] shouldEqual false
    isLong[Short] shouldEqual false
    isLong[Float] shouldEqual false
    "isLong[String]" shouldNot compile
  }

  it should "Problem 2" in {
    import Problem2._

    // Check the laws
    checkCatsMonoidLaws[Data]()(implicitly[Arbitrary[Data]], monoidInstance)
  }

  it should "Problem 3" in {
    import Problem3._

    // Int monoid
    implicit val monoidIntInstance = new Monoid[Int] {
      override def empty = 0
      override def combine(x: Int, y: Int) = x + y
    }

    def dataIsEqual[R, A](d1: R => A, d2: R => A)(implicit arbR: Arbitrary[R]): Assertion = {
      forAll { (r: R) => d1(r) shouldEqual d2(r) }
    }

    // Check monoid laws for R => A
    checkCatsMonoidLaws[Boolean => Int](dataIsEqual[Boolean, Int])
  }

  it should "Problem 4" in {
    import Problem4._

    // Create instance of semigroup S.
    // Taken from https://github.com/winitzki/scala-examples/blob/master/chapter05/src/test/scala/example/Chapter05_03_workedExamplesSpec.scala#L138
    implicit val semigroupIntInstanceNonCommutAssoc = new Semigroup[Int] {
      override def combine(x: Int, y: Int): Int = nonCommutAssoc(x, y)

      // Budden's function: see F. J. Budden, A Non-Commutative, Associative Operation on the Reals.
      //   The Mathematical Gazette, Vol. 54, No. 390 (Dec., 1970), pp. 368-372
      private def nonCommutAssoc(x: Int, y: Int): Int =
        if (x % 2 == 0) x + y else x - y
    }

    // Should be able to derive monoid instance for (Int, Double) now
    checkCatsMonoidLaws[Option[Int]]()
  }

  // it should "Problem 5" in {
  //   import Problem5._
  //   import scala.concurrent.ExecutionContext.Implicits.global

  //   def dataIsEqual[T](f1: F[T], f2: F[T]): Assertion = {
  //     //f1.futureValue shouldEqual f2.futureValue
  //     //Await.result(f1, 1 second) shouldEqual Await.result(f2, 1 second)

  //     // val foo = {for {
  //     //   r1 <- f1
  //     //   r2 <- f2
  //     // } yield r1 shouldEqual r2}
  //     // Await.result(foo, 1 second)

  //     whenReady(f1) { r1 =>
  //       whenReady(f2) { r2 =>
  //         r1 shouldEqual r2
  //       }
  //     }

  //     // import util._
  //     // f1.onComplete {
  //     //   case Success(r1) => f2.onComplete {
  //     //     case Success(r2) => r1 shouldEqual r2
  //     //     case _ => fail
  //     //   }
  //     //   case _ => fail
  //     // }
  //     //type F[T] = Future[Seq[T]]
  //   }

  //   checkCatsFunctorLaws[F, Int, String, Boolean](dataIsEqual)
  // }

  it should "Problem 6" in {
    import Problem6._

    def dataIsEqual[X, Y](x: B[X, Y], y: B[X, Y]): Assertion = x match {
      case Left(iToX1) => y match {
        case Left(iToX2) => forAll { (i: Int) => iToX1(i) shouldEqual iToX2(i) }
        case _ => fail
      }
      case Right(bb1) => y match {
        case Right(bb2) => bb1 shouldEqual bb2
        case _ => fail
      }
    }

    checkCatsBifunctorLaws[B, Int, String, Boolean, Char, Long, Double](dataIsEqual)
  }

  // it should "Problem 7" in {
  //   import Problem7._
  //   Unit
  // }

  it should "Problem 8" in {
    import Problem8._

    def qEqual[T](q1: Q[T], q2: Q[T]): Assertion = q1 match {
      case C1(s1) => q2 match {
        case C1(s2) => s1 shouldEqual s2
        case _ => fail
      }
      case C2(a1, qq1) => q2 match {
        case C2(a2, qq2) => {
          a1 shouldEqual a2
          qEqual(qq1, qq2)
        }
        case _ => fail
      }
    }

    checkCatsFunctorLaws[Q, Int, String, Long](qEqual)
  }

  it should "Problem 9" in {
    import Problem9._

    // Create Functor instances for some simple data types.
    type D1[T] = (T, Int)
    type D2[T] = Either[T, String]
    implicit val d1FunctorInstance = new Functor[D1] {
      override def map[A, B](fa: (A, Int))(f: A ⇒ B): (B, Int) = implement
    }
    implicit val d2FunctorInstance = new Functor[D2] {
      override def map[A, B](fa: Either[A, String])(f: A ⇒ B): Either[B, String] = implement
    }

    // Check functor laws for D1 x D2.
    type D1AndD2[T] = (D1[T], D2[T])
    checkCatsFunctorLaws[D1AndD2, Double, Boolean, Short]()
  }

  it should "Problem 10" in {
    import Problem10._

    // Create contrafunctor instance
    type D1[T] = T => Int
    implicit val contraFunctorInstance = new Contravariant[D1] {
      override def contramap[A, B](fa: A ⇒ Int)(f: B => A): B ⇒ Int = implement
    }
    // Create functor instance
    type D2[T] = Int => T
    implicit val functorInstance = new Functor[D2] {
      override def map[A, B](da: D2[A])(f: A => B): D2[B] = implement
    }

    // Check functor laws for FtoG
    type D1toD2[T] = D1[T] => D2[T]

    def dataIsEqual[T](x: D1toD2[T], y: D1toD2[T])(
      implicit arbD1: Arbitrary[D1[T]]
    ): Assertion = {
      forAll { (d1: D1[T], i: Int) =>
        x(d1)(i) shouldEqual y(d1)(i)
      }
    }

    checkCatsFunctorLaws[D1toD2, Double, Boolean, Short](dataIsEqual)
  }

}
