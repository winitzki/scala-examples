package example

import io.chymyst.ch._
import org.scalacheck.Arbitrary
import org.scalacheck.ScalacheckShapeless._

class Chapter04_02_workedExamplesSpec extends LawChecking {


  // First functor
  type F1[A] = (A, Int)

  val fmap1: FMap[F1] = new FMap[F1] {
    override def code[A, B]: (A => B) => ((A, Int)) => (B, Int) = implement
  }

  def f2Equal[A](value1: F2[A], value2: F2[A]) = value1 match {
    case Left(intToA1) => value2 match {
      case Left(intToA2) => forAll { (x: Int) ⇒ intToA1(x) shouldEqual intToA2(x) }
      case Right(_) => fail
    }
    case Right(a1) => value2 match {
      case Left(_) => fail
      case Right(a2) => a1 shouldEqual a2
    }
  }

  // Second functor
  type F2[A] = Either[Int ⇒ A, A]

  val fmap2: FMap[F2] = new FMap[F2] {
    override def code[A, B]: (A => B) => Either[Int ⇒ A, A] => Either[Int ⇒ B, B] = implement
  }


  behavior of "proving laws for functor combinations"

  it should "create a disjunction of functors" in {
    // Functor Data is the disjunction of F1 and F2.
    type Data[A] = Either[F1[A], F2[A]]

    val fmap: FMap[Data] = new FMap[Data] {
      override def code[A, B]: (A => B) => Data[A] => Data[B] = f ⇒ {
        case Left(f1a) ⇒ Left(fmap1.code(f)(f1a))
        case Right(f2a) ⇒ Right(fmap2.code(f)(f2a))
      }
    }

    def dataEqual[A](d1: Data[A], d2: Data[A]) = d1 match {
      case Left(value1) => d2 match {
        case Left(value2) => value1 shouldEqual value2
        case Right(_) => fail
      }
      case Right(value1) => d2 match {
        case Left(_) => fail
        case Right(value2) => f2Equal(value1, value2)
      }
    }

    // Identity law.
    forAll { (x: Data[Double]) ⇒ dataEqual(x, fmap.code(identity[Double])(x)) }

    // Composition law.
    forAll { (x: Data[Double], f: Double ⇒ String, g: String ⇒ Long) ⇒
      dataEqual(
        fmap.code(f andThen g)(x),
        (fmap.code(f) andThen fmap.code(g)) (x)
      )
    }

  }

  // First contrafunctor
  type CF1[A] = (A ⇒ Int)

  val contrafmap1: ContraFMap[CF1] = new ContraFMap[CF1] {
    override def code[A, B]: (B => A) => (A ⇒ Int) => (B ⇒ Int) = implement
  }

  def cf1Equal[A: Arbitrary](value1: CF1[A], value2: CF1[A]) = forAll { (x: A) ⇒ value1(x) shouldEqual value2(x) }


  it should "create an implication from contrafunctor to functor" in {

    type Data[A] = CF1[A] ⇒ F2[A]

    def dataEqual[A: Arbitrary](value1: Data[A], value2: Data[A])(implicit cf1: Arbitrary[CF1[A]]) = {
      forAll { (cf1: CF1[A]) ⇒
        f2Equal(value1(cf1), value2(cf1))
      }
    }

    val fmap: FMap[Data] = new FMap[Data] {
      override def code[A, B]: (A => B) => (Data[A]) => Data[B] = {
        (f: A ⇒ B) ⇒
          (da: CF1[A] ⇒ F2[A]) ⇒
            (cf1b: CF1[B]) ⇒
              // Need to return a value of type F2[B] = Either[Int ⇒ B, B]
              fmap2.code(f)(da(contrafmap1.code(f)(cf1b)))
      }
    }

    // Identity law.
    forAll { (x: Data[Double]) ⇒ dataEqual(x, fmap.code(identity[Double])(x)) }

    // Composition law.
    forAll { (x: Data[Double], f: Double ⇒ String, g: String ⇒ Long) ⇒
      dataEqual(
        fmap.code(f andThen g)(x),
        (fmap.code(f) andThen fmap.code(g)) (x)
      )
    }
  }

  it should "create a functor by applying type recursion" in {
    // Bifunctor R[A, T] = A + A × T

    type R[A, T] = Either[A, (A, T)] // Taking recursion w.r.t. T will generates the non-empty list functor.

    // Recursive functor Data[A] = A + A × Data[A] = R[A, Data[A]]
    case class Data[A](r: R[A, Data[A]])

    def dataEqual[A](value: Data[A], value1: Data[A]) = value shouldEqual value1

    val fmap: FMap[Data] = new FMap[Data] {
      // recursive definition
      override def code[A, B]: (A => B) => Data[A] => Data[B] = {
        f ⇒
          f3a ⇒
          f3a.r match {
            case Left(xa) ⇒ Data(Left(f(xa)))
            case Right((ya, yf3a)) ⇒ Data(Right((f(ya), code(f)(yf3a))))
          }
      }
    }

    // Identity law.
    forAll { (x: Data[Double]) ⇒ dataEqual(x, fmap.code(identity[Double])(x)) }

    // Composition law.
    forAll { (x: Data[Double], f: Double ⇒ String, g: String ⇒ Long) ⇒
      dataEqual(
        fmap.code(f andThen g)(x),
        (fmap.code(f) andThen fmap.code(g)) (x)
      )
    }
  }
}
