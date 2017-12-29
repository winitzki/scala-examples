package example

import org.scalacheck.Arbitrary
import org.scalatest.Failed
import org.scalacheck.ScalacheckShapeless._

class Chapter04_01_workedExamplesSpec extends LawChecking {

  behavior of "worked examples"

  it should "ex01-1" in {
    // Data[A] ≡ String + A × Int + A × A × A
    sealed trait Data[A]
    final case class Message[A](message: String) extends Data[A]
    final case class Have1[A](x: A, n: Int) extends Data[A]
    final case class Have3[A](x: A, y: A, z: A) extends Data[A]

    def fmap[A, B](f: A ⇒ B): Data[A] ⇒ Data[B] = {
      case Message(message) => Message[B](message)
      case Have1(x, n) ⇒ Have1(f(x), n)
      case Have3(x, y, z) ⇒ Have3(f(x), f(y), f(z))
    }

    // Identity law.
    forAll { (x: Data[Int]) ⇒ fmap(identity[Int])(x) shouldEqual x }

    // Composition law.
    forAll { (x: Data[Int], f: Int ⇒ String, g: String ⇒ Long) ⇒ fmap(f andThen g)(x) shouldEqual (fmap(f) andThen fmap(g)) (x) }
  }

  it should "ex01-2" in {
    // Data[A] ≡ 1 + A × (Int × String + A)
    final case class Data[A](d: Option[(A, Data2[A])]) //  1 + A × Data2[A]

    sealed trait Data2[A]
    final case class Message[A](code: Int, message: String) extends Data2[A]
    final case class Value[A](x: A) extends Data2[A]

    def fmap[A, B](f: A ⇒ B): Data[A] ⇒ Data[B] = {
      case Data(Some((valueA, data2))) ⇒
        val newValueA = f(valueA)
        val newData2 = data2 match {
          case Message(code, message) ⇒ Message[B](code, message)
          case Value(x) ⇒ Value(f(x))
        }
        Data(Some((newValueA, newData2)))
      case Data(None) ⇒ Data(None)

    }

    // Identity law.
    forAll { (x: Data[Double]) ⇒ fmap(identity[Double])(x) shouldEqual x }

    // Composition law.
    forAll { (x: Data[Double], f: Double ⇒ String, g: String ⇒ Long) ⇒ fmap(f andThen g)(x) shouldEqual (fmap(f) andThen fmap(g)) (x) }
  }

  it should "ex01-3" in {
    // Data[A] ≡ (String ⇒ Int ⇒ A) × A + (Boolean ⇒ Double ⇒ A) × A
    // Notice the common structure: (X ⇒ Y ⇒ A) × A
    final case class Data2[X, Y, A](g: X ⇒ Y ⇒ A, da: A)

    // Notice that Data[A] is the same as an Either[..., ...]
    type Data[A] = Either[Data2[String, Int, A], Data2[Boolean, Double, A]]

    def dataEqual[A](d1: Data[A], d2: Data[A]) = forAll { (s: String, i: Int, b: Boolean, d: Double) ⇒
      d1 match {
        case Left(Data2(g1, da1)) ⇒ d2 match {
          case Left(Data2(g2, da2)) ⇒
            da1 shouldEqual da2
            g1(s)(i) shouldEqual g2(s)(i)
          case _ ⇒ Failed
        }
        case Right(Data2(g1, da1)) ⇒ d2 match {
          case Right(Data2(g2, da2)) ⇒
            da1 shouldEqual da2
            g1(b)(d) shouldEqual g2(b)(d)
          case _ ⇒ Failed
        }
      }
    }

    def fmap[A, B](f: A ⇒ B): Data[A] ⇒ Data[B] = {
      case Left(Data2(g, da)) ⇒
        val newData2: Data2[String, Int, B] = Data2(x ⇒ y ⇒ f(g(x)(y)), f(da))
        Left(newData2)
      case Right(Data2(g, da)) ⇒
        val newData2: Data2[Boolean, Double, B] = Data2(x ⇒ y ⇒ f(g(x)(y)), f(da))
        Right(newData2)
    }

    // Identity law.
    forAll { (x: Data[Double]) ⇒ dataEqual(x, fmap(identity[Double])(x)) }

    // Composition law.
    forAll { (x: Data[Double], f: Double ⇒ String, g: String ⇒ Long) ⇒
      dataEqual(
        fmap(f andThen g)(x),
        (fmap(f) andThen fmap(g)) (x)
      )
    }

  }

  it should "ex02-1" in {
    // Data[A] ≡ (A ⇒ Int) + (A ⇒ A ⇒ String)
    type Data[A] = Either[A ⇒ Int, A ⇒ A ⇒ String]

    def dataEqual[A: Arbitrary](d1: Data[A], d2: Data[A]) = forAll { (x: A, y: A) ⇒
      d1 match {
        case Left(g1) ⇒ d2 match {
          case Left(g2) ⇒ g1(x) shouldEqual g2(x)
          case _ ⇒ Failed
        }
        case Right(g1) ⇒ d2 match {
          case Right(g2) ⇒ g1(x)(y) shouldEqual g2(x)(y)
          case _ ⇒ Failed
        }
      }
    }

    def contrafmap[A, B](f: B ⇒ A): Data[A] ⇒ Data[B] = {
      case Left(aToInt) ⇒ Left(a ⇒ aToInt(f(a)))
      case Right(aToAToString) ⇒ Right(b ⇒ c ⇒ aToAToString(f(b))(f(c)))
    }

    // Identity law.
    forAll { (x: Data[Double]) ⇒ dataEqual(x, contrafmap(identity[Double])(x)) }

    // Contracomposition law.
    forAll { (x: Data[Long], f: String ⇒ Long, g: Double ⇒ String) ⇒
      dataEqual(
        contrafmap(g andThen f)(x),
        (contrafmap(f) andThen contrafmap(g)) (x)
      )
    }
  }

  it should "ex02-2" in {
    // Data[A,B] ≡ (A + B) × ((A ⇒ Int) ⇒ B)
    final case class Data[A, B](ab: Either[A, B], d: (A ⇒ Int) ⇒ B)

    def dataEqual[A: Arbitrary, B](d1: Data[A, B], d2: Data[A, B])(implicit aint: Arbitrary[A ⇒ Int]) = forAll { (x: A ⇒ Int) ⇒
      d1.ab shouldEqual d2.ab
      d1.d(x) shouldEqual d2.d(x)
    }

    def fmapB[Z, B, C](f: B ⇒ C): Data[Z, B] ⇒ Data[Z, C] = data ⇒ {
      val newAB: Either[Z, C] = data.ab match {
        case Left(aValue) ⇒ Left(aValue)
        case Right(bValue) ⇒ Right(f(bValue))
      }
      val newD: (Z ⇒ Int) ⇒ C = g ⇒ f(data.d(g))
      Data(newAB, newD)
    }

    // Identity law.
    forAll { (x: Data[Int, Double]) ⇒ dataEqual(x, fmapB(identity[Double])(x)) }

    // Composition law.
    forAll { (x: Data[Int, Double], f: Double ⇒ String, g: String ⇒ Long) ⇒
      dataEqual(
        fmapB[Int, Double, Long](f andThen g)(x),
        (fmapB[Int, Double, String](f) andThen fmapB[Int, String, Long](g)) (x)
      )
    }

    def fmap[X, Y, B](f: X ⇒ Y): Data[X, B] ⇒ Data[Y, B] = data ⇒ {
      val newAB: Either[Y, B] = data.ab match {
        case Left(aValue) ⇒ Left(f(aValue))
        case Right(bValue) ⇒ Right(bValue)
      }
      val newD: (Y ⇒ Int) ⇒ B = g ⇒ data.d(x ⇒ g(f(x)))
      Data(newAB, newD)
    }

    // Identity law.
    forAll { (x: Data[Double, Int]) ⇒ dataEqual(x, fmap(identity[Double])(x)) }

    // Composition law.
    forAll { (x: Data[Double, Int], f: Double ⇒ String, g: String ⇒ Long) ⇒
      dataEqual(
        fmap[Double, Long, Int](f andThen g)(x),
        (fmap[Double, String, Int](f) andThen fmap[String, Long, Int](g)) (x)
      )
    }
  }

  it should "ex03" in {
    sealed trait Coi[+A, B]
    case class Pa[+A, B](b: (A, B), c: B ⇒ Int) extends Coi[A, B]
    case class Re[+A, B](d: A, e: B, c: Int) extends Coi[A, B]
    case class Ci[+A, B](f: String ⇒ A, g: B ⇒ A) extends Coi[A, B]

    // Coi[A, B] = (A × B) × (B ⇒ Int) + A × B × Int + (String ⇒ A) × (B ⇒ A)

    // The type parameter A is found in covariant positions only. Coi[A, B] is a functor w.r.t. A.
    // The type parameter B is found in both covariant and contravariant positions. Coi[A, B] is not a functor w.r.t. B.
    // The type Int is found in covariant positions only. The type String is found in contravariant position.
    // If desired, the type constructor Coi[A, B] can be parameterized additionally by types I and S replacing Int and String.

    sealed trait Coi2[+A, B, +I, -S]
    case class Pa2[+A, B, +I, -S](b: (A, B), c: B ⇒ I) extends Coi[A, B]
    case class Re2[+A, B, +I, -S](d: A, e: B, c: I) extends Coi[A, B]
    case class Ci2[+A, B, +I, -S](f: S ⇒ A, g: B ⇒ A) extends Coi[A, B]
  }

}
