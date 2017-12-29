package example

import io.chymyst.ch._
import org.scalacheck.ScalacheckShapeless._

class Chapter04_01_examplesSpec extends LawChecking with ExistsProperty {

  behavior of "functor laws for Option"

  it should "define map and fmap for Option and verify laws" in {

    def map[A, B]: Option[A] ⇒ (A ⇒ B) ⇒ Option[B] = opt ⇒ f ⇒ opt match {
      case Some(x) ⇒ Some(f(x))
      case None ⇒ None
    }

    def mapBad[A, B]: Option[A] ⇒ (A ⇒ B) ⇒ Option[B] = opt ⇒ f ⇒ opt match {
      case Some(x) ⇒ None
      case None ⇒ None
    }

    def fmap[A, B]: (A ⇒ B) ⇒ Option[A] ⇒ Option[B] = f ⇒ {
      case Some(x) ⇒ Some(f(x))
      case None ⇒ None
    }

    def fmapAuto[A, B]: (A ⇒ B) ⇒ Option[A] ⇒ Option[B] = implement

    // Verify identity law.
    forAll { (opt: Option[Int]) ⇒
      map(opt)(identity[Int]) shouldEqual opt
    }

    forAll { (opt: Option[Int]) ⇒
      fmap(identity[Int])(opt) shouldEqual opt
    }

    forAll { (opt: Option[Int]) ⇒
      fmapAuto(identity[Int])(opt) shouldEqual opt
    }

    // Identity law does not hold for mapBad.
    existsSome { (opt: Option[Int]) ⇒
      mapBad(opt)(identity[Int]) should not be opt
    }

    // Verify composition law.
    forAll { (x: Option[Int], f: Int ⇒ String, g: String ⇒ Long) ⇒
      map(x)(f andThen g) shouldEqual map(map(x)(f))(g)
    }

    forAll { (x: Option[Int], f: Int ⇒ String, g: String ⇒ Long) ⇒
      fmap(f andThen g)(x) shouldEqual (fmap(f) andThen fmap(g)) (x)
    }

    forAll { (x: Option[Int], f: Int ⇒ String, g: String ⇒ Long) ⇒
      fmapAuto(f andThen g)(x) shouldEqual (fmapAuto(f) andThen fmapAuto(g)) (x)
    }

    // Composition law holds (trivially) for mapBad.
    forAll { (x: Option[Int], f: Int ⇒ String, g: String ⇒ Long) ⇒
      mapBad(x)(f andThen g) shouldEqual mapBad(mapBad(x)(f))(g)
    }
  }

  it should "verify that Set[A] is not a correct functor for certain types" in {
    // https://gist.github.com/tpolecat/7401433

    case class Bad(a: Int) {
      override def equals(a: Any) = true
    }

    val f: Int ⇒ Bad = n ⇒ Bad(n)
    val g: Bad ⇒ Int = b ⇒ b.a

    Set(1, 2, 3).map(f andThen g) shouldEqual Set(1, 2, 3)
    Set(1, 2, 3).map(f).map(g) shouldEqual Set(1)

    existsSome { (s: Set[Int]) ⇒ s.map(f andThen g) should not be s.map(f).map(g) }

  }

  behavior of "examples of functors"

  it should "verify laws for functor String × Int × A" in {
    case class QueryResult[A](name: String, time: Int, data: A)

    def fmap[A, B](f: A ⇒ B): QueryResult[A] ⇒ QueryResult[B] = {
      case QueryResult(name, time, data) ⇒ QueryResult(name, time, f(data))
    }

    def fmap1[A, B](f: A ⇒ B): QueryResult[A] ⇒ QueryResult[B] = qr ⇒ qr.copy(data = f(qr.data))

    def fmapAuto[A, B](f: A ⇒ B): QueryResult[A] ⇒ QueryResult[B] = implement

    // Identity law.
    forAll { (x: QueryResult[Int]) ⇒ fmap(identity[Int])(x) shouldEqual x }
    forAll { (x: QueryResult[Int]) ⇒ fmap1(identity[Int])(x) shouldEqual x }
    forAll { (x: QueryResult[Int]) ⇒ fmapAuto(identity[Int])(x) shouldEqual x }

    // Composition law.
    forAll { (x: QueryResult[Int], f: Int ⇒ String, g: String ⇒ Long) ⇒ fmap(f andThen g)(x) shouldEqual (fmap(f) andThen fmap(g)) (x) }
    forAll { (x: QueryResult[Int], f: Int ⇒ String, g: String ⇒ Long) ⇒ fmap1(f andThen g)(x) shouldEqual (fmap1(f) andThen fmap1(g)) (x) }
    forAll { (x: QueryResult[Int], f: Int ⇒ String, g: String ⇒ Long) ⇒ fmapAuto(f andThen g)(x) shouldEqual (fmapAuto(f) andThen fmapAuto(g)) (x) }

  }

  it should "verify laws for functor A × A × A" in {
    case class Vec3[A](x: A, y: A, z: A)

    def fmap[A, B](f: A ⇒ B): Vec3[A] ⇒ Vec3[B] = {
      case Vec3(x, y, z) ⇒ Vec3(f(x), f(y), f(z))
    }

    def fmapAuto[A, B](f: A ⇒ B): Vec3[A] ⇒ Vec3[B] = allOfType[Vec3[A] ⇒ Vec3[B]](f).head

    // Identity law.
    forAll { (x: Vec3[Int]) ⇒ fmap(identity[Int])(x) shouldEqual x }
    forAll { (x: Vec3[Int]) ⇒ fmapAuto(identity[Int])(x) shouldEqual x }

    // Composition law.
    forAll { (x: Vec3[Int], f: Int ⇒ String, g: String ⇒ Long) ⇒ fmap(f andThen g)(x) shouldEqual (fmap(f) andThen fmap(g)) (x) }
    forAll { (x: Vec3[Int], f: Int ⇒ String, g: String ⇒ Long) ⇒ fmapAuto(f andThen g)(x) shouldEqual (fmapAuto(f) andThen fmapAuto(g)) (x) }

  }

  it should "verify laws for functor String + String × Int × A" in {
    sealed trait QueryResult[A]
    final case class Error[A](message: String) extends QueryResult[A]
    final case class Success[A](name: String, time: Int, data: A) extends QueryResult[A]

    def fmap[A, B](f: A ⇒ B): QueryResult[A] ⇒ QueryResult[B] = {
      case Error(message) ⇒ Error(message)
      case Success(name, time, data) ⇒ Success(name, time, f(data))
    }

    def fmapAuto[A, B](f: A ⇒ B): QueryResult[A] ⇒ QueryResult[B] = implement

    // Identity law.
    forAll { (x: QueryResult[Int]) ⇒ fmap(identity[Int])(x) shouldEqual x }
    forAll { (x: QueryResult[Int]) ⇒ fmapAuto(identity[Int])(x) shouldEqual x }

    // Composition law.
    forAll { (x: QueryResult[Int], f: Int ⇒ String, g: String ⇒ Long) ⇒ fmap(f andThen g)(x) shouldEqual (fmap(f) andThen fmap(g)) (x) }
    forAll { (x: QueryResult[Int], f: Int ⇒ String, g: String ⇒ Long) ⇒ fmapAuto(f andThen g)(x) shouldEqual (fmapAuto(f) andThen fmapAuto(g)) (x) }

  }

  it should "fail to derive map for non-functor NotContainer" in {
    case class NotContainer[A](x: A ⇒ Int, y: A)

    def try_fmaps[A, B] = allOfType[(A ⇒ B) ⇒ NotContainer[A] ⇒ NotContainer[B]]

    def try_contrafmaps[A, B] = allOfType[(B ⇒ A) ⇒ NotContainer[A] ⇒ NotContainer[B]]

    // Derived one implementation of fmap; but we will see that it already fails the identity law.
    try_fmaps[Int, String].length shouldEqual 1

    // Derived no implementations of contrafmap.
    try_contrafmaps[Int, String].length shouldEqual 0

    // Identity law fails for autoderived fmap.
    val nc1 = NotContainer[Int](_ + 1, 100)

    existsSome { (i: Int) ⇒
      val nc2 = try_fmaps.head(identity[Int])(nc1)

      (nc1.x(i) == nc2.x(i) && nc1.y == nc2.y) shouldEqual false
    }
  }

  it should "fail to verify laws for element-swapping fmap" in {
    case class Vec3[A](x: A, y: A, z: A)

    def fmapBad[A, B](f: A ⇒ B): Vec3[A] ⇒ Vec3[B] = {
      case Vec3(x, y, z) ⇒ Vec3(f(y), f(x), f(z))
    }

    existsSome { (x: Vec3[Int]) ⇒ fmapBad(identity[Int])(x) should not be x }
  }

  it should "implement map for recursive type" in {
    sealed trait LP[A]
    final case class LPempty[A]() extends LP[A]
    final case class LPpair[A](x: A, y: A, tail: LP[A]) extends LP[A]

    def fmap[A, B](f: A ⇒ B): LP[A] ⇒ LP[B] = {
      case LPempty() ⇒ LPempty[B]()
      case LPpair(x, y, tail) ⇒ LPpair[B](f(x), f(y), fmap(f)(tail))
    }

    // Identity law.
    forAll { (x: LP[Int]) ⇒ fmap(identity[Int])(x) shouldEqual x }

    // Composition law.
    forAll { (x: LP[Int], f: Int ⇒ String, g: String ⇒ Long) ⇒ fmap(f andThen g)(x) shouldEqual (fmap(f) andThen fmap(g)) (x) }
  }
}
