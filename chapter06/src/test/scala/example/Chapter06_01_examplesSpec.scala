package example

import cats.Functor
import cats.syntax.functor._
import io.chymyst.ch._
import org.scalatest.FlatSpec
import org.scalacheck.ScalacheckShapeless._
import cats.derive
import Filterable._

class Chapter06_01_examplesSpec extends FlatSpec with FilterableLawChecking {

  behavior of "examples"

  it should "run filter on Orders case class" in {
    // Orders with simple filtering.
    final case class Orders1[A](tue: Option[A], fri: Option[A]) {
      def withFilter(p: A ⇒ Boolean): Orders1[A] =
        Orders1(tue.filter(p), fri.filter(p))
    }

    Orders1(Some(500), Some(2000)).withFilter(_ < 1000) shouldEqual Orders1(Some(500), None)

    Orders1(Some(500), None).withFilter(_ < 1000) shouldEqual Orders1(Some(500), None)

    Orders1(Some(500), Some(2000)).withFilter(_ > 0) shouldEqual Orders1(Some(500), Some(2000))

    Orders1(Some(500), Some(2000)).withFilter(_ < 0) shouldEqual Orders1(None, None)
  }

  it should "implement additional business rule (a)" in {
    // Orders with business rule (a).
    final case class Orders2[A](tue: Option[A], fri: Option[A]) {
      def withFilter(p: A ⇒ Boolean): Orders2[A] = {
        val newTue = tue.filter(p)
        val newFri = fri.filter(p)
        if (tue.forall(p) && fri.forall(p))
          Orders2(newTue, newFri)
        else Orders2(None, None)
      }
    }

    Orders2(Some(500), Some(2000)).withFilter(_ < 1000) shouldEqual Orders2(None, None)

    Orders2(Some(500), None).withFilter(_ < 1000) shouldEqual Orders2(Some(500), None)

    Orders2(Some(500), Some(2000)).withFilter(_ > 0) shouldEqual Orders2(Some(500), Some(2000))
  }

  it should "implement additional business rule (b)" in {
    // Orders with business rule (b).
    final case class Orders3[A](tue: Option[A], fri: Option[A]) {
      def withFilter(p: A ⇒ Boolean): Orders3[A] = {
        if (tue.exists(p) || fri.exists(p))
          this
        else Orders3(None, None)
      }
    }

    Orders3(Some(500), Some(2000)).withFilter(_ < 1000) shouldEqual Orders3(Some(500), Some(2000))

    Orders3(Some(500), None).withFilter(_ < 1000) shouldEqual Orders3(Some(500), None)

    Orders3(Some(500), Some(2000)).withFilter(_ > 0) shouldEqual Orders3(Some(500), Some(2000))

    Orders3(Some(500), Some(2000)).withFilter(_ < 0) shouldEqual Orders3(None, None)
  }

  it should "use functor block notation" in {
    val result1 = for {
      x ← 1 to 10
      y = x * x - 10 * x + 1
      if y < 0
      z = x + y * y
      if z < 100
      p = z - x
    } yield {
      (x, y, z - x)
    }

    val result2 = for {
      x ← 1 to 10
      y = x * x - 10 * x + 1
      if y < 0
      if x + y * y < 100
      z = x + y * y
      p = z - x
    } yield {
      (x, y, p)
    }

    val result3 = for {
      x ← 1 to 10
      y = x * x - 10 * x + 1
      if y < 0
      if math.abs(y) < math.sqrt(100 - x)
      z = x + y * y
      p = z - x
    } yield {
      (x, y, p)
    }

    val result4 = for {
      x ← 1 to 10
      y = x * x - 10 * x + 1
      if y < 0 && math.abs(y) < math.sqrt(100 - x)
      z = x + y * y
      p = z - x
    } yield {
      (x, y, p)
    }

    result1 shouldEqual Seq((1, -8, 64), (9, -8, 64))

    result1 shouldEqual result2
    result1 shouldEqual result3
    result1 shouldEqual result4
  }

  behavior of "law checking for FilterableWithFilter"

  final case class Orders[A](tue: Option[A], fri: Option[A])

  val data = Orders(Some(500), Some(2000))

  // Functor instance is required for Filterable instances.
  implicit val functorOrders: Functor[Orders] = derive.functor[Orders]

  it should "verify laws for Orders example 1" in {
    // Declare an instance of FilterableWithFilter type class.
    // Orders with simple filtering.
    implicit val fwfOrders: FilterableWithFilter[Orders] = new FilterableWithFilter[Orders]() {
      override def withFilter[A](p: A ⇒ Boolean)(fa: Orders[A]): Orders[A] =
        Orders(fa.tue.filter(p), fa.fri.filter(p))
    }
    checkFilterableLawsWithFilter[Orders, Boolean, Boolean]()

    // Use the data type as filterable functor.
    val result = for {
      x ← data
      if x < 1000
      y = s"Amount: $x"
    } yield y
    result shouldEqual Orders(Some("Amount: 500"), None)
  }

  it should "verify laws for Orders example 2" in {
    // Declare an instance of FilterableWithFilter type class.
    // Orders with business rule (a).
    implicit val fwfOrders: FilterableWithFilter[Orders] = new FilterableWithFilter[Orders]() {
      override def withFilter[A](p: A ⇒ Boolean)(fa: Orders[A]): Orders[A] = {
        val newTue = fa.tue.filter(p)
        val newFri = fa.fri.filter(p)
        if (fa.tue.forall(p) && fa.fri.forall(p))
          Orders(newTue, newFri)
        else Orders(None, None)
      }
    }
    checkFilterableLawsWithFilter[Orders, Boolean, Boolean]()
    // Use the data type as filterable functor.
    val result = for {
      x ← data
      if x < 1000
      y = s"Amount: $x"
    } yield y
    result shouldEqual Orders(None, None)
  }

  it should "verify laws for Orders example 3" in {
    // Declare an instance of FilterableWithFilter type class.
    // Orders with business rule (b).
    implicit val fwfOrders: FilterableWithFilter[Orders] = new FilterableWithFilter[Orders]() {
      override def withFilter[A](p: A ⇒ Boolean)(fa: Orders[A]): Orders[A] = {
        if (fa.tue.exists(p) || fa.fri.exists(p))
          fa
        else Orders(None, None)
      }
    }
    //  checkFilterableLawsWithFilter[Orders, Boolean, Boolean]() // fails

    // Breaks the conjunction law:
    data.filter(_ < 1000).filter(_ > 1000) shouldNot equal(data.filter(x ⇒ x < 1000 && x > 1000))

    // Breaks the partial function law.
    the[Exception] thrownBy data.filter(_ < 1000).map { case x if x < 1000 ⇒ x } should have message "2000 (of class java.lang.Integer)"

    // Use the data type as filterable functor.
    val result = for {
      x ← data
      if x < 1000
      y = s"Amount: $x" // Counter-intuitive: the value 2000 still passes the filter line.
    } yield y
    result shouldEqual Orders(Some("Amount: 500"), Some("Amount: 2000"))
  }

  it should "define withFilter for stingy product" in {
    // Type F[A] = 1 + A × A
    type F[A] = Option[(A, A)]

    implicit val functorF: Functor[F] = derive.functor[F]

    implicit val withFilterF: FilterableWithFilter[F] = new FilterableWithFilter[F]() {
      override def withFilter[A](p: A => Boolean)(fa: F[A]): F[A] = fa match {
        case Some((x, y)) if p(x) && p(y) ⇒ fa
        case _ ⇒ None
      }
    }
    checkFilterableLawsWithFilter[F, Boolean, Boolean]()
    checkFilterableLawsWithFilter[F, Int, String]()
  }

  behavior of "wrong implementations of filter"

  it should "show broken laws for some examples" in {

    final case class A1[T](d: Option[T])

    implicit val functorA1: Functor[A1] = derive.functor[A1]

    implicit val filterableA1: FilterableWithFilter[A1] = new FilterableWithFilter[A1]() {
      override def withFilter[A](p: A => Boolean)(fa: A1[A]): A1[A] = A1(None)
    }

    // Breaks the identity law.
    A1(Some(1)).filter(_ ⇒ true) shouldNot equal(A1(Some(1)))
  }

}
