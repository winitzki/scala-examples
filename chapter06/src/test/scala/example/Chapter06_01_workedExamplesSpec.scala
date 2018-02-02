package example

import cats.derive
import cats.Functor
import cats.syntax.functor._
import io.chymyst.ch._
import org.scalatest.FlatSpec
import org.scalacheck.ScalacheckShapeless._
import Filterable._

class Chapter06_01_workedExamplesSpec extends FlatSpec with FilterableLawChecking {

  behavior of "worked examples"

  it should "ex01" in {
    // Type: (1 + A + A × A + A × A × A) × (1 + A + A × A)
    sealed trait JohnsCoupons[A]
    final case class John0[A]() extends JohnsCoupons[A]
    final case class John1[A](c1: A) extends JohnsCoupons[A]
    final case class John2[A](c1: A, c2: A) extends JohnsCoupons[A]
    final case class John3[A](c1: A, c2: A, c3: A) extends JohnsCoupons[A]

    sealed trait JillsCoupons[A]
    final case class Jill0[A]() extends JillsCoupons[A]
    final case class Jill1[A](c1: A) extends JillsCoupons[A]
    final case class Jill2[A](c1: A, c2: A) extends JillsCoupons[A]

    final case class Coupons[A](johns: JohnsCoupons[A], jills: JillsCoupons[A])

    implicit val functorCoupons = derive.functor[Coupons]

    implicit val filterableCoupons = new FilterableWithFilter[Coupons] {
      override def withFilter[A](p: A ⇒ Boolean)(fa: Coupons[A]): Coupons[A] = {
        val newJohns: JohnsCoupons[A] = fa.johns match {
          case John0() ⇒ John0()
          case John1(c1) ⇒ if (p(c1)) John1(c1) else John0()
          case John2(c1, c2) ⇒ if (p(c1) && p(c2)) John2(c1, c2) else John0()
          case John3(c1, c2, c3) ⇒ if (p(c1) && p(c2) && p(c3)) John3(c1, c2, c3) else John0()
        }
        val newJills: JillsCoupons[A] = fa.jills match {
          case Jill0() ⇒ Jill0()
          case Jill1(c1) ⇒ if (p(c1)) Jill1(c1) else Jill0()
          case Jill2(c1, c2) ⇒ (p(c1), p(c2)) match {
            case (true, true) ⇒ Jill2(c1, c2)
            case (true, false) => Jill1(c1)
            case (false, true) ⇒ Jill1(c2)
            case (false, false) ⇒ Jill0()
          }
        }
        Coupons(newJohns, newJills)
      }
    }

    val data: Coupons[Int] = Coupons(John2(100, 200), Jill2(100, 200))

    val result: Coupons[String] = for {
      x ← data
      if x > 150
      y = s"Coupon value: $x"
    } yield y

    result shouldEqual Coupons(John0(), Jill1("Coupon value: 200"))

    checkFilterableLawsWithFilter[Coupons, Double, String]()
  }

  it should "ex02" in {

    final case class Server[R](requests: Seq[R])

    implicit val functorServer = new Functor[Server] {
      override def map[A, B](fa: Server[A])(f: A ⇒ B): Server[B] = Server(fa.requests.map(f))
    }

    implicit val filterableServer = new FilterableWithFilter[Server] {
      override def withFilter[A](p: A ⇒ Boolean)(fa: Server[A]): Server[A] = {
        val admissibleRequests = fa.requests.takeWhile(p)
        Server(admissibleRequests)
      }
    }

    val data = Server(Seq(10, 20, 30, 40, 50))

    val result = for {
      x ← data
      y = x * x
      if y < 1000
    } yield s"Accepted request $x"

    result shouldEqual Server(Seq("Accepted request 10", "Accepted request 20", "Accepted request 30"))

    checkFilterableLawsWithFilter[Server, String, Double]()
  }

  it should "ex03" in {
    // The type is Int + Int × A + Int × A × A + Int × A × A × A = Int × (1 + A + A × A + A × A × A)
    // We could simply leave Int unchanged and use JohnsCoupons implementation to filter (1 + A + A × A + A × A × A)
    // Here is an interesting implementation that keeps some information about items that were filtered out.
    sealed trait List3[A]
    final case class L0[A](n: Int) extends List3[A]
    final case class L1[A](n: Int, c1: A) extends List3[A]
    final case class L2[A](n: Int, c1: A, c2: A) extends List3[A]
    final case class L3[A](n: Int, c1: A, c2: A, c3: A) extends List3[A]

    implicit val functorList3 = derive.functor[List3]

    implicit val filterableList3 = new FilterableWithFilter[List3] {
      override def withFilter[A](p: A ⇒ Boolean)(fa: List3[A]): List3[A] = fa match {
        case L0(n) ⇒ L0(n)
        case L1(n, c1) ⇒ if (p(c1)) L1(n, c1) else L0(n + 1)
        case L2(n, c1, c2) ⇒
          val newItems = List(c1, c2).filter(p)
          fromList(n - 1, newItems)

        case L3(n, c1, c2, c3) ⇒
          val newItems = List(c1, c2, c3).filter(p)
          fromList(n, newItems)
      }

      // Partial function, only used on lists of length at most 3.
      private def fromList[A](n: Int, l: List[A]): List3[A] = l match {
        case List(x, y, z) ⇒ L3(n, x, y, z)
        case List(x, y) ⇒ L2(n + 1, x, y)
        case List(x) ⇒ L1(n + 2, x)
        case Nil ⇒ L0(n + 3)
      }
    }

    val data: List3[String] = L3(0, "bear", "fox", "wolf")

    val result = for {
      x ← data
      if x.length < 4
    } yield "fire" + x

    result shouldEqual L1(2, "firefox") // 2 items were filtered out

    checkFilterableLawsWithFilter[List3, String, Double]()
  }
}
