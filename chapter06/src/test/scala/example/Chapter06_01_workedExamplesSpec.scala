package example

import cats.{Contravariant, Functor, derive}
import cats.syntax.functor._
import io.chymyst.ch._
import org.scalatest.{Assertion, FlatSpec}
import org.scalacheck.ScalacheckShapeless._
import Filterable._
import org.scalacheck.Arbitrary

class Chapter06_01_workedExamplesSpec extends FlatSpec with FilterableLawChecking with ContraFilterableLawChecking {

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

    // Use `cats.derive` to save ourselves some coding.
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
          case Jill0() ⇒ fa.jills
          case Jill1(c1) ⇒ if (p(c1)) Jill1(c1) else Jill0()
          case Jill2(c1, c2) ⇒ (p(c1), p(c2)) match {
            case (true, true) ⇒ Jill2(c1, c2)
            case (true, false) ⇒ Jill1(c1)
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
      override def map[A, B](fa: Server[A])(f: A ⇒ B): Server[B] =
        Server(fa.requests.map(f))
    }

    implicit val filterableServer = new FilterableWithFilter[Server] {
      override def withFilter[A](p: A ⇒ Boolean)(fa: Server[A]): Server[A] = {
        val admissibleRequests = fa.requests.takeWhile(p)
        Server(admissibleRequests)
      }
    }

    val data = Server(Seq(10, 20, 30, 40, 0))

    val result = for {
      x ← data
      y = x * x
      if y < 1000
    } yield s"Accepted request $x"

    result shouldEqual Server(Seq("Accepted request 10", "Accepted request 20", "Accepted request 30"))

    checkFilterableLawsWithFilter[Server, String, Double]()
  }

  it should "ex03" in {
    // The type is (1 + A) × (1 + A × A).
    // Let us reuse the filterable instances for (1 + A) and for (1 + A × A).

    type F1[A] = Option[A]

    implicit val functorF1 = derive.functor[F1]

    implicit val withFilterF1 = new FilterableWithFilter[F1] {
      override def withFilter[A](p: A ⇒ Boolean)(fa: F1[A]): F1[A] = fa match {
        case Some(x) if p(x) ⇒ fa
        case _ ⇒ None
      } // fa.filter(p)
    }

    type F2[A] = Option[(A, A)]

    implicit val functorF2 = derive.functor[F2]

    implicit val withFilterF2 = new FilterableWithFilter[F2] {
      override def withFilter[A](p: A ⇒ Boolean)(fa: F2[A]): F2[A] = fa match {
        case Some((x, y)) if p(x) && p(y) ⇒ fa
        case _ ⇒ None
      }
    }

    final case class P[T](first: F1[T], second: F2[T])

    implicit val functorF3 = derive.functor[P]

    implicit val withFilterF3 = new FilterableWithFilter[P] {
      override def withFilter[A](p: A ⇒ Boolean)(fa: P[A]): P[A] =
        P(fa.first.filter(p), fa.second.filter(p))
    }

    checkFilterableLawsWithFilter[P, String, Double]()
  }

  it should "ex04" in {
    // The type is Int + Int × A + Int × A × A + Int × A × A × A =
    //
    //    Int × (1 + A + A × A + A × A × A)
    // We could simply leave Int unchanged and use JohnsCoupons implementation to filter (1 + A + A × A + A × A × A)
    // Here is an interesting implementation that keeps some information about items that were filtered out.
    // The resulting transformation on integers is still consistent with the laws for filterable.

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

  // The functor F[A] = A + A × F[A] is not filterable since an empty container
  // is not represented by any part of the disjunction.
  // A + A × A + A × A × A + ...
  // In contrast, F[A] = 1 + A + A × F[A] would be filterable.
  it should "ex05" in {

    sealed trait NonEmptyList[A]
    final case class One[A](one: A) extends NonEmptyList[A]
    final case class WithTail[A](head: A, tail: NonEmptyList[A]) extends NonEmptyList[A]

    implicit val functorNEL = derive.functor[NonEmptyList]

    // We try defining a filterable instance - but will fail the laws.
    implicit val filterableNEL = new FilterableWithFilter[NonEmptyList] {
      override def withFilter[A](p: A ⇒ Boolean)(fa: NonEmptyList[A]): NonEmptyList[A] = fa match {
        case One(one) ⇒ One(one) // No way to use the filter here.
        case WithTail(head, tail) ⇒ if (p(head))
          WithTail(head, withFilter(p)(tail))
        else
          withFilter(p)(tail)
      }
    }

    //    checkFilterableLawsWithFilter[NonEmptyList, String, Double]() // Fails the partial function law.

    val data: NonEmptyList[Double] = WithTail(-200.0, One(-100.0))

    val result: NonEmptyList[Double] = for {
      x ← data
      if x > 0
    } yield math.sqrt(x)

    // math.sqrt of a negative number was called despite filtering.
    result should matchPattern {
      case One(nan) if nan.asInstanceOf[Double].isNaN ⇒
    }
  }

  // The functor F[Z,A] = Z + Int × Z × A × A is filterable because we can reuse the value
  // of type Z to represent an empty container.
  // In contrast, Z + Int × A × A would not be filterable.
  it should "ex06" in {
    type F[Z, A] = Either[Z, (Int, Z, A, A)]

    // derive.functor does not seem to work with a type lambda.
    // Syntax of the "kind projector" plugin:
    // Lambda[X ⇒ F[Z, X]] -- works
    // type Q[X] = F[Z, X] -- does not work
    // F[Z, *] -- works
    /////
    // f(z, x)
    // x ⇒ f(z, x)
    // f(z, _)
    "implicit def functorF[Z] = derive.functor[F[Z, *]]" shouldNot compile

    /* What we need to define a functor instance for F[Z, A] with respect to A:
    implicit def functorF[Z] = new Functor[ ??? ] { ... }
     When we write `new Functor[]` we need to specify a type constructor
     and write something like `new Functor[Q]`. But we don't have a suitable type constructor `Q`.
     The `Functor` type class requires a type constructor with a single type argument. But we have F[Z, A] with two type arguments.
     We need to fix one of the type arguments, Z, and let the other remain free, and so define a new type constructor Q.
     So we need something like this:
     implicit def functorF[Z] = new Functor[  Q,  where Q is defined as `type Q[X] = F[Z, X] `   ]
     A valid Scala syntax for this would be to define `type Q[X]` inside an object and then extract Q from the object's type:
     implicit def functorF[Z] = new Functor[ new Functor[ ({ type Q[X] = F[Z, X] })#Q ] { ... }
     But this is quite long and unreadable. The "kind projector" provides a shorter syntax: F[Z, *].
     */

    // curryhoward's `implement` works here.
    implicit def functorF[Z] = new Functor[F[Z, *]] {
      override def map[A, B](fa: F[Z, A])(f: A ⇒ B): F[Z, B] = implement
    }

    implicit def filterableF[Z] = new FilterableWithFilter[F[Z, *]]() {
      override def withFilter[A](p: A ⇒ Boolean)(fa: F[Z, A]): F[Z, A] = fa match {
        case Left(_) ⇒ fa
        case Right((_, z, x, y)) ⇒ if (p(x) && p(y)) fa else Left(z)
      }
    }

    checkFilterableLawsWithFilter[F[Boolean, *], Int, Boolean]()
    checkFilterableLawsWithFilter[F[Int, *], String, Double]()
  }

  // The functor F[Z, A] = 1 + Z + Int × A × List[A]
  // We will use the standard `filter` function for `List`.
  it should "ex07" in {
    sealed trait F[Z, A]
    final case class Empty[Z, A]() extends F[Z, A]
    final case class HaveZ[Z, A](z: Z) extends F[Z, A]
    final case class HaveList[Z, A](n: Int, x: A, l: List[A]) extends F[Z, A]

    // curryhoward's `implement` does not work here because it doesn't know about List being a functor.
    implicit def functorF[Z] = new Functor[F[Z, *]] {
      override def map[A, B](fa: F[Z, A])(f: A ⇒ B): F[Z, B] = fa match {
        case Empty() ⇒ Empty()
        case HaveZ(z) ⇒ HaveZ(z)
        case HaveList(n, x, l) ⇒ HaveList(n, f(x), l.map(f))
      }
    }

    implicit def filterableF[Z] = new FilterableWithFilter[F[Z, *]] {
      override def withFilter[A](p: A ⇒ Boolean)(fa: F[Z, A]): F[Z, A] = fa match {
        case HaveList(n, x, l) ⇒
          if (p(x))
            HaveList(n, x, l.filter(p))
          else
            Empty() // No other choice - we can't keep HaveList here.

        // In all other cases, the value `fa` is unchanged after the filtering operation.
        case _ ⇒ fa
      }
    }

    checkFilterableLawsWithFilter[F[Boolean, *], Int, String]()
  }

  it should "ex08" in {
    // Filterable contrafunctor C[A, Z] = A ⇒ 1 + Z w.r.t. A.
    type C[-A, +Z] = A ⇒ Option[Z]

    // No automatic derivation seems to be available in cats.derive._ for contrafunctors.
    implicit def contraC[Z] = new Contravariant[C[*, Z]] {
      override def contramap[A, B](fa: C[A, Z])(f: B ⇒ A): C[B, Z] = implement
    }

    // No automatic derivation in cats.derive._ for exponential types either.
    type X[A] = Int ⇒ A

    // cats.derive.functor works only for polynomial types, fails for exponential types.
    "implicit val functorX = derive.functor[X]" shouldNot compile

    implicit def contrafilterC[Z] = new ContraFilterableWithFilter[C[*, Z]] {
      override def withFilter[A](p: A ⇒ Boolean)(fa: C[A, Z]): C[A, Z] = {
        (x: A) ⇒ if (p(x)) fa(x) else None
      }
    }

    type Example8[A] = C[A, String]

    // Now have type class instance for Example8.
    implicitly[ContraFilterableWithFilter[Example8]]

    def e8Equal[A: Arbitrary](x: Example8[A], y: Example8[A]): Assertion = forAll { a: A ⇒ x(a) shouldEqual y(a) }

    checkContraFilterableLawsWithFilter[Example8, Int, String](e8Equal)
  }
}
