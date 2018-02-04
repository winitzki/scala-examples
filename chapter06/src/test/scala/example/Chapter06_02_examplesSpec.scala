package example

import cats.{Functor, derive}
import cats.syntax.functor._
import io.chymyst.ch._
import org.scalatest.FlatSpec
import org.scalacheck.ScalacheckShapeless._
import Filterable._
import cats.evidence.Is

class Chapter06_02_examplesSpec extends FlatSpec with FilterableLawChecking {

  behavior of "filterable laws in depth"

  final case class Orders[A](tue: Option[A], fri: Option[A])

  // Functor instance is required for Filterable instances.
  implicit val functorOrders: Functor[Orders] = derive.functor[Orders]

  val filterableWithFilterOrders = new FilterableWithFilter[Orders] {
    override def withFilter[A](p: A ⇒ Boolean)(fa: Orders[A]): Orders[A] = {
      val newTue = fa.tue.filter(p)
      val newFri = fa.fri.filter(p)
      if (fa.tue.forall(p) && fa.fri.forall(p))
        Orders(newTue, newFri)
      else Orders(None, None)
    }
  }

  val filterableOrders = new Filterable[Orders] {
    override def flatten[A](fa: Orders[Option[A]]): Orders[A] = Orders(fa.tue.flatten, fa.fri.flatten)
  }

  val data = Orders(Some(500), Some(2000))

  it should "express filter through flatten and vice versa, check isomorphism" in {

    def filterFromFlatten[F[_] : Functor, A](flatten: F[Option[A]] ⇒ F[A]): (A ⇒ Boolean) ⇒ F[A] ⇒ F[A] = {
      p ⇒
        val fmapAOptionA = flip(implicitly[Functor[F]].map[A, Option[A]])
        fmapAOptionA(bop(p)) andThen flatten
    }

    def flattenFromFilter[F[_] : FilterableWithFilter, A]: F[Option[A]] ⇒ F[A] = {
      foa ⇒
        implicit val functor = implicitly[FilterableWithFilter[F]].functor
        foa.filter(_.nonEmpty).map(_.get)
    }

    // Use the filterable functor Orders[_] as an example.
    implicit val fwf = filterableWithFilterOrders

    def flattenForOrders[A] = flattenFromFilter[Orders, A]

    def filterFromFlattenForOrders[A] = filterFromFlatten[Orders, A](flattenForOrders)

    forAll { (orders: Orders[Int], p: Int ⇒ Boolean) ⇒ orders.filter(p) shouldEqual filterFromFlattenForOrders(p)(orders) }
  }

  it should "express mapOption through flatten and vice versa, check isomorphism" in {

    def flattenFromMapOption[F[_] : Functor, A, B](mapOption: (A ⇒ Option[B]) ⇒ F[A] ⇒ F[B])
      (implicit ev: A Is Option[B]): F[Option[B]] ⇒ F[B] = {
      fob ⇒
        val fa: F[A] = ev.flip.substitute(fob)
        mapOption { x: A ⇒
          val ob: Option[B] = ev.coerce(x)
          ob
        }(fa)
    }

    def mapOptionFromFlatten[F[_] : Functor, A, B](flatten: F[Option[B]] ⇒ F[B]): (A ⇒ Option[B]) ⇒ F[A] ⇒ F[B] = {
      f ⇒
        val fmapAOptionB = flip(implicitly[Functor[F]].map[A, Option[B]])
        fmapAOptionB(f) andThen flatten
    }

    // Define `flatten` for orders using `filter`.

    implicit val f = filterableOrders

    def flattenForOrders[B]: Orders[Option[B]] ⇒ Orders[B] = fob ⇒ fob.filter(_.nonEmpty).map(_.get)

    def mapOptionFromFlattenForOrders[A, B] = mapOptionFromFlatten[Orders, A, B](flattenForOrders)

    def flattenFromMapOptionForOrders[B] = flattenFromMapOption[Orders, Option[B], B](mapOptionFromFlattenForOrders[Option[B], B])

    forAll { (orders: Orders[Option[Int]]) ⇒ flattenForOrders(orders) shouldEqual flattenFromMapOptionForOrders(orders) }
  }

  it should "demonstrate naturality law for `bop`" in {
    // bop(p): A ⇒ 1 + A is  { x: A ⇒ Some(x).filter(p) }
    def bopAsComposition[A](p: A ⇒ Boolean): A ⇒ Option[A] = Some.apply[A] _ andThen (_.filter(p))

    // Check that `bop` and `bopAsComposition` are the same function.
    forAll { (x: Int, p: Int ⇒ Boolean) ⇒ bop(p)(x) shouldEqual bopAsComposition(p)(x) }

    // Naturality law.
    // f ◦ bop p = bop (f ◦ p) ◦ fmap f
    // (f: T ⇒ A) ◦ (p: A ⇒ Boolean) is of type T ⇒ Boolean
    // (f: T ⇒ A) ◦ (bop p): A ⇒ 1 + A   should equal ((bop f ◦ p): T ⇒ 1 + T) ◦ (fmap f): 1 + T ⇒ 1 + A
    // where fmap is with respect to the Option functor.
    forAll { (x: String, p: Int ⇒ Boolean, f: String ⇒ Int) ⇒ (f andThen bop(p)) (x) shouldEqual (bop(f andThen p) andThen (_.map(f))) (x) }

    /* Check this law symbolically:
    (f ◦ bop p)(x) = bop(p)(f(x)) = Some(f(x)).filter(p)
     = if (p(f(x)) Some(f(x)) else None  // expression (1)

    (bop (f ◦ p) ◦ fmap f)(x) = (bop(f ◦ p)(x)).map(f)
     = (Some(x).filter(f ◦ p)).map(f)
     = (if (p(f(x)) Some(x) else None ).map(f)
     = (if (p(f(x)) Some(x).map(f) else None.map(f) )
     = if (p(f(x)) Some(f(x)) else None  // expression (2)

     Expression (1) and expression (2) are identical.
     */
  }

  behavior of "implementing Filterable type class"

  it should "for Orders example" in {
    implicit val f = filterableOrders

    checkFilterableLaws[Orders, Int, String, Boolean]()
  }
}
