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
    override def deflate[A](fa: Orders[Option[A]]): Orders[A] = Orders(fa.tue.flatten, fa.fri.flatten)
  }

  val data = Orders(Some(500), Some(2000))

  it should "express filter through deflate and vice versa, check isomorphism" in {

    def filterFromdeflate[F[_] : Functor, A](deflate: F[Option[A]] ⇒ F[A]): (A ⇒ Boolean) ⇒ F[A] ⇒ F[A] = {
      p ⇒
        val fmapAOptionA = flip(implicitly[Functor[F]].map[A, Option[A]])
        fmapAOptionA(bop(p)) andThen deflate
    }

    def deflateFromFilter[F[_] : FilterableWithFilter, A]: F[Option[A]] ⇒ F[A] = {
      foa ⇒
        implicit val functor = implicitly[FilterableWithFilter[F]].functor
        foa.filter(_.nonEmpty).map(_.get)
    }

    // Use the filterable functor Orders[_] as an example.
    implicit val fwf = filterableWithFilterOrders

    def deflateForOrders[A] = deflateFromFilter[Orders, A]

    def filterFromdeflateForOrders[A] = filterFromdeflate[Orders, A](deflateForOrders)

    forAll { (orders: Orders[Int], p: Int ⇒ Boolean) ⇒ orders.filter(p) shouldEqual filterFromdeflateForOrders(p)(orders) }
  }

  it should "express mapOption through deflate and vice versa, check isomorphism" in {

    def deflateFromMapOption[F[_] : Functor, A, B](mapOption: (A ⇒ Option[B]) ⇒ F[A] ⇒ F[B])
      (implicit ev: A Is Option[B]): F[Option[B]] ⇒ F[B] = {
      fob ⇒
        val fa: F[A] = ev.flip.substitute(fob)
        mapOption { x: A ⇒
          val ob: Option[B] = ev.coerce(x)
          ob
        }(fa)
    }

    def mapOptionFromdeflate[F[_] : Functor, A, B](deflate: F[Option[B]] ⇒ F[B]): (A ⇒ Option[B]) ⇒ F[A] ⇒ F[B] = {
      f ⇒
        val fmapAOptionB = flip(implicitly[Functor[F]].map[A, Option[B]])
        fmapAOptionB(f) andThen deflate
    }

    // Define `deflate` for orders using `filter`.

    implicit val f = filterableOrders

    def deflateForOrders[B]: Orders[Option[B]] ⇒ Orders[B] = fob ⇒ fob.filter(_.nonEmpty).map(_.get)

    def mapOptionFromdeflateForOrders[A, B] = mapOptionFromdeflate[Orders, A, B](deflateForOrders)

    def deflateFromMapOptionForOrders[B] = deflateFromMapOption[Orders, Option[B], B](mapOptionFromdeflateForOrders[Option[B], B])

    forAll { (orders: Orders[Option[Int]]) ⇒ deflateForOrders(orders) shouldEqual deflateFromMapOptionForOrders(orders) }
  }

  it should "demonstrate the naturality law for `bop`" in {
    // bop(p): A ⇒ 1 + A is  { (x: A) ⇒ Some(x).filter(p) }
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

  it should "demonstrate the conjunction property for `bop`" in {
    // bop(x ⇒ p1(x) && p2(x))(x) = bop(p1)(x).flatMap (bop(p2))

    forAll { (x: Int, p1: Int ⇒ Boolean, p2: Int ⇒ Boolean) ⇒ bop((x: Int) ⇒ p1(x) && p2(x))(x) shouldEqual bop(p1)(x).flatMap(bop(p2)) }

    /* Check this property symbolically:
    bop(x ⇒ p1(x) && p2(x))(x) = Some(x).filter(x ⇒ p1(x) && p2(x))
     = if (p1(x) && p2(x)) Some(x) else None // expression (1)

    bop(p1)(x) = Some(x).filter(p1) = if (p1(x)) Some(x) else None

    bop(p1)(x).flatMap(bop(p2)) = (if (p1(x)) Some(x) else None).flatMap(x ⇒ if (p2(x)) Some(x) else None)
     = if (p1(x)) { if (p2(x)) Some(x) else None } else None
     = if (p1(x) && p2(x)) Some(x) else None  // expression (2)

     Expression (1) and expression (2) are identical.
     */
  }

  behavior of "implementing Filterable type class"

  it should "for Orders example" in {
    implicit val f = filterableOrders

    checkFilterableLaws[Orders, Int, String, Boolean]()
  }
}
