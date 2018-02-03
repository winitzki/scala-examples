package example

import cats.{Functor, derive}
import cats.syntax.functor._
import io.chymyst.ch._
import org.scalatest.FlatSpec
import org.scalacheck.ScalacheckShapeless._
import Filterable._

class Chapter06_02_examplesSpec extends FlatSpec with FilterableLawChecking {

  behavior of "filterable concepts"

  final case class Orders[A](tue: Option[A], fri: Option[A])

  // Functor instance is required for Filterable instances.
  implicit val functorOrders: Functor[Orders] = derive.functor[Orders]

  val data = Orders(Some(500), Some(2000))

  // Use some filterable functor as an example.
  implicit val fwfOrders = new FilterableWithFilter[Orders] {
    override def withFilter[A](p: A ⇒ Boolean)(fa: Orders[A]): Orders[A] = {
      val newTue = fa.tue.filter(p)
      val newFri = fa.fri.filter(p)
      if (fa.tue.forall(p) && fa.fri.forall(p))
        Orders(newTue, newFri)
      else Orders(None, None)
    }
  }

  it should "express filter through flatten and vice versa, check isomorphism" in {

    def flattenFromFilter[F[_] : FilterableWithFilter, A]: F[Option[A]] ⇒ F[A] = {
      foa ⇒
        implicit val functor = implicitly[FilterableWithFilter[F]].functor
        foa.filter(_.nonEmpty).map(_.get)
    }

    def filterFromFlatten[F[_] : Functor, A](flatten: F[Option[A]] ⇒ F[A]): (A ⇒ Boolean) ⇒ F[A] ⇒ F[A] = {
      p ⇒
        val fmapAOptionA = flip(implicitly[Functor[F]].map[A, Option[A]])
        fmapAOptionA(optB(p)) andThen flatten
    }

    def flattenForOrders[A] = flattenFromFilter[Orders, A]

    def filterFromFlattenForOrders[A] = filterFromFlatten[Orders, A](flattenForOrders)

    forAll { (orders: Orders[Int], p: Int ⇒ Boolean) ⇒ orders.filter(p) shouldEqual filterFromFlattenForOrders(p)(orders) }
  }

}
