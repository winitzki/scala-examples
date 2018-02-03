package example

import cats.{Contravariant, Functor, data, derive}
import org.scalatest.FlatSpec
import cats.syntax.functor._
import cats.syntax.contravariant._
import io.chymyst.ch._
import org.scalatest.FlatSpec
import org.scalacheck.ScalacheckShapeless._
import Filterable._

class Chapter06_02_workedExamplesSpec extends FlatSpec with FilterableLawChecking {

  final case class Orders[A](tue: Option[A], fri: Option[A])

  val data = Orders(Some(500), Some(2000))

  val p: Int ⇒ Boolean = _ < 1000

  // Functor instance is required for Filterable instances.
  implicit val functorOrders: Functor[Orders] = derive.functor[Orders]

  implicit val filterableOrders = new Filterable[Orders] {
    override def flatten[A](fa: Orders[Option[A]]): Orders[A] = Orders(fa.tue.flatten, fa.fri.flatten)
  }

  behavior of "filterable functor constructions"

  it should "derive filterable instance for sums and products of filterables (constructions 2 and 3)" in {

    implicit def functorProduct[F[_] : Functor, G[_] : Functor] = new Functor[Lambda[X ⇒ (F[X], G[X])]] {
      override def map[A, B](fa: (F[A], G[A]))(f: A ⇒ B): (F[B], G[B]) = (fa._1.map(f), fa._2.map(f))
    }

    implicit def functorSum[F[_] : Functor, G[_] : Functor] = new Functor[Lambda[X ⇒ Either[F[X], G[X]]]] {
      override def map[A, B](fga: Either[F[A], G[A]])(f: A ⇒ B): Either[F[B], G[B]] = fga match {
        case Left(fa) ⇒ Left(fa.map(f))
        case Right(ga) ⇒ Right(ga.map(f))
      }
    }

    implicit def filterableProduct[F[_] : Filterable : Functor, G[_] : Filterable : Functor] = {
      new Filterable[Lambda[X ⇒ (F[X], G[X])]] {
        override def flatten[A](fa: (F[Option[A]], G[Option[A]])): (F[A], G[A]) = (fa._1.flatten, fa._2.flatten)
      }
    }

    implicit def filterableSum[F[_] : Filterable : Functor, G[_] : Filterable : Functor] = new Filterable[Lambda[X ⇒ Either[F[X], G[X]]]] {
      override def flatten[A](fa: Either[F[Option[A]], G[Option[A]]]): Either[F[A], G[A]] = fa match {
        case Left(foa) ⇒ Left(foa.flatten)
        case Right(goa) ⇒ Right(goa.flatten)
      }
    }

    // Check that we now have an instance of Filterable for the product and sum types.
    type Orders2[X] = (Orders[X], Orders[X])
    type OrdersE[X] = Either[Orders[X], Orders[X]]

    implicitly[Filterable[Orders2]]
    implicitly[Filterable[OrdersE]]

    val dataProduct: Orders2[Int] = (data, data)

    dataProduct.filter(p) shouldEqual ((data.filter(p), data.filter(p)))

    val dataSum: OrdersE[Int] = Left(data)

    dataSum.filter(p) shouldEqual Left(data.filter(p))

    checkFilterableLaws[Orders2, Int, String, Boolean]()
    checkFilterableLaws[OrdersE, Int, String, Boolean]()
  }


  behavior of "filterable functor construction 5 and 6"

  it should "derive filterable instance for construction 5" in {
    // Construction 5: 1 + A × G[A]

    implicit def functor5[G[_] : Functor] = new Functor[Lambda[X ⇒ Option[(X, G[X])]]] {
      override def map[A, B](fa: Option[(A, G[A])])(f: A ⇒ B): Option[(B, G[B])] = fa.map { case (x, gx) ⇒ (f(x), gx.map(f)) }
    }

    implicit def filterable5[G[_] : Filterable : Functor] = new Filterable[Lambda[X ⇒ Option[(X, G[X])]]] {
      override def flatten[A](fa: Option[(Option[A], G[Option[A]])]): Option[(A, G[A])] =
        fa.flatMap { case (oa, goa) ⇒ oa.map(x ⇒ (x, goa.flatten)) }
    }

    // Check that we now have an instance of Filterable for the new types.
    type C5Orders[A] = Option[(A, Orders[A])]

    implicitly[Filterable[C5Orders]]

    val dataC5a: C5Orders[Int] = Some(400, data)
    val dataC5b: C5Orders[Int] = Some(2000, data)

    dataC5a.filter(p) shouldEqual Some(400, Orders(Some(500), None))
    dataC5b.filter(p) shouldEqual None

    checkFilterableLaws[C5Orders, Int, String, Boolean]()
  }

  it should "derive filterable instance for construction 6" in {
    // Construction 6: F[A] = G[A] + A × F[A] (recursive).
    // Need to use a sealed trait because `type F[A] = Either[G[A], (A, F[A])]` will not compile.

    sealed trait C6[G[_], A]
    final case class Base[G[_], A](ga: G[A]) extends C6[G, A]
    final case class Step[G[_], A](x: A, fa: C6[G, A]) extends C6[G, A]

    // Now we can put all implicits into the companion object for convenience.
    object C6 {
      implicit def functor6[G[_] : Functor] = new Functor[Lambda[X ⇒ C6[G, X]]] {
        override def map[A, B](fa: C6[G, A])(f: A ⇒ B): C6[G, B] = fa match {
          case Base(ga) ⇒ Base(ga.map(f))
          case Step(x, xfa) ⇒ Step(f(x), map(xfa)(f)) // Recursive step.
        }
      }

      implicit def filterable6[G[_] : Filterable : Functor] = new Filterable[Lambda[X ⇒ C6[G, X]]] {
        override def flatten[A](fa: C6[G, Option[A]]): C6[G, A] = fa match {
          case Base(ga) ⇒ Base(ga.flatten)
          case Step(ox, xfoa) ⇒ ox match {
            case Some(x) ⇒ Step(x, flatten(xfoa))
            case None ⇒ flatten(xfoa) // Recursive step: 0 + 1 × F[1 + A] -> F[1 + A] -> F[A]
          }
        }
      }
    }

    // Check that we now have an instance of Filterable for the new types.
    type C6Orders[A] = C6[Orders, A]

    implicitly[Filterable[C6Orders]]

    val dataC6: C6Orders[Int] = Step(400, Step(900, Step(1400, Base(Orders(Some(500), Some(2000))))))

    dataC6.filter(_ < 1000) shouldEqual Step(400, Step(900, Base(Orders(Some(500), None))))

    checkFilterableLaws[C6Orders, Int, String, Boolean]()
  }

  it should "derive filterable instance for construction 7" in {
    // Construction 7: G[A] ⇒ H[A]

    import ContraFilterable._

    implicit def contrafunctor7[G[_] : Contravariant, H[_] : Functor] = new Functor[Lambda[X ⇒ G[X] ⇒ H[X]]] {
      override def map[A, B](fa: G[A] ⇒ H[A])(f: A ⇒ B): G[B] ⇒ H[B] = gb ⇒ fa(gb.contramap(f)).map(f)
    }

    implicit def contrafilterable7[G[_] : ContraFilterable : Contravariant, H[_] : Filterable : Functor] = new Filterable[Lambda[X ⇒ G[X] ⇒ H[X]]] {
      override def flatten[A](fa: G[Option[A]] ⇒ H[Option[A]]): G[A] ⇒ H[A] = { ga ⇒
        val x = fa(ga.inflate)
        implicitly[Filterable[H]].flatten(x)
      }
    }

    // Use Option[A] ⇒ Int as the filterable contrafunctor G, and Orders as the filterable functor H.

    type C3[A] = Option[A] ⇒ Int

    implicit val contrafunctorC3 = new Contravariant[C3] {
      override def contramap[A, B](fa: C3[A])(f: B ⇒ A): C3[B] = implement
    }

    implicit val filterableC3 = new ContraFilterable[C3] {
      override def inflate[A](fa: C3[A]): C3[Option[A]] = implement
    }

    // Check that we now have an instance of Filterable for the new types.
    type C7Orders[A] = C3[A] ⇒ Orders[A]

    implicitly[Filterable[C7Orders]]

  }


}
