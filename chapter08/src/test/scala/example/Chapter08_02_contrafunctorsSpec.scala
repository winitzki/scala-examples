package example

import cats.kernel.Monoid
import cats.{Contravariant, ContravariantSemigroupal, Functor}
import ContraWuZip._
import cats.syntax.contravariant._
import cats.syntax.functor._
import cats.syntax.monoid._
import org.scalatest.{FlatSpec, Matchers}
import io.chymyst.ch._

class Chapter08_02_contrafunctorsSpec extends FlatSpec with Matchers {

  behavior of "applicative contrafunctor constructions"

  it should "define construction 3 for contrafunctors" in {
    // If G and H are applicative contrafunctors then G + H is also applicative.

    // Contrafunctor instance:
    implicit def contrafunctorSum[G[_] : Contravariant, H[_] : Contravariant]: Contravariant[Lambda[A ⇒ Either[G[A], H[A]]]] = new Contravariant[Lambda[A ⇒ Either[G[A], H[A]]]] {
      override def contramap[A, B](fa: Either[G[A], H[A]])(f: B ⇒ A): Either[G[B], H[B]] = fa match {
        case Left(ga) ⇒ Left(ga contramap f)
        case Right(ha) ⇒ Right(ha contramap f)
      }
    }

    // Applicative instance:
    implicit def contraaplicativeSum[G[_] : ContraWuZip : Contravariant, H[_] : ContraWuZip : Contravariant]: ContraWuZip[Lambda[A ⇒ Either[G[A], H[A]]]] = new ContraWuZip[Lambda[A ⇒ Either[G[A], H[A]]]] {
      override def wu: Either[G[Unit], H[Unit]] = Left(wU[G]) // We need to make an arbitrary choice between Left() and Right() here.

      override def zip[A, B](fa: Either[G[A], H[A]], fb: Either[G[B], H[B]]): Either[G[(A, B)], H[(A, B)]] = (fa, fb) match {
        case (Left(ga), Left(gb)) ⇒ Left(ga zip gb)
        case (Left(ga), Right(hb)) ⇒ Right(hb contramap { case (x, y) ⇒ y }) // Since the wrapped unit is a `Left()`, we need to create a `Right()` in the product.
        case (Right(ha), Left(gb)) ⇒ Right(ha contramap { case (x, y) ⇒ x })
        case (Right(ha), Right(hb)) ⇒ Right(ha zip hb)
      }
    }
    
    /* Check the laws:
    
    Associativity: Let's describe the computation of `zip` in a way that is clearly associative.
     
    (fa zip fb zip fc) is computed as G's `zip` or H's `zip` if all fa, fb, fc are on one side.
    This is associative.
    
    If some of (fa, fb, fc) are Left() while others are Right(), all the Left() ones are ignored,
    and the remaining Right() ones are converted to Right[(A, B, C)] using contramap on the function
    such as { case (a, b, c) ⇒ (a, b) } as required.
    This is also associative.
    
    Identity laws: Assuming that G's identity law works - and not using H's identity laws - we find: 
    
    (fa zip wu) = (fa zip Left(wU[G]) = (fa, Left(wU[G])) match {
      case (Left(ga), Left(gb)) ⇒ Left(ga zip gb) = Left(ga zip wU[G]) ≅ Left(ga)
      case (Right(ha), Left(gb)) ⇒ Right(ha contramap { case (x, ()) ⇒ x } ≅ Right(ha)
     }
     */
  }
}
