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

  it should "define construction 4 for contrafunctors" in {
    // If H[A] is any functor and G[A] is a contrafunctor then H[A] ⇒ G[A] is applicative.

    // Contrafunctor instance:
    implicit def contrafunctor4[G[_] : Contravariant, H[_] : Functor]: Contravariant[Lambda[A ⇒ H[A] ⇒ G[A]]] = new Contravariant[Lambda[A ⇒ H[A] ⇒ G[A]]] {
      override def contramap[A, B](fa: H[A] ⇒ G[A])(f: B ⇒ A): H[B] ⇒ G[B] = { hb ⇒ fa(hb map f) contramap f }
    }

    // Applicative instance:
    implicit def contraaplicative4[G[_] : ContraWuZip : Contravariant, H[_] : Functor]: ContraWuZip[Lambda[A ⇒ H[A] ⇒ G[A]]] = new ContraWuZip[Lambda[A ⇒ H[A] ⇒ G[A]]] {
      override def wu: H[Unit] ⇒ G[Unit] = { _ ⇒ wU[G] }

      override def zip[A, B](fa: H[A] ⇒ G[A], fb: H[B] ⇒ G[B]): H[(A, B)] ⇒ G[(A, B)] = { hab ⇒
        val ha = hab map { case (a, b) ⇒ a }
        val hb = hab map { case (a, b) ⇒ b }
        fa(ha) zip fb(hb)
      }
    }

    /* Check the laws:
    
    Associativity:
    
    Consider (fa zip fb zip fc): H[(A, B, C)] ⇒ G[(A, B, C)].
    This computation will proceed as
      val ha = habc map { case (a, b, c) ⇒ a }
      val hb = habc map { case (a, b, c) ⇒ b }
      val hc = habc map { case (a, b, c) ⇒ c }
      fa(ha) zip fb(hb) zip fc(hc)
    The steps computing ha, hb, hc are associative because they are just deconstructing nested tuples, which are associative.
    The last step is G's zip, so it is associative by assumption. 
      
    Identity:
    
    Compute zip(fa, _ ⇒ wU[G]) = { hab ⇒ ... fa(ha) zip (_ ⇒ wU[G])(hb) }
    Clearly (_ ⇒ wU[G])(hb) = wU[G]. Hence we get fa(ha) zip wU[G].
    Use G's identity law for its `zip`, and find that fa(ha) is just mapped from G[A] into G[(A, Unit)].
    This is the isomorphism we expect.
    
    So the identity laws hold. 
     */
  }

  it should "define construction 5 for contrafunctors" in {
    // If G[A] is a functor and H[A] is a contrafunctor, both applicative, then G[H[A]] is an applicative contrafunctor.

    // Contrafunctor instance:
    implicit def contrafunctor5[G[_] : Functor, H[_] : Contravariant]: Contravariant[Lambda[A ⇒ G[H[A]]]] = new Contravariant[Lambda[A ⇒ G[H[A]]]] {
      override def contramap[A, B](gha: G[H[A]])(f: B ⇒ A): G[H[B]] = gha.map(_ contramap f)
    }

    // Applicative instance:
    implicit def contraaplicative5[G[_] : WuZip : Functor, H[_] : ContraWuZip : Contravariant]: ContraWuZip[Lambda[A ⇒ G[H[A]]]] = new ContraWuZip[Lambda[A ⇒ G[H[A]]]] {
      override def wu: G[H[Unit]] = WuZip[G].pure(wU[H])

      import WuZip.WuZipSyntax

      override def zip[A, B](gha: G[H[A]], ghb: G[H[B]]): G[H[(A, B)]] = (gha zip ghb).map { case (ha, hb) ⇒ ha zip hb }
    }

    /* Check the laws:
    
    Follow the same proof as in construction 8 for applicative functors.
    We use `map2` and `pureG` for G, but we never use `map2` or `pure` for `H` in that proof.
    We only use the laws of H's `zip`. Therefore, the same proof goes through here.
    
     */
  }
}
