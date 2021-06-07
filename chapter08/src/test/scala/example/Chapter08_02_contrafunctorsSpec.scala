package example

import cats.syntax.contravariant._
import cats.syntax.functor._
import cats.{Contravariant, Functor}
import example.ContraWuZip._
import org.scalatest.{FlatSpec, Matchers}

class Chapter08_02_contrafunctorsSpec extends FlatSpec with Matchers {

  behavior of "applicative contrafunctor constructions"

  // Convert C[((A, B), C)] ⇒ C[(A, (B, C))]
  // need f: ( (A, (B, C)) ) ⇒ ((A, B), C)

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
        case (Left(_), Right(hb)) ⇒ Right(hb contramap { case (a, b) ⇒ b }) // Since the wrapped unit is a `Left()`, we need to create a `Right()` in the product.
        case (Right(ha), Left(_)) ⇒ Right(ha contramap { case (a, b) ⇒ a })
        case (Right(ha), Right(hb)) ⇒ Right(ha zip hb)
      }
    }

    /* Check the laws:
    
    Associativity: Let's reformulate the computation of `zip` of 3 arguments in a way that is manifestly associative.
     
    (fa zip fb zip fc) is computed as G's `zip` or H's `zip` if all fa, fb, fc are on one side.
    This is associative.
    
    If some of (fa, fb, fc) are Left() while others are Right(), all the Left() ones are ignored,
    and the remaining Right() ones are converted to Right[H[(A, B, C)]] using contramap on the function
    such as { case (a, b, c) ⇒ (a, b) } as required.
    This is also associative.
    
    Identity laws: Assuming that G's identity law works - and not using H's identity laws - we find:
    
    (fa zip wu) = (fa zip Left(wU[G])) = (fa, Left(wU[G])) match {
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
        val ha: H[A] = hab map { case (a, b) ⇒ a } // hab map (_._1)
        val hb: H[B] = hab map { case (a, b) ⇒ b }
        fa(ha) zip fb(hb)
      }
    }

    /* Check the laws:
    
    Associativity:
    
    Consider `(fa zip fb) zip fc: H[((A, B), C)] ⇒ G[((A, B), C)]`:
    
    fa zip fb = { hab ⇒ fa(hab map(_._1)) zip fb(hab map(_._2)) }
    (fa zip fb) zip fc
     = { habc: H[((A, B), C)] ⇒ (fa zip fb)(habc map(_._1) zip fc(habc map (_._2))
     = { habc: H[((A, B), C)] ⇒ (fa(habc map(_._1) map (_._1)) zip fb(habc map (_._1) map (_._2)) zip fc(habc map (_._2))
    
    To simplify the data structure, let's flatten the tuple type, from ((A, B), C) to (A, B, C):
    
    We will need to map h_abc: H[(A, B, C)] as h_abc map { case (a, b, c) ⇒ ((a, b), c) } and call that habc in the expression above.
    Note that we have expressions such as `habc map (_._1) map (_._1)`.
    These are simplified as h_abc map { case (a, b, c) ⇒ ((a, b), c)._1._1 } = h_abc map (_._1)
    and so on. Thus:
    
    ((fa zip fb) zip fc) contramap { case (a, b, c) ⇒ ((a, b), c) } = { habc: H[(A, B, C)] ⇒
      ((fa(h_abc map(_._1)) zip fb(h_abc map (_._2))) zip fc(h_abc map (_._3)) contramap { case (a, b, c) ⇒ ((a, b), c) }
    }
    
    The last step is G's zip, which is associative by assumption, followed by an isomorphism transformation. 
    
    Similarly, starting with `fa zip (fb zip fc): H[(A, (B, C))] ⇒ G[(A, (B, C))]` and applying `contramap { case (a, b, c) ⇒ (a, (b, c)) }`, we will find:
    
    (fa zip (fb zip fc)) contramap { case (a, b, c) ⇒ (a, (b, c)) } = { habc: H[(A, B, C)] ⇒
      (fa(h_abc map(_._1)) zip (fb(h_abc map (_._2)) zip fc(h_abc map (_._3))) contramap { case (a, b, c) ⇒ (a, (b, c)) }
    }
      
    The associativity of G's zip shows that this is the same expression after the isomorphism.
      
    Identity:
    
    Compute zip(fa, _ ⇒ wU[G]) = { hab ⇒ ... fa(ha) zip (_ ⇒ wU[G])(hb) }
    Clearly (_ ⇒ wU[G])(hb) = wU[G]. Hence we get fa(ha) zip wU[G].
    Use G's identity law for its `zip`, and find that fa(ha) is just mapped from G[A] into G[(A, Unit)].
    This is the isomorphism we expect.
    
    So the identity laws hold. 
     */
  }

  it should "verify that exponential construction does not work for applicative functors" in {
    def withParams[P, Q]() = {
      import io.chymyst.ch._
      type S[A] = (A ⇒ P) ⇒ Q

      "def zip[A, B](p: S[A], q: S[B]): ((A, B) ⇒ P) ⇒ Q = implement" shouldNot typeCheck

      "def zip[A, B](p: S[A], q: S[B]): S[(A, B)] = implement" shouldNot typeCheck

      type R[A] = (A ⇒ P) ⇒ Option[A]

      def zip[A, B]: (R[A], R[B]) ⇒ R[(A, B)] = implement

      val zip1 = zip.lambdaTerm.prettyPrint
      zip1 shouldEqual "a ⇒ b ⇒ (None() + 0)"
    }

    withParams[Int, String]()
  }


  it should "verify run-time error in invalid nesting" in {
    """val result = for {
      x ← List(1, 2)
      (a, (b, c)) ← List(1, 2, 3) zip List("a", "b", "c") zip List(true, false, true)
    } yield a""" shouldNot compile

  }
}
