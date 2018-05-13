package example

import cats.syntax.functor._
import cats.{Functor, Monad}
import org.scalatest.FlatSpec

class Chapter07_02_semimonadsSpec extends FlatSpec with FlattenableLawChecking with CatsLawChecking {

  behavior of "constructions of monads"
  
  it should "verify construction 1: constant functor" in {
    type C[Z, A] = Z
    // The functor is going to be C[Z, ?], for a fixed Z.
    implicit def semimonadC[Z]: Semimonad[C[Z, ?]] = new Semimonad[C[Z, ?]] {
      override def flatMap[A, B](fa: C[Z, A])(f: A ⇒ C[Z, B]): C[Z, B] = fa
    }
    
    // Flatten is id. So, associativity is trivial.
    
    // Why is this not a full monad? Because the right identity law fails unless Z = 1.
    // We must have flatten (pure (x: C[Z, A])) = x. But pure: A ⇒ Z must give a fixed value of Z.
    // So pure(x) cannot depend on x, and we cannot restore x if we only know pure(x).
    // Therefore, flatten(pure(x)) cannot compute x.
    // However, if Z = 1 there is only one value of x, so there is no problem and pure(x) returns 1.
  }
  
  it should "verify semimonad construction 2" in {
    // The functor F[A] is defined as (A, G[A]).
    // In the syntax of the "kind projector", it is Lambda[X ⇒ (X, G[X])].
    implicit def semimonadAG[G[_]: Functor]: Semimonad[Lambda[X ⇒ (X, G[X])]] = new Semimonad[Lambda[X ⇒ (X, G[X])]] {
      override def flatMap[A, B](fa: (A, G[A]))(f: A ⇒ (B, G[B])): (B, G[B]) = {
        // The first effect is in `fa`, the second effect is in the result of applying `f`.
        // We discard the second effect.
        val f1: A ⇒ B = f andThen (_._1)
        val (a, ga) = fa
        (f1(a), ga.map(f1))
      }
    }
    
    // Derive flatten from flatMap as flatten(ffa) = ffa.flatMap(id), where id is of type F[F[A]] ⇒ F[F[A]].
    
    def ftn[G[_]: Functor, A](ffa: ((A, G[A]), G[(A, G[A])])): (A, G[A]) = {
      /*
        val f1 = identity andThen (_._1) = _._1
        val (a, ga) = ffa
        (f1(a), ga.map(f1))
       */
      val (a, ga) = ffa
      (a._1, ga.map(_._1))
    }
    
    // If G = 1, we have F[A] = A; this is the Identity monad. flatmap = id, fmap = id, flatten = id, pure = id.
    // All monad laws hold trivially.
  }
  
}
