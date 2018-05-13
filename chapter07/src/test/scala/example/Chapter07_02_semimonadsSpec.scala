package example

import cats.Monad
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
    implicit def semimonadAG[G[_], A]: Semimonad[Lambda[X ⇒ (X, G[X])]] = new Semimonad[Lambda[X ⇒ (X, G[X])]] {
      override def flatMap[A, B](fa: (A, G[A]))(f: A ⇒ (B, G[B])): (B, G[B]) = f(fa._1) // Discarding the first effect!
    }
  }
}
