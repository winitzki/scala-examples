package example

import cats.Monad
import org.scalatest.FlatSpec

class Chapter07_02_semimonadsSpec extends FlatSpec with FlattenableLawChecking with CatsLawChecking {

  behavior of "constructions of monads"
  
  it should "verify monad laws for constant functor" in {
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
  }
}
