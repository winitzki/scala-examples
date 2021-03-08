package example

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PHOAS1Spec extends AnyFlatSpec with Matchers :

  behavior of "PHOAS implemented in Scala 3"

  def lazyFix[A](f: A ⇒ A): A = f(lazyFix(f))

  // Compute the factorial function as a fixpoint:
  // factorial  =  μ f. λ n → if (n ≡ 0) then 1 else n * f(n − 1)
  it should "implement lazy fixpoint" in {
    val factorial = lazyFix { (f: Int ⇒ Int) ⇒ (n: Int) ⇒ if n == 0 then 1 else n * f(n - 1) }
    factorial(10) shouldEqual 3628800
  }  
