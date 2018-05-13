package example

import org.scalatest.FlatSpec

class Chapter07_02_semimonadsSpec extends FlatSpec with FlattenableLawChecking with CatsLawChecking {

  behavior of "constructions of monads"
  
  it should "verify monad laws for constant functor" in {
    
  }
}
