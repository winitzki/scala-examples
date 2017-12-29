package example.unit

import example.Chapter01_01_functions
import org.scalatest.{FlatSpec, Matchers}

class Chapter01_01_functionsSpec extends FlatSpec with Matchers {

  behavior of "FP examples"

  it should "compute correct factorial values" in {
    Chapter01_01_functions.factorial(0) shouldEqual 1
    Chapter01_01_functions.factorial(1) shouldEqual 1
    Chapter01_01_functions.factorial(2) shouldEqual 2
    Chapter01_01_functions.factorial(6) shouldEqual 720
  }

  it should "compute correct values of is_prime" in {
    Chapter01_01_functions.is_prime(2) shouldEqual true
    Chapter01_01_functions.is_prime(59) shouldEqual true
    Chapter01_01_functions.is_prime(121) shouldEqual false
  }

  it should "compute correct values for count_even" in {
    Chapter01_01_functions.count_even(Set()) shouldEqual 0
    Chapter01_01_functions.count_even(Set(1)) shouldEqual 0
    Chapter01_01_functions.count_even(Set(2)) shouldEqual 1
    Chapter01_01_functions.count_even((1 to 6).toSet) shouldEqual 3
  }

  it should "compute correct values for count_even_using_val" in {
    Chapter01_01_functions.count_even(Set()) shouldEqual 0
    Chapter01_01_functions.count_even(Set(1)) shouldEqual 0
    Chapter01_01_functions.count_even(Set(2)) shouldEqual 1
    Chapter01_01_functions.count_even((1 to 6).toSet) shouldEqual 3
  }
}
