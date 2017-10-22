package example

object Example01_01_functions {
  def factorial(n: Int): Int = {
    (1 to n).product
  }

  def is_prime(n: Int): Boolean = {
    (2 until n).forall(i ⇒ n % i != 0)
  }

  def count_even(s: Set[Int]): Int = {
    def is_even(k: Int): Int = if (k % 2 == 0) 1 else 0

    s.toSeq.map(k ⇒ is_even(k)).sum
  }
}
