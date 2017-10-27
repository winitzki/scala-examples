package example

object Chapter01_02_examples {
  def ex01(x: Int): Int = x + 20

  val ex01a: Int ⇒ Int = x ⇒ x + 20

  def ex02(x: Int): (Int ⇒ Int) = y ⇒ y + x

  val ex02a: Int ⇒ (Int ⇒ Int) = x ⇒ (y ⇒ y + x)

  def ex03(x: Int): Boolean = !Chapter01_01_functions.is_prime(x)

  val ex03a: Int ⇒ Boolean = x ⇒ !Chapter01_01_functions.is_prime(x)

  def ex04(s: Seq[Double]): Double = s.sum / s.size

  def ex05(n: Int): Double = (1 to n)
    .map { i ⇒ (2 * i).toDouble / (2 * i - 1) * (2 * i) / (2 * i + 1) }
    .product

  def ex06(s: Seq[Set[Int]]): Seq[Set[Int]] = s.filter(t ⇒ t.size >= 3)
}
