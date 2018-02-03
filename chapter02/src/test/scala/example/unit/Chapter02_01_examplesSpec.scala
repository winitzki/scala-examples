package example.unit

import org.scalatest.{FlatSpec, Matchers}

class Chapter02_01_examplesSpec extends FlatSpec with Matchers {

  behavior of "Examples"

  def ex1(a: Seq[Double]): Seq[(Double, Double)] = {
    a.map(x ⇒ (math.cos(x), math.sin(x)))
  }

  it should "run ex1" in {
    ex1(Seq(0, 0.1, 0.2)) shouldEqual Seq(
      (math.cos(0), math.sin(0)),
      (math.cos(0.1), math.sin(0.1)),
      (math.cos(0.2), math.sin(0.2))
    )
  }

  def ex2(a: Seq[Double]): Int = {
    a.count(x ⇒ math.cos(x) > math.sin(x))

  }

  it should "run ex2" in {
    ex2(Seq(0, 0.1, 0.2)) shouldEqual 3
  }

  def ex3(a: Seq[Double], b: Seq[Double]): Seq[Double] = {
    val x: Seq[(Double, Double)] = a.zip(b)

    x.map { case (p, q) ⇒ p - q }
  }

  it should "run ex3" in {
    ex3(Seq(0, 0.1, 0.2), Seq(1.0, 2.0, 3.0)) shouldEqual Seq(-1.0, -1.9, -2.8)
  }

  // math.sqrt((x + 1) * 2)
  // val b = x + 1
  // val c = b * 2
  // val d = math.sqrt(c)
  // d

  def ex4(a: Seq[Int]): Int = {
    //    val b: Seq[Int] = a.drop(1)
    //    val c: Seq[(Int, Int)] = a.zip(b)
    //    c.count { case (x, y) ⇒ x > y }
    a.zip(a.drop(1)).count { case (x, y) ⇒ x > y }
  }

  it should "run ex4" in {
    ex4(Seq(1, 3, 2, 4)) shouldEqual 1
  }

  def ex5(k: Int, a: Seq[Int]): Seq[Int] = {
    val b = a.sliding(2 * k + 1)
    b.map(l ⇒ l.max).toSeq
  }

  it should "run ex5" in {
    ex5(1, Seq(1, 5, 2, 10, 4, -1, 20)) shouldEqual Seq(5, 10, 10, 10, 20)
  }

  def ex6(size: Int): Map[(Int, Int), Int] = {
    val a = 1 to size
    a.flatMap(i ⇒ a.map(j ⇒
      (i, j) -> i * j // a -> b  is the same as (a, b)
    )).toMap
  }

  it should "run ex6" in {
    ex6(3) shouldEqual Map(
      (1, 1) → 1,
      (1, 2) → 2,
      (1, 3) → 3,
      (2, 1) → 2,
      (2, 2) → 4,
      (2, 3) → 6,
      (3, 1) → 3,
      (3, 2) → 6,
      (3, 3) → 9
    )
  }

  def ex7(a: Seq[Double]): Double = {
    val b = a.flatMap(x ⇒ Seq(x, math.cos(x), math.sin(x)))
    b.max
  }

  it should "run ex7" in {
    ex7(Seq(0.0, 0.1, 0.5, 1.0, 2.0)) shouldEqual 2.0
  }

  def ex8(m: Map[String, String]): Map[String, String] = {
    m.map { case (name, address) ⇒ address → name }
  }

  it should "run ex8" in {
    val data = Map("user1" → "address1", "user2" → "address2")
    val expected = Map("address1" → "user1", "address2" → "user2")
    ex8(data) shouldEqual expected
  }

  def ex8a[X, Y](m: Map[X, Y]): Map[Y, X] = {
    m.map { case (x, y) ⇒ y → x }
  }

  it should "run ex8 with type parameters" in {
    val data =
      Map(1 → ("address1", true), 2 → ("address2", false))
    val expected =
      Map(("address1", true) → 1, ("address2", false) → 2)
    ex8a(data) shouldEqual expected
  }
}
