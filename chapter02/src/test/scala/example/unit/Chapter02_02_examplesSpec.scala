package example.unit

import cats.Monad
import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec

class Chapter02_02_examplesSpec extends FlatSpec with Matchers {

  behavior of "Examples"

  def ex01(f: Int ⇒ Int, init: Int, limit: Int): Int = {
    Iterator.iterate(init)(f).takeWhile(_ <= limit).size
  }

  it should "run ex01" in {
    ex01(x ⇒ 2 * x + 1, 1, 1000) shouldEqual 9
  }

  def ex02(a: Seq[Int], k: Int): Int = {
    val init: Seq[Int] = Seq.fill(k)(Int.MinValue)
    val r: Seq[Int] = a.foldLeft[Seq[Int]](init) { case (seq, x) ⇒ (seq :+ x).sorted.reverse.take(k) }
    r.last
  }

  it should "run ex02" in {
    ex02(Seq(1, 10, -5, 0, 20, 3, 100), 3) shouldEqual 10
  }

  @tailrec final def ex03(a: Seq[Int]): Int = a match {
    case Seq(x) ⇒ x
    case _ ⇒ ex03(a.drop(1))
  }

  it should "run ex03" in {
    ex03(Seq(1, 10, 5)) shouldEqual 5
  }

  @tailrec final def ex04(arr: Array[Int], x: Int): Int = arr match {
    case Array(z) ⇒ z
    case _ ⇒
      val i = arr.length / 2
      val (left, right) = arr.splitAt(i)
      // (0, i - 1) and (i, length - 1)
      if (x >= arr(i)) { // (i, length - 1)
        ex04(right, x)
      } else { // (0, i - 1)
        ex04(left, x)
      }
  }

  it should "run ex04" in {
    ex04(Array(1, 2, 5, 10, 20, 25), 15) shouldEqual 10
    ex04(Array(1, 2, 5, 10, 20, 25), 10) shouldEqual 10
  }

  def sumOfDigits(n: Int): Int = {
    val digits: Iterator[Int] = Iterator.iterate((n, 0)) { case (m, _) ⇒ (m / 10, m % 10) }.takeWhile { case (m, d) ⇒ m > 0 || d > 0 }.drop(1).map(_._2)
    digits.sum
  }

  def ex05(n: Int): Iterator[Int] = {
    Iterator.iterate(sumOfDigits(n))(sumOfDigits)
  }

  it should "run ex05" in {
    ex05(1234).take(5).toList shouldEqual List(10, 1, 1, 1, 1)
  }

  def ex06[T](s: Iterator[T]): Iterator[T] = {
    s.map(x ⇒ Iterator.fill(2)(x)).flatten
  }

  it should "run ex06" in {
    ex06(ex05(1234)).take(6).toList shouldEqual List(10, 10, 1, 1, 1, 1)
  }

  def ex07[T](s: Seq[T]): Seq[T] = {
    // s0, s1, s2, s3, s4, s5, s6, s7, s8, ...
    // s0, s0, s1, s1, s2, s2, s3, s3, s4, ...

    Seq(s.head) ++ s.toIterator
      .zip(ex06(s.toIterator)) // Iterator[(T, T)]
      .drop(1) // remove first pair, which is always (s0, s0)
      .takeWhile { case (x, y) ⇒ x != y } // cut the sequence when repetition occurs
      .map { case (x, y) ⇒ x } // drop the second element
      .toSeq
  }

  it should "run ex07" in {
    ex07(Seq(1, 3, 5, 7, 3, 5, 7, 3, 5, 7, 3, 5, 7)) shouldEqual
      Seq(1, 3, 5, 7, 3)
  }
}
