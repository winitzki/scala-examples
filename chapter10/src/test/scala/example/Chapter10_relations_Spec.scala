package example

import org.scalatest.{FlatSpec, Matchers}

class Chapter10_relations_Spec extends FlatSpec with Matchers {

  object Rel {
    val maxCount = 10000

    val boolseq: Seq[Boolean] = Seq(false, true)
    val intseq: Seq[Int] = (1 to maxCount).flatMap(x ⇒ (-x to x by x))
  }

  import Rel._

  abstract class Rel[A, B] {
      self ⇒
    def check(a: A, b: B): Boolean

    final def apply(a: A, b: B): Boolean = check(a, b)

    def find(a: A): Seq[B]

    def compose[C](other: Rel[B, C]): Rel[A, C] = new Rel[A, C] {
      def findB(a: A, c: C): Seq[B] = self.find(a).filter(b ⇒ self.check(a, b) && other.check(b, c))

      override def check(a: A, c: C): Boolean = findB(a, c).nonEmpty

      override def find(a: A): Seq[C] = self.find(a).flatMap(b ⇒ other.find(b).filter(c ⇒ other.check(b, c)))
    }
  }

  behavior of "Relations"

  it should "compose relations" in {
    // An example of a many-to-many relation that is not equivalent to a function
    val r1: Rel[Int, Int] = new Rel[Int, Int] {
      override def check(a: Int, b: Int): Boolean = a % 2 == b % 3

      override def find(a: Int): Seq[Int] = intseq.map(_ * 3 + (a % 2))
    }
    val r2: Rel[Int, Int] = new Rel[Int, Int] {
      override def check(a: Int, b: Int): Boolean = a % 3 == b % 5

      override def find(a: Int): Seq[Int] = intseq.map(_ * 5 + (a % 3))
    }
    val r3: Rel[Int, Int] = r1 compose r2

    r1(1, 1) shouldEqual true
    r1(1, 2) shouldEqual false
    r1(2, 3) shouldEqual true
    r1(4, 6) shouldEqual true
    r1(5, 7) shouldEqual true

    r2(1, 1) shouldEqual true
    r2(1, 2) shouldEqual false
    r2(3, 5) shouldEqual true
    r2(6, 10) shouldEqual true
    r2(7, 11) shouldEqual true

    r1.find(1).nonEmpty shouldEqual true

    r3(1, 1) shouldEqual true
    r3(1, 2) shouldEqual false
    r3(2, 5) shouldEqual true
    r3(4, 10) shouldEqual true
    r3(5, 11) shouldEqual true
    r3(23, 24) shouldEqual false
  }
}
