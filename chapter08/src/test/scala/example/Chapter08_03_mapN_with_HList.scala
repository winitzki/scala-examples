package example

import org.scalatest.{FlatSpec, Matchers}

import scala.::

object HList1 {

  trait HL {
    def |:[A](head: A): HL = |:(head, this)
  }

  final case object HN extends HL

  final case class |:[A](head: A, tail: HL)

  private val example1: HL = HN
  private val example2: HL = 1 |: HN
  private val example3: HL = 1 |: "xyz" |: HN

  // Cannot read the types inside HL - they are all hidden. HL = 1 + ∃A. A × HL
  def head: HL ⇒ Any = ???
}

object HList2 {

  trait HL[B <: HL[B]] {
    def |:[A](head: A): HL[A |: B] = new |:[A, B](head, this)
  }

  final case object HN extends HL[Nothing]

  final case class |:[A, B <: HL[B]](head: A, tail: HL[B]) extends HL[A |: B]

  private val example1: HL[Nothing] = HN
  private val example2: HL[Int |: Nothing] = 1 |: HN
  private val example3: HL[Int |: (String |: Nothing)] = 1 |: "xyz" |: HN

  // Not clear how to develop a type notation for this. We are using GADTs to create a list at the type level.

  // The `head` function must return an initially unknown type `A`. So, it must have `A` as a type parameter.
  def head[A, B <: HL[B], C <: HL[C]](hl: HL[C]): A = ???

  private val example4 = head[Int, Nothing, Int |: Nothing](example2) // This does not work.
}

class Chapter08_03_mapN_with_HList extends FlatSpec with Matchers {

  it should "define mapN using type-level list" in {

  }
}
