package example

import org.scalatest.{FlatSpec, Matchers}

object HList1 {
  // Trying to define it quick and dirty. The result is unusable.
  trait HL {
    def |:[A](head: A): HL = |:(head, this)
  }

  final case object HN extends HL

  final case class |:[A](head: A, tail: HL)

  private val example1: HL = HN
  private val example2: HL = 1 |: HN
  private val example3: HL = 1 |: "xyz" |: HN

  // Cannot read the types inside HL - they are all hidden.
  // HL = 1 + ∃A. A × HL
  def head: HL ⇒ Any = ???
}

object HList2 {
  // Trying to capture the type of the list in the type argument. The result goes out of hand quickly.
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

// Code taken from https://gist.github.com/dragisak/10a3a5837662cdaa29db
object HList3 {
  // Works but cannot pattern match on types.
  sealed trait HList {
    type prepend[A] <: HList

    def ::[A](a: A): prepend[A]
  }

  case class |:[H, Tail <: HList](head: H, tail: Tail) extends HList {
    override type prepend[A] = |:[A, |:[H, Tail]]

    override def ::[A](a: A): prepend[A] = |:(a, this)
  }

  case object HNil extends HList {
    override type prepend[A] = |:[A, HNil.type]

    override def ::[A](a: A): prepend[A] = |:(a, this)
  }

  type HNil = HNil.type

  private val example1: Int |: HNil = 1 :: HNil

  //  def head[A](hl: A |: HList): A = hl.head // Not needed since we already called them `head` and `tail`.

  private val example2: Int = example1.head

  val example3: Int = example1 match {
    case a |: _ ⇒ a
  }

  // Would like to have a type function to determine the type of head. Not sure if it is possible.
}

class Chapter08_03_mapN_with_HList extends FlatSpec with Matchers {

  it should "define mapN using type-level list" in {

  }
}
