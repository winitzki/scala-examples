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

object HList1a {
  // Trying to fix the unusability of HList1.
  trait HL {
    type Head

    def |:[A](head: A): HL = |:(head, this)
  }

  final case object HN extends HL {
    type Head = Nothing
  }

  final case class |:[A](head: A, tail: HL) extends HL {
    type Head = A
  }

  private val example1: HL = HN
  private val example2: HL = 1 |: HN
  private val example3: HL = 1 |: "xyz" |: HN

  // Cannot read the types inside HL - they are all hidden.
  // HL = 1 + ∃A. A × HL
  def headOption(hl: HL): Option[hl.Head] = hl match {
    case HN ⇒ None
    case h |: _ ⇒ Some(h.asInstanceOf[hl.Head]) // A typecast is required here.
  }
  // The type of a HList does not show whether the list is empty or what types are in it.
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

  final case class |:[H, Tail <: HList](head: H, tail: Tail) extends HList {
    override type prepend[A] = A |: H |: Tail

    override def ::[A](a: A): prepend[A] = |:(a, this)
  }

  final case object HNil extends HList {
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

// see https://apocalisp.wordpress.com/2010/07/06/type-level-programming-in-scala-part-6a-heterogeneous-list%C2%A0basics/
object HList4 {
  sealed trait HList {
    final def |:[A](a: A): A |: this.type = new |:(a, this)
  }

  final case class |:[H, Tail <: HList](head: H, tail: Tail) extends HList

  final case object HNil extends HList

  type HNil = HNil.type

  implicit final class PostfixHList[A](val x: A) extends AnyVal {
    def | : (A |: HNil) = x |: HNil
  }

  import scala.language.postfixOps

  val example1 = 1 |: "abc" |: true.|
  val x: Boolean |: HNil = example1.tail.tail

  val y: String = example1 match {
    case _ |: z |: _ |: HNil ⇒ z
  }
}

class Chapter08_03_mapN_with_HList extends FlatSpec with Matchers {

  it should "define mapN using type-level list" in {

  }
}
