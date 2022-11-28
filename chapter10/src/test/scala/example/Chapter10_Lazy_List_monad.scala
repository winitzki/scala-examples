package example

import org.scalatest.{BeforeAndAfterEach, FlatSpec, Matchers}

class Chapter10_Lazy_List_monad extends FlatSpec with Matchers with BeforeAndAfterEach {
  final case class Stream[A](head: A)(lazyTail: => Stream[A]) {
    def tail: Stream[A] = lazyTail // Call this when we need to evaluate the tail.

    def map[B](f: A ⇒ B): Stream[B] = Stream(f(head))(lazyTail.map(f))

    def flatMap[B](f: A => Stream[B]): Stream[B] = flatten(map(f))

    def take(n: Int): Seq[A] = if (n <= 0) Seq() else head +: tail.take(n - 1)
  }

  def pure[A](a: A): Stream[A] = Stream(a)(pure(a)) // Recursive call is in the lazy tail.

  def flatten[A](ss: Stream[Stream[A]]): Stream[A] = Stream(ss.head.head)(flatten(ss.tail.map(_.tail)))

  it should "Verify that lazy list works as a monad" in {
    val x = pure(1) // [1, 1, ...]
    val y = pure(x) // [ [1, 1, ...], [1, 1, ...], ...]
    val z = flatten(y) // Again [1, 1, ...]
    z.take(4) shouldEqual Seq(1, 1, 1, 1)

    def naturalsFrom(i: Int): Stream[Int] = Stream(i)(naturalsFrom(i + 1)) // [i, i+1, i+2, ...]

    val naturals = naturalsFrom(1) // [1, 2, 3, ... ]
    naturals.take(4) shouldEqual Seq(1, 2, 3, 4)
    val multTable = naturals.map { i ⇒ naturals.map(_ * i) } // multTable[i, j] = i * j
    multTable.take(4).map(_.take(4)) shouldEqual Seq(Seq(1, 2, 3, 4), Seq(2, 4, 6, 8), Seq(3, 6, 9, 12), Seq(4, 8, 12, 16))
    val squares = flatten(multTable)
    squares.take(4) shouldEqual Seq(1, 4, 9, 16)
    val square1 = naturals.flatMap { i ⇒ naturals.map(_ * i) }
    square1.take(4) shouldEqual Seq(1, 4, 9, 16)

    val square2 = for {
      i ← naturals
      j ← naturals
    } yield i * j
    square2.take(4) shouldEqual Seq(1, 4, 9, 16)
  }
}
