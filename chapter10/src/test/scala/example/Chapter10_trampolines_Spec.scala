package example

import org.scalatest.{FlatSpec, Matchers}

import scala.util.control.TailCalls._

class Chapter10_trampolines_Spec extends FlatSpec with Matchers {

  def f_not_tailrec(x: Int): Int = if (x == 0) 0 else 1 + f_not_tailrec(x - 1)

  val n = 100000

  it should "generate stack overflow with non-tailrec function" in {
    f_not_tailrec(10) shouldEqual 10

    the[StackOverflowError] thrownBy {
      f_not_tailrec(n)
    } should have message null
  }

  def f_1(x: Int): TailRec[Int] = if (x == 0) done(0) else tailcall(f_1(x - 1).map(_ + 1))

  it should "fix stack overflow when using TailRec" in {
    f_1(n).result shouldEqual n
  }

  sealed trait T2[A]

  final case class L[A](value: A) extends T2[A]

  final case class B[A](left: T2[A], right: T2[A]) extends T2[A]

  def large(size: Int): T2[Int] = if (size == 0) L(0) else B(L(size), large(size - 1))

  it should "fail to produce a large tree" in {

    the[StackOverflowError] thrownBy {
      large(n)
    } should have message null
  }

  def large_with_tailrec(size: Int): TailRec[T2[Int]] = if (size == 0) done(L(0)) else for {
    next <- tailcall(large_with_tailrec(size - 1))
  } yield B(L(size), next)


  it should "succeed to produce a large tree with TailRec" in {
    large_with_tailrec(n).result
  }

  sealed trait Step[A] {
    def map[B](f: A => B): Step[B] = this match {
      case Stop(value) => Stop(f(value))
      case Move(defer) => Move(() => defer().map(f))
    }

    def flatMap[B](f: A => Step[B]): Step[B] = this match {
      case Stop(value) => f(value)
      case Move(defer) => Move(() => defer().flatMap(f))
    }

    def result: A = this match {
      case Stop(value) => value
      case Move(defer) => defer().result
    }
  }

  final case class Stop[A](value: A) extends Step[A]

  final case class Move[A](defer: () => Step[A]) extends Step[A]


  def large_with_steps(size: Int): Step[T2[Int]] = if (size == 0) Stop(L(0)) else for {
    next <- Move(() => large_with_steps(size - 1))
  } yield B(L(size), next)

  it should "fail to fix stack overflow when using the Step monad" in {
    the[StackOverflowError] thrownBy {
      large_with_steps(n).result
    } should have message null
  }
}