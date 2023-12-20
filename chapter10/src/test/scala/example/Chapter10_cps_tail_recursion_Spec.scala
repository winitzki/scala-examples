package example

import io.chymyst.ch.implement
import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec

class Chapter10_cps_tail_recursion_Spec extends FlatSpec with Matchers {

  sealed trait T2[A]

  final case class Leaf[A](a: A) extends T2[A]

  final case class Branch[A](l: T2[A], r: T2[A]) extends T2[A]

  final case class Cont[A, R](run: (A => R) => R) {
    def map[B](f: A => B): Cont[B, R] = implement

    def flatMap[B](f: A => Cont[B, R]): Cont[B, R] = implement
  }

  object Cont {
    def pure[A, R](a: A): Cont[A, R] = Cont(f => f(a))
  }

  object Trampoline {
    @tailrec
    def run[A](trampoline: Trampoline[A]): A = {
      trampoline match {
        case Done(v) => v
        case More(t) => run(t())
      }
    }
  }

  sealed trait Trampoline[A] {
    def map[B](f: A => B): Trampoline[B]

    def flatMap[B](f: A => Trampoline[B]): Trampoline[B]

  }

  case class Done[A](value: A) extends Trampoline[A] {
    override def map[B](f: A => B): Trampoline[B] = Done(f(value))

    override def flatMap[B](f: A => Trampoline[B]): Trampoline[B] = f(value)
  }

  case class More[A](call: () => Trampoline[A]) extends Trampoline[A] {
    override def map[B](f: A => B): Trampoline[B] = More(() => call().map(f))

    override def flatMap[B](f: A => Trampoline[B]): Trampoline[B] = More(() => f(Trampoline.run(call())))
  }

  def makeLongTree(n: Int): T2[Int] = (1 to n).foldLeft(Leaf(0): T2[Int])((a, b) => Branch(a, Leaf(b)))

  it should "fail to run fmap on Tree non-tail-recursively" in {

    def fmapNotTailRec[A, B](f: A => B): T2[A] => T2[B] = {
      case Leaf(a) => Leaf(f(a))
      case Branch(l, r) => Branch(fmapNotTailRec(f)(l), fmapNotTailRec(f)(r))
    }

    // Works until about 3000.
    fmapNotTailRec[Int, Int](i => i + 1)(makeLongTree(2000))

    the[StackOverflowError] thrownBy {
      fmapNotTailRec[Int, Int](i => i + 1)(makeLongTree(3000))
    } should have message (null)

  }

  it should "fail to run fmap on Tree non-tail-recursively with CPS" in {

    def fmapCPS[A, B, C](f: A => B): T2[A] => Cont[T2[B], C] = {
      case Leaf(a) => Cont.pure(Leaf(f(a)))
      case Branch(l, r) => //Branch(fmapNotTailRec(f)(l), fmapNotTailRec(f)(r))
        val x: Cont[T2[B], C] = for {
          left <- fmapCPS(f)(l)
          right <- fmapCPS(f)(r)
        } yield Branch(left, right)
        x
    }

    the[StackOverflowError] thrownBy {
      fmapCPS[Int, Int, Int](i => i + 1)(makeLongTree(4000))
    } should have message (null)

    // Works until about 3000.
    fmapCPS[Int, Int, Int](i => i + 1)(makeLongTree(2000))

  }

  it should "still fail to run fmap on Tree non-tail-recursively with CPS" in {

    def fmapCPS[A, B, C](f: A => B, t2: T2[A], k: T2[B] => C): C = t2 match {
      case Leaf(a) => Cont.pure(Leaf(f(a))).run(k)
      case Branch(l, r) =>
        fmapCPS[A, B, C](f, l, lb => fmapCPS[A, B, C](f, r, rb => k(Branch(lb, rb))))
    }

    // Works until about 1000. Even worse.
    val result: T2[Int] = fmapCPS[Int, Int, T2[Int]](i => i + 1, makeLongTree(1000), identity)

    the[StackOverflowError] thrownBy {
      val result2: T2[Int] = fmapCPS[Int, Int, T2[Int]](i => i + 1, makeLongTree(2000), identity)

    } should have message (null)

  }

  it should "fail to run fmap on Tree with trampolines and for/yield" in {
    def fmapTrampoline[A, B](f: A => B): T2[A] => Trampoline[T2[B]] = {
      case Leaf(a) => Done(Leaf(f(a)))
      case Branch(l, r) => //Branch(fmapNotTailRec(f)(l), fmapNotTailRec(f)(r))
        for {
          left <- fmapTrampoline(f)(l)
          right <- fmapTrampoline(f)(r)
        } yield Branch(left, right)
    }

    // Works until about 1000. Even worse.
    val result = fmapTrampoline[Int, Int](i => i + 1)(makeLongTree(2000))

    the[StackOverflowError] thrownBy {
      val result2 = fmapTrampoline[Int, Int](i => i + 1)(makeLongTree(3000))

    } should have message (null)
  }
  // We would like to be able to implement stack safety for:
  // - arbitrary recursive functions where f(x) = g(x, f) and g may call f several times with arbitrary arguments
  // - building up a recursive data structure using a recursive function
  // - building up a recursive data structure from its recursion scheme using `unfold`
}