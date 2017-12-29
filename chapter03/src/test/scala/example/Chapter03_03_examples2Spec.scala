package example

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.ScalacheckShapeless._
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, TableDrivenPropertyChecks}
import org.scalatest.{Assertion, FlatSpec, Matchers}

class Chapter03_03_examples2Spec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks with TableDrivenPropertyChecks {

  def existsSome[T: Arbitrary](test: T ⇒ Assertion): Assertion = {
    val sampleValues = Iterator.continually(arbitrary[T].sample).collect { case Some(x) ⇒ x }.take(100).toSeq
    val sampleTable = Table("sample", sampleValues: _*)
    exists(sampleTable)(test)
  }

  behavior of "worked examples"

  it should "ex01" in {
    // or:  final case class MyT[T](f: Boolean ⇒ MyTVals[T])
    type MyT[T] = Boolean ⇒ MyTVals[T]

    sealed trait MyTVals[T]
    final case class EmptyValueT[T]() extends MyTVals[T]
    final case class SingleValueT[T](t: T) extends MyTVals[T]
    final case class IntWithT[T](i: Int, t: T) extends MyTVals[T]
    final case class StringToT[T](st: String ⇒ T) extends MyTVals[T]

    val myT: MyT[Int] = b ⇒ if (b) StringToT(_.length) else EmptyValueT()
  }

  it should "ex02" in {
    type T1[A, B, C, D] = (Either[A, B], Either[C, D]) // (A+B)*(C+D)
    // A*C + A*D + B*C + B*D
    sealed trait T2[A, B, C, D]
    final case class T2AC[A, B, C, D](a: A, c: C) extends T2[A, B, C, D]
    final case class T2AD[A, B, C, D](a: A, d: D) extends T2[A, B, C, D]
    final case class T2BC[A, B, C, D](b: B, c: C) extends T2[A, B, C, D]
    final case class T2BD[A, B, C, D](b: B, d: D) extends T2[A, B, C, D]

    def f1[A, B, C, D]: T1[A, B, C, D] ⇒ T2[A, B, C, D] = {
      case (ab, cd) ⇒
        ab match {
          case Left(a) => cd match {
            case Left(c) => T2AC(a, c)
            case Right(d) => T2AD(a, d)
          }
          case Right(b) => cd match {
            case Left(c) => T2BC(b, c)
            case Right(d) => T2BD(b, d)
          }
        }
    }

    def f2[A, B, C, D]: T2[A, B, C, D] ⇒ T1[A, B, C, D] = {
      case T2AC(a, c) => (Left(a), Left(c))
      case T2AD(a, d) => (Left(a), Right(d))
      case T2BC(b, c) => (Right(b), Left(c))
      case T2BD(b, d) => (Right(b), Right(d))
    }

    def check[A: Arbitrary, B: Arbitrary, C: Arbitrary, D: Arbitrary]() = {
      forAll { (t1: T1[A, B, C, D]) ⇒ f2(f1(t1)) shouldEqual t1 }
      forAll { (t2: T2[A, B, C, D]) ⇒ f1(f2(t2)) shouldEqual t2 }
    }

    check[Int, Char, String, Boolean]()
  }

  it should "ex03" in {
    // Show A+A not equivalent to A
    def f1[A]: Either[A, A] ⇒ A = {
      case Left(a) ⇒ a
      case Right(a) ⇒ a
    }

    def f2[A]: A ⇒ Either[A, A] = a ⇒ Right(a) // Must choose Right or Left here!

    existsSome[Either[Int, Int]] { t ⇒ f2(f1(t)) should not equal t }
    forAll { (t: Int) ⇒ f1(f2(t)) shouldEqual t }

    // show A * A not equivalent to A

    def g1[A]: ((A, A)) ⇒ A = _._1 // Must choose the first or the second here!
    def g2[A]: A ⇒ (A, A) = a ⇒ (a, a)

    existsSome[(Int, Int)] { t ⇒ g2(g1(t)) should not equal t }
    forAll { (t: Int) ⇒ g1(g2(t)) shouldEqual t }
  }

  it should "ex04" in {
    // show (A×B)⇒C not equal (A⇒C)+(B⇒C) in logic
    def f1[A, B, C]: (((A, B)) ⇒ C) ⇒ Either[A ⇒ C, B ⇒ C] = { fabc ⇒
      // Need to produce either A ⇒ C or B ⇒ C, and must now decide which one!
      // Suppose we decide to produce A ⇒ C.
      Left((a: A) ⇒
        // Now need to produce a value of type C. But we need both A and B to use `fabc`
        fabc((a, ???)) // We don't have a value of type B here! Can't implement.
      )
    }

    // Works in this direction.
    def f2[A, B, C]: Either[A ⇒ C, B ⇒ C] ⇒ ((A, B)) ⇒ C = {
      case Left(ac) ⇒ pab ⇒ ac(pab._1)
      case Right(bc) ⇒ pab ⇒ bc(pab._2)
    }

  }

  type Reader[E, T] = E ⇒ T

  it should "ex05" in {

    def pure[E, A]: A ⇒ Reader[E, A] = a ⇒ _ ⇒ a

    def map[E, A, B]: Reader[E, A] ⇒ (A ⇒ B) ⇒ Reader[E, B] = { r ⇒
      f ⇒
        // Need to return a function of type E ⇒ B
        e ⇒
          // We need to return a value of type B now.
          // We have a value of type A:
          val a: A = r(e)
          // Now we can get a value of type B by using `f`.
          val b: B = f(a)
          b
    }

    def map1[E, A, B](r: Reader[E, A])(f: A ⇒ B): Reader[E, B] = e ⇒ f(r(e))
  }

  it should "ex06" in {

    // Cannot implement map with respect to the E argument.
    def map[T, A, B](r: Reader[A, T])(f: (A ⇒ B)): Reader[B, T] = {
      b ⇒ // need to produce a t: T
        val a: A = ??? // Need a value of type A here, but don't have it!
      val t: T = r(a)
        t
    }

    // However, we can implement a contraMap:
    def contraMap[T, A, B]: Reader[A, T] ⇒ (B ⇒ A) ⇒ Reader[B, T] = { r ⇒
      f ⇒
        b ⇒ // need to produce a t: T
          val a: A = f(b)
          // Need a value of type A here, produce it using f!
          val t: T = r(a)
          t
    }

  }

  // 1 + A ⇒ (A ⇒ B) ⇒ 1 + B
  it should "ex07" in {
    def map[A, B](optA: Option[A])(f: A ⇒ B): Option[B] = optA match {
      case Some(a) ⇒ Some(f(a))
      case None ⇒ None
    }

    // The other possibility is to always return `None` ("information loss"):
    def mapBad[A, B](optA: Option[A])(f: A ⇒ B): Option[B] = None

    def check[A: Arbitrary]() = {
      forAll { (x: Option[A]) ⇒ map[A, A](x)(identity[A]) shouldEqual x }
      existsSome { (x: Option[A]) ⇒ mapBad[A, A](x)(identity[A]) should not equal x }
    }

    check[Int]()
  }

  it should "ex07.1" in {
    def map[L, R, T](e: Either[L, R])(f: R ⇒ T): Either[L, T] = e match {
      case Right(r) ⇒ Right(f(r))
      case Left(l) ⇒ Left(l)
    }

    // Seq(1,2,3).flatMap(x => Seq(x,x,x))
    // (x: Seq[T]) . flatMap (T => Seq[U]) : Seq[U]

    def flatMap[L, R, T](e: Either[L, R])(f: R ⇒ Either[L, T]): Either[L, T] = e match {
      case Right(r) ⇒ f(r)
      case Left(l) ⇒ Left(l)
    }
  }

  type State[S, A] = S ⇒ (A, S)

  it should "ex08" in {
    def pure[S, A]: A ⇒ State[S, A] = a ⇒ s ⇒ (a, s)

    def map[S, A, B](sa: State[S, A])(f: A ⇒ B): State[S, B] = { s ⇒
      // Need to produce new values of type S and B.
      val (a, newS) = sa(s)
      val b: B = f(a)
      (b, newS) // Would be a shame to ignore `newS`! Returning `s` here would be "information loss".
    }

    def flatMap[S, A, B](sa: State[S, A])(f: A ⇒ State[S, B]): State[S, B] = { s ⇒
      // Need to produce new values of type S and B.
      val (a, newS1) = sa(s)
      val newStateB = f(a)
      val (b, newS2) = newStateB(newS1)
      (b, newS2) // Trying to avoid "information loss", we return newS2 here.
    }
  }

  // NEList[T] ≡ T + T × NEList[T]
  sealed trait NEList[T]
  final case class NEHead[T](head: T) extends NEList[T]
  final case class NETail[T](head: T, tail: NEList[T]) extends NEList[T]

  it should "ex09" in {
    // Not tail-recursive!
    def map[T, U](nEList: NEList[T])(f: T ⇒ U): NEList[U] = nEList match {
      case NEHead(head) ⇒ NEHead(f(head))
      case NETail(head, tail) ⇒ NETail(f(head), map(tail)(f))
    }

    // Not tail-recursive!
    def concat[T](l1: NEList[T], l2: NEList[T]): NEList[T] = l1 match {
      case NEHead(head) ⇒ NETail(head, l2)
      case NETail(head, tail) ⇒ NETail(head, concat(tail, l2))
    }
  }
}
