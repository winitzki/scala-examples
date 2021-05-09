package example

import org.scalatest.{FlatSpec, Matchers}

object DependentPairSpec {

  /*
  A "dependent pair" is a pair of values whose first element `a` is of type `A`,
   and the second element is of type `B` that depends on the *value* `a`.

   Scala allows us to have curried functions whose first argument is `a`
   and the second argument is `a.T` if `a` has a type member called T:

   def f[A](a: A)(b: a.T) = ...

   This can work only if `a` contains a type member. So, let us wrap `A` into a `SourceValue` trait that has a type member.

   But we want a value that represents the dependent pair, not a function that consumes it (an "eliminator").

   To convert an eliminator into a constructor, we use the Church encoding:

   The type forall B. (X => B) => B  is equivalent to X (by the Yoneda lemma)

   So, if we have an eliminator X => B, we use it to create values of type forall B. (X => B) => B
   */

  // This type represents a pair (A, Type). The type T depends on the value of type SourceValue.
  sealed trait SourceValue[A] {
    type T
    val a: A
  }

  final case class SV[A, R](a: A) extends SourceValue[A] {
    override type T = R
  }

  implicit class SourceValueOps[A](a: A) {
    def toType[B]: SourceValue[A] = SV[A, B](a)
  }

  // Auxiliary type. This is used as an argument in the Church encoding.
  trait DependentPairArg[A, Result] {
    def fromPair(a: SourceValue[A])(t: a.T): Result
  }

  // This will represent the dependent pair. It is forall Result: (fromPair
  trait DependentPair[A] {
    def pair[Result](arg: DependentPairArg[A, Result]): Result

    type Second
  }

  def makePair[A](a: SourceValue[A])(t: a.T): DependentPair[A] = new DependentPair[A] {
    override def pair[Result](arg: DependentPairArg[A, Result]): Result = arg.fromPair(a)(t)

    override type Second = a.T
  }

  def fromPairFirst[A](dp: DependentPair[A]): A = dp.pair(new DependentPairArg[A, A] {
    override def fromPair(a: SourceValue[A])(t: a.T): A = a.a
  })

  def fromPairSource[A](dp: DependentPair[A]): SourceValue[A] = dp.pair(new DependentPairArg[A, SourceValue[A]] {
    override def fromPair(a: SourceValue[A])(t: a.T): SourceValue[A] = a
  })

  def fromPairSecond[A](dp: DependentPair[A]): dp.Second = dp.pair[dp.Second](new DependentPairArg[A, dp.Second] {
    override def fromPair(a: SourceValue[A])(t: a.T): dp.Second = t.asInstanceOf[dp.Second]
  })
}

class DependentPairSpec extends FlatSpec with Matchers {

  import DependentPairSpec._

  behavior of "dependent pair"

  it should "create a dependent pair" in {

  makePair[Int](SourceValue[Int, Boolean](1))
  }

}
