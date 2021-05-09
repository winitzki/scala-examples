package example

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

object DependentPairSpec:

  trait SourceValue[A]:
    type T
    val a: A

// Does not compile but don't know why.
//  final case class DepPair[A](c: [B] => ((a: SourceValue[A]) ⇒ (t: a.T) ⇒ B) ⇒ B) 


class DependentPairSpec extends AnyFlatSpec with Matchers :

  behavior of "dependent pair"
