package example

import example.CollectionAPI._
import example.PipeOps.PipeOp
import example.Utils.elapsed
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class FastComposeSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  behavior of "correctness of FastCompose"

  def makeCollImplicit(limit: Int) = collChain(limit)

  it should "convert a single function to counted function, and check compositions" in {
    implicit val collApi = makeCollImplicit(100)
    val f1: Boolean ⇒ Int = b ⇒ if (b) 10 else 11
    val f2: Int ⇒ String = x ⇒ s"have $x"
    val f3: Int ⇒ Boolean = x ⇒ x % 2 == 0
    forAll { x: Boolean ⇒ x |> f1 |> f3 shouldEqual x }
    f1(true) shouldEqual 10
    f1(false) shouldEqual 11
    f2(1) shouldEqual "have 1"
    val f1c = CountCompose(f1)
    val f2c = CountCompose(f2)
    val f3c = CountCompose(f3)
    f1c.composedCount shouldEqual 1
    f2c.composedCount shouldEqual 1
    f3c.composedCount shouldEqual 1
    f1c(true) shouldEqual 10
    f1c(false) shouldEqual 11
    f2c(1) shouldEqual "have 1"
    forAll { x: Boolean ⇒ x |> f1c |> f3c shouldEqual x }
    val h = f3c andThen f1c andThen f2c
    h(1) shouldEqual "have 11"
    h.composedCount shouldEqual 3
    val k = f2c compose f1c compose f3c
    k(1) shouldEqual "have 11"
    k.composedCount shouldEqual 3

    import FastCompose.FastComposeOps
    val l = f3 before f1 before f2
    l.debugInfo shouldEqual List(3)
    l(1) shouldEqual "have 11"

    val m = f2 after f1 after f3 after f1 after f3
    m.debugInfo shouldEqual List(5)
    m(1) shouldEqual "have 11"
  }

  def postComposeMany[A, Coll[_]: CollectionAPI](count: Int, func: A ⇒ A): A ⇒ A = {
    import FastCompose.FastComposeOps
    (1 to count).foldLeft[A ⇒ A](identity[A])((q, _) ⇒ q before func)
  }

  def preComposeMany[A, Coll[_]: CollectionAPI](count: Int, func: A ⇒ A): A ⇒ A = {
    import FastCompose.FastComposeOps
    (1 to count).foldLeft[A ⇒ A](identity[A])((q, _) ⇒ q after func)
  }

  it should "compose many functions and maintain chains" in {
    val directCompositionLimit = 100
    implicit val collApi = makeCollImplicit(directCompositionLimit)
    val increment = 10
    val f: Int ⇒ Int = x ⇒ x + increment
    val count1 = directCompositionLimit / 2
    val count2 = directCompositionLimit + count1

    val result1Pre = preComposeMany(count1, f)
    val result2Pre = preComposeMany(count2, f)
    val result1Post = postComposeMany(count1, f)
    val result2Post = postComposeMany(count2, f)

    result1Pre(0) shouldEqual increment * count1
    result2Pre(0) shouldEqual increment * count2
    result1Post(0) shouldEqual increment * count1
    result2Post(0) shouldEqual increment * count2

    // ???
    result1Pre.asInstanceOf[FastCompose[_, _, List]].debugInfo shouldEqual List(count1 + 1)
    result1Post.asInstanceOf[FastCompose[_, _, List]].debugInfo shouldEqual List(count1 + 1)
    result2Pre.asInstanceOf[FastCompose[_, _, List]].debugInfo shouldEqual List(count1 + 1, directCompositionLimit)
    result2Post.asInstanceOf[FastCompose[_, _, List]].debugInfo shouldEqual List(directCompositionLimit) ++ List.fill(count1 + 1)(1)

  }

  behavior of "speed of FastCompose"

  it should "build a long FastCompose chain of functions and run it without causing a stack overflow" in {
    val directCompositionLimit = 100
    implicit val collApi = makeCollImplicit(directCompositionLimit)

    val repetitions = 1000
    val count = directCompositionLimit * repetitions
    val f: Int ⇒ Int = x ⇒ x + 1

    val result1Post = elapsed(postComposeMany(count, f))
    println(s"Post-composing $count functions took ${result1Post._2} seconds")
    val result1Pre = elapsed(preComposeMany(count, f))
    println(s"Pre-composing $count functions took ${result1Pre._2} seconds")

    val runPost = elapsed(result1Post._1(0))
    println(s"Running post-composed $count functions took ${runPost._2} seconds")
    val runPre = elapsed(result1Pre._1(0))
    println(s"Running pre-composed $count functions took ${runPre._2} seconds")

    /* Results for Coll = List:

Post-composing 100000 functions took 42.26614339 seconds
Pre-composing 100000 functions took 0.010626068 seconds
Running post-composed 100000 functions took 0.013142418 seconds
Running pre-composed 100000 functions took 0.004555858 seconds

For Coll = Vector:
Post-composing 100000 functions took 0.063813275 seconds
Pre-composing 100000 functions took 0.036010323 seconds
Running post-composed 100000 functions took 0.009538363 seconds
Running pre-composed 100000 functions took 0.004438007 seconds

For Coll = Chain: (correctness not yet established?)

Post-composing 100000 functions took 0.045865894 seconds
Pre-composing 100000 functions took 0.024936107 seconds
Running post-composed 100000 functions took 1.15975E-4 seconds
Running pre-composed 100000 functions took 0.012662047 seconds

     */
  }
}
