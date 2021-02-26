package example
/*
import example.CollectionAPI._
import example.FastCompose.FastComposeOps
import example.PipeOps.PipeOp
import example.Utils.elapsed
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class FastComposeSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  behavior of "correctness of FastCompose"

  def makeCollImplicit(limit: Int) = collArrayList(limit)

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
    forAll { x: Boolean ⇒ x |> f1c.f |> f3c.f shouldEqual x }
    val h = f3c andThen f1c andThen f2c
    h(1) shouldEqual "have 11"
    h.composedCount shouldEqual 3
    val k = f2c compose f1c compose f3c
    k(1) shouldEqual "have 11"
    k.composedCount shouldEqual 3

    val l = f3 before f1 before f2
    l.debugInfo shouldEqual List(3)
    l(1) shouldEqual "have 11"

    val m = f2 after f1 after f3 after f1 after f3
    m.debugInfo shouldEqual List(5)
    m(1) shouldEqual "have 11"
  }

  it should "maintain optimal chains when length of chain is one" in {
    implicit val collApi = makeCollImplicit(3)
    val i = identity[Int] _
    val f = FastCompose.of(i)
    val g = f before i before i before i before i before i
    g.debugInfo shouldEqual List(3, 3)
  }

  def postComposeMany[A, Coll[_] : CollectionAPI](count: Int, func: A ⇒ A): A ⇒ A = {
    (1 to count).foldLeft[A ⇒ A](identity[A])((q, _) ⇒ q before func)
  }

  def preComposeMany[A, Coll[_] : CollectionAPI](count: Int, func: A ⇒ A): A ⇒ A = {
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

    result1Pre.asInstanceOf[FastCompose[_, _, List]].debugInfo shouldEqual List(count1 + 1)
    result1Post.asInstanceOf[FastCompose[_, _, List]].debugInfo shouldEqual List(count1 + 1)
    result2Pre.asInstanceOf[FastCompose[_, _, List]].debugInfo shouldEqual List(count1 + 1, directCompositionLimit)
    result2Post.asInstanceOf[FastCompose[_, _, List]].debugInfo shouldEqual List(directCompositionLimit) ++ List.fill(count1 + 1)(1)
  }

  def createManyPostComposed[Coll[_] : CollectionAPI](count: Int): (Double, Double) = {
    val (bigCompose, elapsedTime) = elapsed {
      (1 to count).foldLeft[Int ⇒ Int](identity)((f, i) ⇒ f before { x: Int ⇒ x * i } before { x: Int ⇒ x - i + 2 } before { x: Int ⇒ x / 2 })
    }
//    bigCompose.asInstanceOf[FastCompose[_, _, List]].debugInfo shouldEqual Nil
    println(s"Composing ${4 * count} functions using `before` took $elapsedTime seconds")
    val (result, elapsedTime2) = elapsed {
      bigCompose(1)
    }
    println(s"Running ${4 * count} function compositions took $elapsedTime2 seconds")
    result shouldEqual 1
    (elapsedTime, elapsedTime2)
  }

  def createManyPreComposed[Coll[_] : CollectionAPI](count: Int): (Double, Double) = {
    val (bigCompose, elapsedTime) = elapsed {
      (1 to count).foldLeft[Int ⇒ Int](identity)((f, i) ⇒ f after { x: Int ⇒ x / 2 } after { x: Int ⇒ x - i + 2 } after { x: Int ⇒ x * i })
    }
    println(s"Composing ${4 * count} functions using `after` took $elapsedTime seconds")
    val (result, elapsedTime2) = elapsed {
      bigCompose(1)
    }
    println(s"Running ${4 * count} function compositions took $elapsedTime2 seconds")
    result shouldEqual 1
    (elapsedTime, elapsedTime2)
  }

  it should "verify correct operation for non-commuting function compositions" in {
    val count = 100000
    val directCompositionLimit = 100
    implicit val collApi = makeCollImplicit(directCompositionLimit)
    //    createManyPreComposed(count)
    createManyPostComposed(count)
    /* Coll = ArrayList:

limit = 100


Composing 400000 functions using `before` took 0.04870757 seconds
Running 400000 function compositions took 0.013332446 seconds
Composing 400000 functions using `after` took 0.033828999 seconds
Running 400000 function compositions took 0.005606974 seconds


Composing 400000 functions using `after` took 0.052702859 seconds
Running 400000 function compositions took 0.006050703 seconds
Composing 400000 functions using `before` took 0.025413493 seconds
Running 400000 function compositions took 0.019187712 seconds

Composing 400000 functions using `before` took 0.058695421 seconds
Running 400000 function compositions took 0.013247751 seconds

limit = 100
Composing 400000 functions using `before` took 0.052108728 seconds
Running 400000 function compositions took 0.016391999 seconds

limit = 23
Composing 400000 functions using `before` took 0.067324518 seconds
Running 400000 function compositions took 0.017766714 seconds

limit = 5
Composing 400000 functions using `before` took 0.058479536 seconds
Running 400000 function compositions took 0.015547031 seconds

limit = 250
Composing 400000 functions using `before` took 0.071186712 seconds
Running 400000 function compositions took 0.01800784 seconds

limit = 500
Composing 400000 functions using `before` took 0.068326724 seconds
Running 400000 function compositions took 0.021399614 seconds

limit = 1000
Composing 400000 functions using `before` took 0.072005326 seconds
Running 400000 function compositions took 0.017424277 seconds

     */
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

For Coll = Chain:

Post-composing 100000 functions took 51.30325707 seconds
Pre-composing 100000 functions took 0.023182832 seconds
Running post-composed 100000 functions took 0.008583027 seconds
Running pre-composed 100000 functions took 0.005280912 seconds

Coll = ArrayList

Post-composing 100000 functions took 0.018733719 seconds
Pre-composing 100000 functions took 0.014695719 seconds
Running post-composed 100000 functions took 0.011991254 seconds
Running pre-composed 100000 functions took 0.006037905 seconds

Coll = ArrayBuffer

Post-composing 100000 functions took 4.163173013 seconds
Pre-composing 100000 functions took 0.01365162 seconds
Running post-composed 100000 functions took 0.00769413 seconds
Running pre-composed 100000 functions took 0.004622205 seconds

     */
  }

  it should "find the best value of the composition limit" in {
    val count = 1000
    val result = for {directCompositionLimit ← Seq(50, 100, 250, 500, 1000, 2500, 5000, 10000)} yield {
      implicit val collApi = makeCollImplicit(directCompositionLimit)
      createManyPreComposed(count) // Priming.
      createManyPreComposed(count)
      val (x1, y1) = createManyPreComposed(count)
      createManyPostComposed(count)
      createManyPostComposed(count)
      val (x2, y2) = createManyPostComposed(count)
      directCompositionLimit → (x1, y1, x2, y2)
    }
    println(result)
  }
  /*
  count = 1000000
  List((10,(4.8418053,0.05670215,0.308235505,0.063292302)), (25,(0.886090931,0.054338366,0.177003849,0.041491461)), (50,(0.207546162,0.100359183,0.189550943,0.050619677)), (100,(0.158267517,0.053095597,0.110744037,0.065348905)), (250,(0.150410882,0.067370248,0.134540593,0.054340362)), (500,(0.088916163,0.077178511,0.092972602,0.047302149)), (1000,(0.090675661,0.069217648,0.125723396,0.049708735)))

List((100,(0.325119938,0.050617831,0.331057854,0.056823426)), (250,(0.196862743,0.046109383,0.200273083,0.049606873)), (500,(0.074490338,0.052585702,0.182320997,0.062874869)), (1000,(0.116098613,0.041743209,0.11251719,0.053354365)), (2500,(0.234507355,0.043497473,0.1152335,0.065734138)), (5000,(0.092904441,0.073487101,0.090821451,0.055292123)))

With priming:
List((50,(0.33683487,0.042499188,0.184516974,0.044210406)), (100,(0.142561414,0.06555719,0.259711171,0.074015237)), (250,(0.15676254,0.086068574,0.246768819,0.071605716)), (500,(0.150476854,0.09355668,0.243468672,0.079160082)), (1000,(0.150113705,0.092442202,0.232436276,0.072355475)), (2500,(0.147476658,0.102836838,0.229639997,0.070580472)), (5000,(0.147906618,0.092157602,0.250010924,0.071155097)), (10000,(0.147953079,0.096197923,0.270158878,0.06759879)))

   */
}
*/