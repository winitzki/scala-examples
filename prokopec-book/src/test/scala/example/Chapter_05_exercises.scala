package example

import org.scalatest.{FlatSpec, Matchers}

class Chapter_05_exercises extends FlatSpec with Matchers {

  behavior of "exercises"

  // Timings for a computation.

  def timingsNs(x: ⇒ Unit): Long = {
    val n = 1000
    (1 to n * 10).foreach(_ ⇒ x) // Warm up JVM.
    var elapsed: Long = 0
    (1 to n).foreach { _ ⇒
      val initTime = System.nanoTime()
      x
      elapsed += System.nanoTime() - initTime
    }
    elapsed / n
  }

  val n = 100000

  def timingsWithVolatileNs(x: ⇒ Unit): Long = {
    (1 to n * 10).foreach(_ ⇒ x) // Warm up JVM.
    var elapsed: Long = 0
    @volatile var v: Any = null
    (1 to n).foreach { _ ⇒
      val initTime = System.nanoTime()
      v = x
      elapsed += System.nanoTime() - initTime
    }
    elapsed / n
  }

  it should "exercise 1" in {
    // Timings for creating a new object.
    final class A
    final class B
    val result1 = timingsNs(new A)
    val result2 = timingsWithVolatileNs(new B)
    println(s"With volatile: $result2, without volatile: $result1")
  }
  
  
}
