package example

import scala.concurrent.ExecutionContext.Implicits.global
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

class Chapter_05_exercises extends FlatSpec with Matchers {

  behavior of "exercises"

  // Timings for a computation.

  def timingsNs(n: Int, x: ⇒ Unit): Long = {
    (1 to n * 10).foreach(_ ⇒ x) // Warm up JVM.
    var elapsed: Long = 0
    (1 to n).foreach { _ ⇒
      val initTime = System.nanoTime()
      x
      elapsed += System.nanoTime() - initTime
    }
    elapsed / n
  }

  def timingsWithVolatileNs(n: Int, x: ⇒ Unit): Long = {
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
    val n = 100000
    val result1 = timingsNs(n, new A)
    val result2 = timingsWithVolatileNs(n, new B)
    println(s"With volatile: $result2, without volatile: $result1")
    // With volatile: 39, without volatile: 34
  }

  def randomChar(probSpace: Float): Char = {
    if (scala.util.Random.nextFloat() <= probSpace)
      ' '
    else
      scala.util.Random.nextPrintableChar()
  }

  def randomString(len: Int, probSpace: Float): String = (1 to len)
    .map(_ ⇒ randomChar(probSpace))
    .mkString("")

  def randomStringPar(len: Int, probSpace: Float): String = (1 to len)
    .par.aggregate[String]("")((s, _) ⇒ s + randomChar(probSpace), _ + _)

  def countSpaces(s: String): Int = s.count(_ == ' ')

  def countSpacesPar(s: String): Int = s.par.count(_ == ' ')

  it should "exercise 2" in {
    val n = 1000
    val p = 0.01f

    println(s"Generate random string: sequential ${timingsWithVolatileNs(n, randomString(n, p))}, parallel ${timingsWithVolatileNs(n, randomStringPar(n, p))}")
    // Generate random string: sequential 57675, parallel 220061 

    val result1 = timingsWithVolatileNs(n, countSpaces(randomString(n, p)))
    val result2 = timingsWithVolatileNs(n, countSpacesPar(randomString(n, p)))

    println(s"Count spaces: sequential $result1, parallel $result2")
    // Count spaces: sequential 48411, parallel 120573
  }

  it should "count spaces in parallel using futures" in {
    def countSpacesFut(s: String): Future[Int] = {
      if (s.length < 3) Future(s.count(_ == ' '))
      else {
        val (s1, s2) = s.splitAt(s.length / 2)
        for {
          c1 ← countSpacesFut(s1)
          c2 ← countSpacesFut(s2)
        } yield c1 + c2
      }
    }

    val n = 1000
    val p = 0.01f

    val r = randomString(n, p)

    println(s"Count spaces with futures: ${timingsWithVolatileNs(n, Await.result(countSpacesFut(r), Duration.Inf))}")
    // 3626342
  }

}
