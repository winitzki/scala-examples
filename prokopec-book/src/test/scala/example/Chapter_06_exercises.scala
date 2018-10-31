package example

import org.scalatest.{FlatSpec, Matchers}
import rx.lang.scala._
import scala.concurrent.duration._

class Chapter_06_exercises extends FlatSpec with Matchers {
  behavior of "reactive"

  it should "exercise 2 not working" in {
    val timed5 = Observable.interval(50.millis).map(_ * 5)
    val timed12 = Observable.interval(120.millis).map(_ * 12)
    val result = (timed5 merge timed12)
      .filter { _ % 30 != 0}.distinct

    result.foreach(println)

    Thread.sleep(2000)
  }

  it should "exercise 2 working" in {
    val result = Observable.interval(10.millis)
      .filter(x ⇒ (x % 5 == 0 || x % 12 == 0) && x % 30 != 0)

    result.foreach(println)

    Thread.sleep(2000)
  }
  
  it should "exercise 3" in {
    // Simple average of a finite stream, emitted as the single value in an Observable.
    def average(x: Observable[Double]) = for {
      sum ← x.sum
      length ← x.length
    } yield sum / length
    
    val c = Observable.from(0 to 100).map(_.toDouble)
    val c1 = c.sum
    val c2 = c.length
    average(c).foreach(println) // prints 50.0
    
    // Moving average.
    val x: Observable[Double] = Observable.from(1 to 100).map(_.toDouble)
    val y: Observable[Observable[Double]] = x.sliding(10, 1)
    val z: Observable[Double] = y.flatMap(x ⇒ average(x))
    
    z.foreach(println) // This prints nothing???
  }
}
