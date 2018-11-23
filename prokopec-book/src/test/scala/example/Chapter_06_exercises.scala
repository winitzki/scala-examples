package example

import org.scalatest.{FlatSpec, Matchers}
import rx.{Rx, Var}
import rx.lang.scala._

import scala.concurrent.duration._
import rx.async._
import rx.async.Platform._

class Chapter_06_exercises extends FlatSpec with Matchers {
  behavior of "reactive"

  it should "exercise 2 not working" in {
    val timed12 = Observable.interval(120.millis).map(_ * 12)
    val timed5 = Observable.interval(50.millis).map(_ * 5)
    val result = (timed12 merge timed5)
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
    
    def mySum(x: Observable[Double]) = x.sum.flatMap( s ⇒ 
      x.length.map(l ⇒ s)
    )
//      for {
//      s ← x.sum
//      l ← x.length
//    } yield s

      val c = Observable.from(0 to 100).map(_.toDouble)
      average(c).foreach(println) // prints 50.0

      // Sliding window average.
      val x: Observable[Double] = c // Observable.from(0 to 100).map(_.toDouble)
      val y: Observable[Observable[Double]] = x.sliding(10, 1)
      val z = y.flatMap(mySum)

      z.foreach(println)
    } // This prints nothing???
  
}
