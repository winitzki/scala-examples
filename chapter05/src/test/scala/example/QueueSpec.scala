package example

import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.Queue

// What is faster: a Queue or a Vector? Vector is a lot faster.

class QueueSpec extends FlatSpec with Matchers {
  val n = 10000000

  "vector" should "append and traverse" in {
    val q = Vector[Int]()
    val initTime = System.nanoTime()
    val q1M = (1 to n).foldLeft(q) { case (prev, x) ⇒ prev :+ x }
    val elapsed1 = System.nanoTime() - initTime
    val x = q1M.foreach { i ⇒ }
    val elapsed2 = System.nanoTime() - initTime - elapsed1
    println(s"vector ${elapsed1 / 1000000} ms ${elapsed2 / 1000000} ms")
  }
  
  "queue" should "append and traverse" in {
    val q = Queue[Int]()
    val initTime = System.nanoTime()
    val q1M = (1 to n).foldLeft(q) { case (prev, x) ⇒ prev :+ x }
    val elapsed1 = System.nanoTime() - initTime
    val x = q1M.foreach { i ⇒ }
    val elapsed2 = System.nanoTime() - initTime - elapsed1
    println(s"queue ${elapsed1 / 1000000} ms ${elapsed2 / 1000000} ms")
  }
  
  "vector" should "append and traverse 2" in {
    val q = Vector[Int]()
    val initTime = System.nanoTime()
    val q1M = (1 to n).foldLeft(q) { case (prev, x) ⇒ prev :+ x }
    val elapsed1 = System.nanoTime() - initTime
    val x = q1M.foreach { i ⇒ }
    val elapsed2 = System.nanoTime() - initTime - elapsed1
    println(s"vector ${elapsed1 / 1000000} ms ${elapsed2 / 1000000} ms")
  }

  "queue" should "append and traverse 2" in {
    val q = Queue[Int]()
    val initTime = System.nanoTime()
    val q1M = (1 to n).foldLeft(q) { case (prev, x) ⇒ prev :+ x }
    val elapsed1 = System.nanoTime() - initTime
    val x = q1M.foreach { i ⇒ }
    val elapsed2 = System.nanoTime() - initTime - elapsed1
    println(s"queue ${elapsed1 / 1000000} ms ${elapsed2 / 1000000} ms")
  }
  /*
  vector 912 ms 253 ms
queue 7365 ms 232 ms
vector 1661 ms 152 ms
   */
  
  /*
  vector 899 ms 244 ms
queue 7368 ms 234 ms
vector 1823 ms 152 ms
queue 5697 ms 298 ms

vector 977 ms 77 ms
queue 1747 ms 6522 ms
vector 1258 ms 153 ms
queue 5824 ms 249 ms

vector 1141 ms 86 ms
queue 1722 ms 7541 ms
vector 1439 ms 252 ms
queue 6444 ms 295 ms

vector 922 ms 76 ms
queue 1459 ms 6833 ms
vector 1169 ms 166 ms
queue 6526 ms 442 ms
   */
  
}
