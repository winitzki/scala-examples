package example

import WuZip.WuZipSyntax
import cats.Functor
import cats.syntax.functor._
import org.scalatest.{FlatSpec, Matchers}
import io.chymyst.ch._

import scala.annotation.tailrec

class Chapter_02_exercises extends FlatSpec with Matchers {

  behavior of "exercises"

  it should "implement exercise 1" in {
    def parallel[A, B](a: ⇒ A, b: ⇒ B): (A, B) = {
      var resultA: A = null.asInstanceOf[A]
      var resultB: B = null.asInstanceOf[B]

      val threadA = new Thread {
        override def run(): Unit = {
          resultA = a
        }
      }
      threadA.start()

      val threadB = new Thread {
        override def run(): Unit = {
          resultB = b
        }
      }
      threadB.start()

      threadA.join()
      threadB.join()

      (resultA, resultB)
    }

    (0 to 100).foreach { i ⇒
      parallel(s"first thread $i", s"second thread $i") shouldEqual ((s"first thread $i", s"second thread $i"))
    }
  }

  it should "implement exercise 2" in {
    def periodically(duration: Long)(b: ⇒ Unit): Thread = {
      val t = new Thread {
        override def run(): Unit = {
          while (true) {
            b
            Thread.sleep(duration)
          }
        }
      }
      t.start()
      t
    }

    var x = 0
    var time = System.currentTimeMillis()
    val t = periodically(100) {
      x += 1
      val currentTime = System.currentTimeMillis()
      val elapsed = currentTime - time
      time = currentTime
      println(s"new value is $x after time $elapsed")
    }
    Thread.sleep(1000)
    t.interrupt()
  }

  class SyncVar[A] {
    @volatile var x: A = null.asInstanceOf[A]

    def get: A = this.synchronized {
      if (x != null) {
        val result = x
        x = null.asInstanceOf[A]
        result
      } else throw new Exception("cannot get because x is empty")
    }

    def put(a: A): Unit = this.synchronized {
      if (x != null) throw new Exception("cannot put because x is not empty")
      else if (a == null) throw new Exception("cannot put a null value")
      else x = a
    }

    def isEmpty: Boolean =
      x == null

    def nonEmpty: Boolean =
      x != null
  }

  it should "implement exercise 3" in {
    val s = new SyncVar[Int]
    the[Exception] thrownBy s.get should have message "cannot get because x is empty"
    s.put(1)
    the[Exception] thrownBy s.put(2) should have message "cannot put because x is not empty"
    s.get shouldEqual 1
    the[Exception] thrownBy s.get should have message "cannot get because x is empty"
  }

  // Helper functions to work with a SyncVar, using busy wait.
  @tailrec
  final def busyPut[A](s: SyncVar[A], x: A): Unit =
    try s.put(x) catch {
      case e: Exception ⇒ busyPut(s, x)
    }

  @tailrec
  final def busyGet[A](s: SyncVar[A]): A =
    try s.get catch {
      case e: Exception ⇒ busyGet(s)
    }

  it should "implement exercise 4" in {
    val s = new SyncVar[Int]

    val producerThread = new Thread {
      override def run(): Unit = {
        (0 until 15).foreach(busyPut(s, _))
      }
    }

    val consumerThread = new Thread {
      override def run(): Unit = {
        while (true) println(s"Got value: ${busyGet(s)}")
      }
    }
    consumerThread.start() // Threads can start in any order.
    producerThread.start()
    Thread.sleep(1000)
  }

}
