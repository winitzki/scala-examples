package example

import WuZip.WuZipSyntax
import cats.Functor
import cats.syntax.functor._
import org.scalatest.{FlatSpec, Matchers}
import io.chymyst.ch._

class Chapter_02_exercises extends FlatSpec with Matchers {

  behavior of "exercises"

  it should "implement exercise 1" in {
    def parallel[A, B](a: ⇒ A, b: ⇒ B): (A, B) = {
      var resultA: A = null.asInstanceOf[A]
      var resultB: B = null.asInstanceOf[B]

      val threadA = new Thread {
        resultA = a
      }
      threadA.start()

      val threadB = new Thread {
        resultB = b
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
    def periodically(duration: Long)(b: ⇒ Unit): Unit = {
      new Thread {
        while (true) {
          b
          Thread.sleep(duration)
        }
      }.start()
    }

    var x = 0
    periodically(100) {
      x += 1
      println(s"new value is $x")
    }
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

    def isEmpty: Boolean = (x == null)

    def nonEmpty: Boolean = (x != null)
  }

  it should "implement exercise 3" in {
    val s = new SyncVar[Int]
    the[Exception] thrownBy s.get should have message "cannot get because x is empty"
    s.put(1)
    the[Exception] thrownBy s.put(2) should have message "cannot put because x is not empty"
    s.get shouldEqual 1
    the[Exception] thrownBy s.get should have message "cannot get because x is empty"
  }
  it should "implement exercise 4" in {
  }
}
