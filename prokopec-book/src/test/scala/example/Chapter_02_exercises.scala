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

  class SyncVar0[A] {
    // Probably don't need @volatile here.
    var x: A = null.asInstanceOf[A]

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
  }

  it should "implement exercise 3" in {
    val s = new SyncVar0[Int]
    the[Exception] thrownBy s.get should have message "cannot get because x is empty"
    s.put(1)
    the[Exception] thrownBy s.put(2) should have message "cannot put because x is not empty"
    s.get shouldEqual 1
    the[Exception] thrownBy s.get should have message "cannot get because x is empty"
  }

  // Helper functions to work with a SyncVar0, using busy wait.
  @tailrec
  final def busyPut[A](s: SyncVar0[A], x: A): Unit =
    try s.put(x) catch {
      case e: Exception ⇒ busyPut(s, x)
    }

  @tailrec
  final def busyGet[A](s: SyncVar0[A]): A =
    try s.get catch {
      case e: Exception ⇒ busyGet(s)
    }

  val s0 = new SyncVar0[Int]

  def producerThread0(range: Range): Thread = new Thread {
    override def run(): Unit = range.foreach(busyPut(s0, _))
  }

  def consumerThread0(print: Boolean): Thread = new Thread {
    override def run(): Unit = {
      while (true) if (print) println(s"Got value: ${busyGet(s0)}") else busyGet(s0)
    }
  }

  it should "implement exercise 4" in {
    consumerThread0(print = true).start() // Threads can start in any order.
    val t = producerThread0(0 until 15)
    t.start()
    t.join()
  }

  it should "perform stress test for SyncVar0 with busy wait" in {
    val n = 3000 // Heavy contention sets in at around n = 3000.
    val init = System.currentTimeMillis()
    val t1 = producerThread0(1 to n)
    t1.start()
    val t2 = producerThread0(n + 1 to 2 * n)
    t2.start()
    val t3 = producerThread0(2 * n + 1 to 3 * n)
    t3.start()
    consumerThread0(print = false).start()
    consumerThread1(print = true).start()
    t1.join()
    t2.join()
    t3.join()
    println(s"Elapsed time: ${System.currentTimeMillis() - init}")
  }

  class SyncVar1[A] {
    var x: A = null.asInstanceOf[A]
    val monitor = new AnyRef
    val monitorPut = new AnyRef
    val monitorGet = new AnyRef

    def getWait: A = monitorPut.synchronized {
      while (x == null) monitorPut.wait()
      monitor.synchronized {
        monitorGet.synchronized {
          val result = x
          x = null.asInstanceOf[A]
          monitorGet.notify()
          result
        }
      }
    }

    def putWait(a: A): Unit = monitorGet.synchronized {
      while (x != null) monitorGet.wait()
      monitor.synchronized {
        monitorPut.synchronized {
          x = a
          monitorPut.notify()
        }
      }
    }

    /* This is a deadlock.
    def getWait: A = monitorPut.synchronized {
      while (x == null) monitorPut.wait()
      monitorGet.synchronized {
        val result = x
        x = null.asInstanceOf[A]
        monitorGet.notify()
        result
      }
    }

    def putWait(a: A): Unit = monitorGet.synchronized {
      while (x != null) monitorGet.wait()
      monitorPut.synchronized {
        x = a
        monitorPut.notify()
      }
    }
    */

    /* This is a deadlock.
    def getWait: A = {
      println("DEBUG: entering getWait")
      monitor.synchronized {
        println(s"DEBUG: getWait acquired monitor; have x = $x")
        while (x == null) monitor.wait()
        println(s"DEBUG: getWait has non-null x = $x, proceeding")
        val result = x
        x = null.asInstanceOf[A]
        monitor.notify()
        result
      }
    }

    def putWait(a: A): Unit = {
      println(s"DEBUG: entering putWait($a)")
      monitor.synchronized {
        println(s"DEBUG: putWait($a) acquired monitor; have x = $x")
        while (x != null) monitor.wait()
        println(s"DEBUG: putWait($a) has null x = $x, proceeding")
        x = a
        monitor.notify()
      }
    }

    */
    /* This is also a deadlock.
    def getWait: A = monitorPut.synchronized {
      while (x == null) monitorPut.wait()
      monitorGet.synchronized {
        val result = x
        x = null.asInstanceOf[A]
        monitorGet.notify()
        result
      }
    }

    def putWait(a: A): Unit = monitorGet.synchronized {
      monitorPut.synchronized {
        while (x == null) monitorPut.wait()
        val result = x
        x = null.asInstanceOf[A]
        monitorGet.notify()
        result
      }
    }
    */
    /* This is also a deadlock.
    @volatile var getResult: A = null.asInstanceOf[A]

    def getWait: A = {
      while (maybeGet()) waitForPut()
      getResult
    }

    def putWait(a: A): Unit = {
      while (maybePut(a)) waitForGet()
    }

    def maybeGet(): Boolean = monitorGet.synchronized {
      if (x != null) {
        getResult = x
        monitorGet.notify()
        true
      } else false
    }

    def maybePut(a: A): Boolean = monitorPut.synchronized {
      if (x == null) {
        x = a
        monitorPut.notify()
        true
      } else false
    }

    def waitForGet(): Unit = monitorGet.synchronized {
      while (x != null) monitorGet.wait()
    }

    def waitForPut(): Unit = monitorPut.synchronized {
      while (x == null) monitorPut.wait()
    }
    */
  }

  val s1 = new SyncVar1[Int]

  def producerThread1(range: Range): Thread = new Thread {
    override def run(): Unit = range.foreach(s1.putWait)
  }

  def consumerThread1(print: Boolean): Thread = new Thread {
    override def run(): Unit = {
      while (true) if (print) println(s"Got value: ${
        s1.getWait
      }") else s1.getWait
    }
  }

  it should "implement exercise 5" in {
    consumerThread1(print = true).start()
    val t = producerThread1(0 until 15)
    t.start()
    t.join()
  }

  it should "perform stress test for SyncVar1 with idle wait" in {
    val n = 500000 // 16 seconds; this means 10µs per put/get
    val init = System.currentTimeMillis()
    val t1 = producerThread1(1 to n)
    t1.start()
    val t2 = producerThread1(n + 1 to 2 * n)
    t2.start()
    val t3 = producerThread1(2 * n + 1 to 3 * n)
    t3.start()
    consumerThread1(print = false).start()
    consumerThread1(print = false).start()
    t1.join()
    t2.join()
    t3.join()
    println(s"Elapsed time: ${
      System.currentTimeMillis() - init
    }")
  }
}
