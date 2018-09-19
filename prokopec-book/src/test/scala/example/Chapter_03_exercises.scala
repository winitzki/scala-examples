package example

import java.util.concurrent.atomic.AtomicReference

import org.scalatest.{FlatSpec, Matchers}
import shapeless.T

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext
import scala.util.Try

class Chapter_03_exercises extends FlatSpec with Matchers {

  behavior of "exercises"

  it should "implement exercise 1" in {
    class ThisThreadContext1 extends ExecutionContext {
      override def execute(runnable: Runnable): Unit = {
        Try(runnable.run()).recover { case t: Throwable ⇒ reportFailure(t)
        }
      }

      override def reportFailure(cause: Throwable): Unit = println(s"Failure: $cause")
    }

    val tc1 = new ThisThreadContext1

    // Run a task that also uses tc1.execute().
    tc1.execute(() ⇒ {
      println("step 1")
      tc1.execute(() ⇒ println("step 2"))
    })
    println("step 3")
  }

  it should "implement exercise 2" in {
    class TreiberStack[T] {
      private val stack = new AtomicReference[List[T]](Nil)

      @tailrec
      final def push(x: T): Unit = {
        val old = stack.get()
        if (stack.compareAndSet(old, x :: old)) () else push(x)
      }

      @tailrec
      final def pop(): T = {
        val old = stack.get()
        if (old.isEmpty) throw new Exception("stack is empty")
        else if (stack.compareAndSet(old, old.tail))
          old.head
        else pop()
      }

      def toList: List[T] = stack.get
    }

    // Test: simultaneous push and pop many times, must not lose any data.

    val s = new TreiberStack[Int]
    val n = 1000
    // Prepare with n initial elements.
    (1 to n).foreach(s.push)
    // First thread will quickly push `n` elements.
    val t1 = new Thread {
      override def run(): Unit = (n + 1 to n * 2).foreach(s.push)
    }
    // Second thread will quickly pop `n` elements.
    val t2 = new Thread {
      override def run(): Unit = (1 to n).foreach(_ ⇒ s.pop())
    }

    // Run them until completion.
    t2.start() // Start popping.
    Thread.sleep(1) // Give it some time to start.
    t1.start() // Start pushing.
    t1.join()
    t2.join()

    // The stack should contain `n` elements.
    val result = s.toList
    result.length shouldEqual n
    println(result) // Some elements are from the initial list, some were pushed on.
  }

  it should "implement exercise 3" in {
    import scala.collection.SortedSet
    import scala.collection.immutable.TreeSet
    class ConcurrentSortedList[T](implicit val ord: Ordering[T]) {
      val list: AtomicReference[SortedSet[T]] = new AtomicReference(new TreeSet[T]())

      @tailrec
      final def add(x: T): Unit = {
        val old = list.get
        if (list.compareAndSet(old, old + x)) () // Allocate 1 new object.
        else add(x)
      }

      def iterator: Iterator[T] = list.get.iterator
    }

  }

  it should "implement exercise 5" in {
    class LazyCell[T](initialization: ⇒ T) {
      private var value: T = null.asInstanceOf[T]

      def apply(): T = synchronized {
        if (value == null) value = initialization

        value
      }
    }
  }

  it should "implement exercise 6" in {
    class PureLazyCell[T](initialization: ⇒ T) {
      private var value = new AtomicReference[T]()

      @tailrec
      final def apply(): T = {
        val old = value.get
        if (old != null) old
        else if (value.compareAndSet(old, initialization)) value.get
        else apply()
      }
    }
  }
}
