package example

import java.util.concurrent.atomic.AtomicReference

import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
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

  it should "blocking wait" in {
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.blocking
    Future { blocking { Thread.sleep(1000000) } }
    // The `global` execution context will now add 1 more thread, to improve parallelism.
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
    val n = 10000
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
        val newList = old + x
        if (list.compareAndSet(old, newList)) () // Allocate 1 new object.
        else add(x)
      }

      def iterator: Iterator[T] = list.get.iterator
    }
    val s = new ConcurrentSortedList[Int]
    val n = 1000
    // Prepare with n initial elements.
    (1 to n).foreach(s.add)
    // First thread will quickly push `n` elements.
    val t1 = new Thread {
      override def run(): Unit = (n + 1 to n * 2).foreach(s.add)
    }
    // Second thread will quickly pop `n` elements.
    val t2 = new Thread {
      override def run(): Unit = (n*2+1 to n*3).foreach(s.add)
    }

    // Run them until completion.
    t2.start() // Start popping.
    Thread.sleep(1) // Give it some time to start.
    t1.start() // Start pushing.
    t1.join()
    t2.join()
    val result = s.iterator.toList
    result.length shouldEqual n*3
    println(result)
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
      private val value = new AtomicReference[T]()

      @tailrec
      final def apply(): T = {
        val old = value.get
        if (old != null) old
        else if (value.compareAndSet(old, initialization)) value.get
        else apply()
      }
    }
  }

  it should "implement exercise 7" in {
    class SyncConcurrentMap[K, V] extends scala.collection.concurrent.Map[K, V] {
      private val data = scala.collection.mutable.Map[K, V]()

      override def putIfAbsent(k: K, v: V): Option[V] = synchronized {
        data.get(k) match {
          case Some(_) ⇒ None
          case None ⇒
            data.update(k, v)
            Some(v)
        }
      }

      override def remove(k: K, v: V): Boolean = synchronized {
        data.remove(k).nonEmpty
      }

      override def replace(k: K, oldvalue: V, newvalue: V): Boolean = synchronized {
        data.get(k) match {
          case Some(oldV) ⇒
            (oldV == oldvalue) && {
              data.update(k, newvalue)
              true
            }
          case None ⇒ false
        }
      }

      override def replace(k: K, v: V): Option[V] = synchronized {
        data.get(k) map {
          oldV ⇒
            data.update(k, v)
            oldV
        }
      }

      override def +=(kv: (K, V)): SyncConcurrentMap.this.type = synchronized {
        data.update(kv._1, kv._2)
        this
      }

      override def -=(key: K): SyncConcurrentMap.this.type = synchronized {
        data.remove(key)
        this
      }

      override def get(key: K): Option[V] = data.get(key)

      override def iterator: Iterator[(K, V)] = data.iterator // Not much we can do to synchronize.
    }
  }
}
