package example

import java.nio.ByteBuffer
import java.nio.channels.{AsynchronousFileChannel, CompletionHandler}
import java.nio.file.{Paths, StandardOpenOption}

import cats.syntax.functor._
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}

class ContinuationMonadPresentation extends FlatSpec with Matchers {

  behavior of "Programming with callbacks"

  it should "define functions that pass results via callbacks, and perform calculations with them" in {
    def add3(x: Int)(k: Int ⇒ Unit): Unit = {
      val result = x + 3
      k(result)
      ()
    }

    def mul4(x: Int)(k: Int ⇒ Unit): Unit = {
      val result = x * 4
      k(result)
      ()
    }

    def const(x: Int)(k: Int ⇒ Unit): Unit = {
      val result = x
      k(result)
      ()
    }

    var res = 0
    // Computation: 10 * 4 + 3 = 43
    const(10) { r1 ⇒
      mul4(r1) { r2 ⇒
        add3(r2) { r3 ⇒
          res = r3 // Store the result.
        }
      }
    }
    res shouldEqual 43
  }

  // A callback API may return immediately and schedule the callback to be run later (or not at all!).

  it should "define functions that pass results via callbacks, and perform calculations asynchronously" in {
    def add3(x: Int)(k: Int ⇒ Unit): Unit = {
      Future {
        val result = x + 3
        k(result)
      }
      ()
    }

    def mul4(x: Int)(k: Int ⇒ Unit): Unit = {
      Future {
        val result = x * 4
        k(result)
      }
      ()
    }

    def const(x: Int)(k: Int ⇒ Unit): Unit = {
      Future {
        val result = x
        k(result)
      }
      ()
    }

    var res = 0

    const(10) { r1 ⇒
      mul4(r1) { r2 ⇒
        add3(r2) { r3 ⇒
          res = r3 // Store the result.
        }
      }
    }

    Thread.sleep(200) // Uh... nothing else we could do here.
    res shouldEqual 43
  }

  /*
  Problems with callbacks:
  
  - don't know when (and whether) the callback was actually called
  - can't easily wait until something happens, unless our code is within the deepest callback
  - code is deeply nested, it is difficult to pass values from one place to another
  - callbacks are not composable: information is passed via side effects or mutable values 
  - information flow is obscured once we start passing callbacks around ("callback hell")

  */

  // How could we make the callback API "composable"?

  it should "make callback API composable" in {
    def add3(x: Int)(k: Int ⇒ Unit): Unit = k(x + 3)

    def mul4(x: Int)(k: Int ⇒ Unit): Unit = k(x * 4)

    def const(x: Int)(k: Int ⇒ Unit): Unit = k(x)

    // We would like to take `const(10)` and `mul4(_)` and "compose" them. How?
    val c10: (Int ⇒ Unit) ⇒ Unit = const(10)

    /*
    Examine the types:
    The function const(10) registers a callback:
    c10 : (Int ⇒ Unit) ⇒ Unit

    Denote for brevity the "register callback" type by RC:
    */
    type RC[A] = (A ⇒ Unit) ⇒ Unit

    val step1: Int ⇒ RC[Int] = (x: Int) ⇒ mul4(x)

    // Can we write `c40 = c10 @@ step1` to "compose" them, and the result should be the same as c40 here:
    val c40: RC[Int] = const(40)
    // In other words, x ⇒ mul4(x) means that we take x = 10 and apply mul4(_) to it.
    // The operation `@@` should know that a function such as x ⇒ mul4(x) needs to operate on the previously computed x.

    // So that later we can write
    val step2: Int ⇒ RC[Int] = (x: Int) ⇒ add3(x)

    // c43 = c40 @@ step2

    val c43: RC[Int] = const(43)

    /*
    What is needed for us to be able to implement the operation `@@` ?

    We have c10 : RC[Int] and step1: Int ⇒ RC[Int]
    So, the operation @@ should have type signature (RC[Int], Int ⇒ RC[Int]) ⇒ RC[Int]
    Let's implement it slightly more generally, with type parameters A and B:
    */

    implicit class C1[A](rc: RC[A]) {  // RC[A] = (A ⇒ Unit) ⇒ Unit
      def @@[B](f: A ⇒ RC[B]): RC[B] = { (cb: B ⇒ Unit) ⇒
        rc(a ⇒ f(a)(cb))
      }
    }

    // Now the syntax works:

    val result: RC[Int] = c10 @@ step1 @@ step2

    // To test this, extract the value:
    def extractValue(rc: RC[Int]): Int = {
      var result = 0
      rc(result = _)
      result
    }

    extractValue(result) shouldEqual 43

    // The type signature of @@ is similar to that of `flatMap`.
    // We also have a function `const` of type `Int ⇒ RC[Int]`, 
    // but we can easily generalize to `A ⇒ RC[A]`.
    // This type signature is that of `pure`.
    // Therefore, `RC` is a monad. This is called the *continuation monad*.
    // Let us implement the standard type signatures:

    def pure[A](a: A): RC[A] = { (ca: A ⇒ Unit) ⇒ ca(a) }

    implicit class C2[A](rc: RC[A]) {
      def map[B](f: A ⇒ B): RC[B] = { (cb: B ⇒ Unit) ⇒
        rc(a ⇒ cb(f(a)))
      }

      def flatMap[B](f: A ⇒ RC[B]): RC[B] = { (cb: B ⇒ Unit) ⇒
        rc(a ⇒ f(a)(cb))
      }
    }

    // Now we can use the for/yield syntax:
    val result2: RC[Int] = for {
      x ← pure(10)
      y ← mul4(x) _
      z ← add3(y) _
    } yield z
    
    /*
    val result3 = for {
    x <- result2
    y <- add3(x) _
    } yield ...
     */

    extractValue(result2) shouldEqual 43
  }

  /*
  What we gained:

  - we can compose API calls for APIs that register callbacks
  - we have easy access to the *innermost* callback

  ((A ⇒ 1) ⇒ 1)  @@ (A ⇒ (B ⇒ 1) ⇒ 1) : (B ⇒ 1) ⇒ 1
    |                 ^    ^               |
    |                |    |               |
    \_______________/     \_____________/

   */

  it should "read a file and copy its contents to another file, using NIO2 callback API" in {
    val fileChannel = AsynchronousFileChannel.open(Paths.get("chapter07/src/test/resources/sample.txt"), StandardOpenOption.READ)

    // In this simple example, the file is shorter than 256 bytes.
    val buffer1 = ByteBuffer.allocate(256)
    fileChannel.read(buffer1, 0, null, new CompletionHandler[Integer, Object] {
      override def failed(exc: Throwable, attachment: Object): Unit = println(s"Failed to read file: $exc")

      override def completed(result1: Integer, attachment: Object): Unit = {
        println(s"Read $result1 bytes")
        fileChannel.close()
        buffer1.rewind()
        buffer1.limit(result1)
        // Copy to another file.
        val outputFileChannel = AsynchronousFileChannel.open(Paths.get("chapter07/target/sample-copy2.txt"), StandardOpenOption.CREATE, StandardOpenOption.WRITE)
        outputFileChannel.write(buffer1, 0, null, new CompletionHandler[Integer, Object] {
          override def failed(exc: Throwable, attachment: Object): Unit = println(s"Failed to write file: $exc")

          override def completed(result2: Integer, attachment: Object): Unit = {
            println(s"Wrote $result2 bytes")
            outputFileChannel.close()
            // Now read from the file and check that we copied the contents correctly.
            val inputChannel = AsynchronousFileChannel.open(Paths.get("chapter07/target/sample-copy2.txt"), StandardOpenOption.READ)
            val buffer2 = ByteBuffer.allocate(256)
            inputChannel.read(buffer2, 0, null, new CompletionHandler[Integer, Object] {
              override def failed(exc: Throwable, attachment: Object): Unit = println(s"Failed to read new file: $exc")

              override def completed(result3: Integer, attachment: Object): Unit = {
                buffer2.rewind()
                buffer2.limit(result3)
                val isIdentical = new String(buffer2.array()) == new String(buffer1.array())
                println(s"Read $result3 bytes, contents is identical: $isIdentical")
                // We need to return this result, but it's not easy since we are in a deeply nested function.
                // Or, use another callback!
                // reportResult(isIdentical)
              }
            })
          }
        })
      }
    })
  }

  behavior of "the same code using continuation monad"

  // Define `Cont[R, A] = (A ⇒ R) ⇒ R` and use `Cont[Unit, A]` here. The parameter R will be useful later.

  it should "read a file and copy its contents to another file, using NIO2 API via Cont[Unit, A]" in {
    // Now rewrite this code using the continuation monad.
    // The type is (A ⇒ Unit) ⇒ Unit. Define this type constructor for convenience:
    type NioMonad[A] = Cont[Unit, A]

    import Semimonad.SemimonadSyntax

    // Monadic representation for NIO channel .read() method.
    def nioRead(channel: AsynchronousFileChannel): NioMonad[(ByteBuffer, Integer)] = Cont { callback ⇒
      val buffer = ByteBuffer.allocate(256)
      channel.read(buffer, 0, null, new CompletionHandler[Integer, Object] {
        override def failed(exc: Throwable, attachment: Object): Unit = println(s"Failed to read file: $exc")

        override def completed(result: Integer, attachment: Object): Unit = {
          println(s"Cont: Read $result bytes")
          buffer.rewind()
          buffer.limit(result)
          channel.close()
          callback((buffer, result))
        }
      })
    }

    // Monadic representation for NIO channel .write() method.
    def nioWrite(buffer: ByteBuffer, channel: AsynchronousFileChannel): NioMonad[Integer] = Cont { callback ⇒
      channel.write(buffer, 0, null, new CompletionHandler[Integer, Object] {
        override def failed(exc: Throwable, attachment: Object): Unit = println(s"Failed to write file: $exc")

        override def completed(result: Integer, attachment: Object): Unit = {
          println(s"Cont: Wrote $result bytes")
          channel.close()
          callback(result)
        }
      })
    }

    val channel1 = AsynchronousFileChannel.open(Paths.get("chapter07/src/test/resources/sample.txt"), StandardOpenOption.READ)

    val statusMonad: NioMonad[Boolean] = for {
      (buffer1a, result1a) ← nioRead(channel1)

      channel2 = AsynchronousFileChannel.open(Paths.get("chapter07/target/sample-copy3.txt"), StandardOpenOption.CREATE, StandardOpenOption.WRITE)

      _ ← nioWrite(buffer1a, channel2)

      channel3 = AsynchronousFileChannel.open(Paths.get("chapter07/target/sample-copy3.txt"), StandardOpenOption.READ)

      (buffer2a, result3a) ← nioRead(channel3)
    } yield {
      val isIdentical = result1a == result3a && new String(buffer2a.array()) == new String(buffer1a.array())
      isIdentical
    }

    // Now run the monad and provide a continuation for the result value - the `status`.
    // The monad gives direct and easy access to the last, deeply nested callback.
    statusMonad.run { status ⇒ println(s"After running the monad: Status is $status") }
  }

  /*
  There are still some important deficiencies of this code:

  - we don't have a handle on the running operations, can't wait for them to finish, because .run() returns Unit
  - so, Cont[Unit, A] works only with synchronous operations
  - we can't catch any exceptions thrown in one of the callbacks

  To rectify this, we need to make the .run() return a value rather than a Unit. Let that value be of type R.
  Also, make the callbacks return a value of type R (it can then encapsulate errors or other information).

  The result is a computation that first goes in the forward direction (into the nested callbacks)
  and then unwinds, collecting the final value of type R.
   */

  it should "use the continuation monad to compute the total cost of computations" in {
    final case class Cost[A](run: (A ⇒ Double) ⇒ Double) {
      def map[B](f: A ⇒ B): Cost[B] = Cost { cb ⇒ run(a ⇒ cb(f(a))) }

      def flatMap[B](f: A ⇒ Cost[B]): Cost[B] = Cost { cb ⇒
        run(a ⇒ f(a).run(cb))
      }

      // Convenience method to add a cost value.
      def addCost(c: Double): Cost[A] = Cost { ca ⇒ run(ca) + c }
    }

    // A constant computation with zero cost.
    def pure[A](a: A): Cost[A] = Cost(cb ⇒ cb(a))

    // Lift some computations into this monad.

    // Addition costs $10.
    def add3(x: Int): Cost[Int] = Cost[Int] { ca ⇒ ca(x + 3) }

    // Multiplication costs $50. We can specify that later.
    def mul4(x: Int): Cost[Int] = Cost[Int] { ca ⇒ ca(x * 4) }

    // Define the computation.

    val result = for {
      x ← pure(10).addCost(10)
      y ← mul4(x).addCost(50) // Ad hoc assignment of cost.
      z ← add3(y)
    } yield z

    // Run the computation and extract the value.

    def interpret[A](cost: Cost[A]): (A, Double) = {
      var result: A = null.asInstanceOf[A]
      val finalCost = cost.run { x ⇒ result = x; 0 }
      (result, finalCost)
    }

    interpret(result) shouldEqual ((43, 60.0))
  }

  /*
  The return value R can also describe errors; see the "transaction monad" for an example of how that works.
  The required type is (A ⇒ Future[R]) ⇒ Future[R].

  Conclusions:

  - we should avoid using callbacks unless we are forced to use a legacy API
  - for legacy APIs, implement adapters that convert callback API to a monadic DSL using Future (for async) or Cont (for sync)

  Main ideas of a monadic DSL:

  - a "DSL program" is a value of type F[A]; we will need to define the type constructor F
  - values of type F[A] are combined using `flatMap : F[A] ⇒ (A ⇒ F[B]) ⇒ F[B]`
  - actual computations are performed by "running" or "interpreting" the values of type F[A]

  Cost:

  - all computations or API need to be "lifted into the monad type" before using them
  - a "runner" or "interpreter" needs to be provided

  Benefits:

  - computations become values that can be stored and composed at will, before running them
  - different interpreters may be provided for testing or other purposes

   */

}
