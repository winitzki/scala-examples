package example

import java.nio.ByteBuffer
import java.nio.channels.{AsynchronousFileChannel, CompletionHandler}
import java.nio.file.{Paths, StandardOpenOption}
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import cats.syntax.functor._
import org.scalatest.{FlatSpec, Matchers}

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

    const(10) { r1 ⇒
      mul4(r1) { r2 ⇒
        add3(r2) { r3 ⇒
          res = r3 // Store the result.
        }
      }
    }
    res shouldEqual 43
  }
  
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
    
    Thread.sleep(100) // Uh... nothing else we could do here.
    res shouldEqual 43
  }
  
  /*
  Problems with callbacks:
  
  - don't know when (and whether) the callback was actually called
  - can't easily wait until something happens, unless our code is within the deepest callback
  - code is deeply nested, it is difficult to pass values from one place to another
  - callbacks are not composable: information is passed via side effects or mutable values 
  - information flow is obscured once we start passing callbacks around ("callback hell")
  
  "Solutions":
  
  - do not use callbacks
  - for legacy APIs, implement adapters that convert callback API to a monadic DSL
  
  Main ideas of a monadic DSL:
  
  - a "DSL program" is a value of type F[A]; we will need to define the type constructor F
  - values of type F[A] are combined using `flatMap : F[A] ⇒ (A ⇒ F[B]) ⇒ F[B]`
  - actual computations are performed by "running" or "interpreting" the values of type F[A]
  
   */

  it should "read a file and copy its contents to another file, using NIO2 API" in {
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
        val outputFileChannel = AsynchronousFileChannel.open(Paths.get("chapter07/target/sample-copy1.txt"), StandardOpenOption.CREATE, StandardOpenOption.WRITE)
        outputFileChannel.write(buffer1, 0, null, new CompletionHandler[Integer, Object] {
          override def failed(exc: Throwable, attachment: Object): Unit = println(s"Failed to write file: $exc")

          override def completed(result2: Integer, attachment: Object): Unit = {
            println(s"Wrote $result2 bytes")
            outputFileChannel.close()
            // Now read from the file and check that we copied the contents correctly.
            val inputChannel = AsynchronousFileChannel.open(Paths.get("chapter07/target/sample-copy1.txt"), StandardOpenOption.READ)
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

  behavior of "code using continuation monad"

  it should "read a file and copy its contents to another file, using NIO2 API" in {
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

      channel2 = AsynchronousFileChannel.open(Paths.get("chapter07/target/sample-copy2.txt"), StandardOpenOption.CREATE, StandardOpenOption.WRITE)

      _ ← nioWrite(buffer1a, channel2)

      channel3 = AsynchronousFileChannel.open(Paths.get("chapter07/target/sample-copy2.txt"), StandardOpenOption.READ)

      (buffer2a, result3a) ← nioRead(channel3)
    } yield {
      val isIdentical = result1a == result3a && new String(buffer2a.array()) == new String(buffer1a.array())
      isIdentical
    }

    // Now run the monad and provide a continuation for the result value - the `status`.
    statusMonad.run { status ⇒ println(s"After running the monad: Status is $status") }
  }

}
