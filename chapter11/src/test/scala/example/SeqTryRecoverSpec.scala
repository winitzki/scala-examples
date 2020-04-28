package example

import org.scalatest.{FlatSpec, Matchers}

import scala.util.{Failure, Success, Try}

final case class SeqTry[A](value: Seq[Try[A]]) extends AnyVal {
  def map[B](f: A ⇒ B): SeqTry[B] = SeqTry(value.map(_.map(f)))

  def flatMap[B](f: A ⇒ SeqTry[B]): SeqTry[B] = SeqTry(value.flatMap(t ⇒ SeqTry.traverse((x: A) ⇒ f(x).value)(t).map(_.flatten)))

  def withFilter(p: A ⇒ Boolean): SeqTry[A] = SeqTry(value.filter(_.filter(p).isSuccess))
}

object SeqTry {
  def pure[A](x: A): SeqTry[A] = SeqTry(Seq(Success(x)))

  def sequence[A]: Try[Seq[A]] ⇒ Seq[Try[A]] = {
    case Success(v) ⇒ v.map(Success.apply)
    case Failure(e) ⇒ Seq(Failure(e))
  }

  def traverse[A, B](f: A ⇒ Seq[B]): Try[A] ⇒ Seq[Try[B]] = {
    case Success(v) ⇒ f(v).map(Success.apply)
    case Failure(e) ⇒ Seq(Failure(e))
  }

  implicit def tryToSeqTry[A]: Try[A] ⇒ SeqTry[A] = t ⇒ SeqTry(sequence(t.map(x ⇒ Seq(x))))

  implicit def seqToSeqTry[A]: Seq[A] ⇒ SeqTry[A] = s ⇒ SeqTry(s.map(Success.apply))

//  implicit def trySeqToSeqTry[A]: Try[Seq[A]] ⇒ SeqTry[A] = ts ⇒ SeqTry(sequence(ts))

  implicit class TryWithLoggedFailure[A](t: Try[A]) {
    def logFailure(logFunction: Throwable ⇒ Unit): Try[A] = t.recoverWith { case throwable: Throwable ⇒
      logFunction(throwable)
      Failure(throwable) // Do not actually recover, just log the failure.
    }
  }

  implicit class LiftTry[A](val x: Try[A]) extends AnyVal {
    def up : SeqTry[A] = x
  }

  implicit class LiftSeq[A](val x: Seq[A]) extends AnyVal {
    def up : SeqTry[A] = x
  }

}

class SeqTryRecoverSpec extends FlatSpec with Matchers {

  import SeqTry._

  behavior of "Seq/Try monad transformer with recovery"

  it should "perform a sequence of operations that may fail" in {
    // Write a for/yield block, but each expression on the right-hand side is "lifted" using the special `.up` method.
    // The right-hand side can be a Try, a List, or a SeqTry.
    // Iterations continue over failures. Failures can be recovered using Try().recover(), or just logged using `Try().logFailure()`.
    val result: SeqTry[Int] = for {
      values ← Try(List(1, 2, 3)).up
      x ← values.up
      y ← Try(6 / (x - 1)).logFailure(_ ⇒ println(s"failure with $x")).up
      // The first will fail, the others will not. But iterations will continue. The result is that y = [<failure>, 6, 3].
      z ← (1 to y).up                       // The result is z = [<failure>, 1, 2, 3, 4, 5, 6, 1, 2, 3]
      t ← Try(24 / math.abs(z - 2)).up      // Some of them will fail but iterations will continue.
      // The result is that t = [<failure>, 24, <failure>, 24, 12, 8, 6, 24, <failure>, 24] 
    } yield t

    // To verify the results quickly, replace the errors by the special `failure` value -12345678 that
    // will not be normally present in the output.
    val failure = -12345678
    result.value.map(_.recover { case _ ⇒ failure }).map(_.get) shouldEqual List(failure, 24, failure, 24, 12, 8, 6, 24, failure, 24)
  }

}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object FutureCloseable {
  def using[T, R <: AutoCloseable](r: => R)(program: R => Future[T])(implicit ec: ExecutionContext): Future[T] = {
    val futureResult: Future[T] = for {
      resource ← Future(r) // If this fails, we will just return the resulting failed future value.
      // Only recover the given program. Do not recover from failure creating the resource.
      result ← program(resource).recoverWith {
        case e: Throwable ⇒
          Try(resource.close()) match {
            case Failure(e2) ⇒
              e.addSuppressed(e2)
            case Success(_) ⇒
          }
          Future.failed(e)
      }
    } yield {
      Try(resource.close()) // We ignore any exceptions here.
      result
    }

    futureResult
  }
}

// Problem: this does not work together with continuations.
