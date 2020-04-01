package example

import io.chymyst.ch._
import org.scalatest.{FlatSpec, Matchers}

import scala.util.{Failure, Success, Try}

class BadMonadExampleSpec extends FlatSpec with Matchers {

  behavior of "bad monad"

  it should "fail to implement flatten" in {
    def withParams[P, R, Z]() = {
      type M[A] = Option[A] // Either[Z, A]
      type Bad[A] = (M[A] ⇒ R) ⇒ M[R]

      def flattens[A] = allOfType[Bad[Bad[A]] ⇒ Bad[A]]()

      //      def flatMap[A, B]: Bad[A] ⇒ (A ⇒ Bad[B]) ⇒ Bad[B] = implement
      def flatMaps[A, B] = allOfType[Bad[A] ⇒ (A ⇒ Bad[B]) ⇒ Bad[B]]()

      //      flattens[Int].map(_.lambdaTerm.prettyPrint).length
      //      flatMap[Int, Int].lambdaTerm.prettyPrint
      (flattens[Int].length, flatMaps[Int, Int].length)
    }
    // We can't see a clear failure to implement flatten! When we use the continuation monad for M[A], curryhoward runs out of memory.
    withParams() shouldEqual ((10, 2))
  }

  it should "fail to implement base lift for continuation monad transformer" in {
    type Cont[R, A] = (A ⇒ R) ⇒ R
    type ContT[M[_], R, A] = (A ⇒ M[R]) ⇒ M[R]

    def withParams[Z]() = {
      type M[A] = Either[Z, A]

      def blifts[R, A] = allOfType[Cont[R, A] ⇒ ContT[M, R, A]]()

      blifts.length
    }

    withParams() shouldEqual 0


    """ def blift[R, A]: Cont[R, A] ⇒ ContT[Either[Int, ?], R, A] = implement """ shouldNot compile
  }

  import TryOps._

  it should "run Try with partial recovery" in {
    import javax.crypto.AEADBadTagException

    val result1 = Try {
      throw new AEADBadTagException("xyz")
    } onException[AEADBadTagException] { _ ⇒ Try(123) }

    result1 shouldEqual Success(123)

  }

  val e = new Exception("abc")

  it should "run Try with partial recovery but do not evaluate the recovery" in {
    val result2 = Try(100) onException[Exception] { _ ⇒ throw e }

    result2 shouldEqual Try(100)
  }

  it should "run Try with partial recovery but do not recover on wrong exception" in {
    val result3 = Try(throw e) onException[IllegalArgumentException] { _ ⇒ Try(0) }

    result3 shouldEqual Failure(e)
  }
}

object TryOps {

  implicit class TryPartialRecovery[A, B >: A](val t: Try[A]) extends AnyVal {

    import scala.reflect.ClassTag

    def onException[T](recover: T => Try[B])(implicit classT: ClassTag[T]): Try[B] = t match {
      case Failure(e) if classT.runtimeClass == e.getClass => recover(e.asInstanceOf[T])
      case _ => t
    }
  }

}