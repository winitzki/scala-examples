package example

import io.chymyst.ch.implement
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

/*
A simplistic implementation of a "transaction monad" along the lines of https://www.fos.kuis.kyoto-u.ac.jp/~igarashi/papers/pdf/contextflow-REBLS17.pdf
 */

// First, implement the standard continuation monad.
final case class Cont[R, A](run: (A ⇒ R) ⇒ R) {
  def withFilter(p: A ⇒ Boolean): Cont[R, A] = this

  def map[B](f: A ⇒ B): Cont[R, B] = implement

  def flatMap[B](f: A ⇒ Cont[R, B]): Cont[R, B] = implement
}

// Define the trait `Tx[A]` for convenience.
// The type `Tx[A]` is equivalent to `forall R: (A ⇒ Future[R]) ⇒ Future[R]`.
trait Tx[A] {
  self ⇒

  // This method needs to be overridden.
  def run[R]: Cont[Future[R], A]

  final def map[B](f: A ⇒ B): Tx[B] = new Tx[B] {
    def run[R]: Cont[Future[R], B] = self.run.map(f)
  }

  final def flatMap[B](f: A ⇒ Tx[B]): Tx[B] = new Tx[B] {
    def run[R]: Cont[Future[R], B] = self.run.flatMap(a ⇒ f(a).run)
  }
  
  final def withFilter(p: A ⇒ Boolean): Tx[A] = this
}

object Tx {
  // Stub method: log failure while cleaning up.
  def log[A](t: Try[A]): Unit = t match {
    case Failure(ex) ⇒ println(s"Failure during cleanup: $ex")
    case _ ⇒
  }

  // Convenience method: create a `Tx` step that has a no-op cleanup.
  def step[A](step: ⇒ Future[A]): Tx[A] = stepWithUndo(step)(_ ⇒ Success(()))

  // Convenience method: create a `Tx` step from a future value (that might throw exceptions) and a cleanup function.
  def stepWithUndo[A](step: ⇒ Future[A])(undo: A ⇒ Try[Unit]): Tx[A] = new Tx[A] {
    override def run[R]: Cont[Future[R], A] = Cont(restOfPipeline ⇒
      for {
        a ← step // We fail if this fails.
        result ← restOfPipeline(a).transform(identity, { ex ⇒ log(undo(a)); ex })
      } yield result
    )
  }

  // Convenience syntax: `withCleanup`. Adds an undo operation to an existing `Tx` step.
  implicit class TxOps[A](tx: Tx[A]) {
    def withCleanup(undo: A ⇒ Try[Unit]): Tx[A] = for {
      a ← tx
      _ ← stepWithUndo(Future.successful(a))(undo)
    } yield a

  }

}

class TransactionMonad extends FlatSpec with Matchers {

  behavior of "transaction chain"

  // The tests will use the following fake API. Each API call may succeed or fail, according to `succeed`.

  // Create and write a temporary file whose name is unknown in advance. Returns file name and file size.
  def writeTmpFile(data: String, succeed: Boolean = true): Future[(String, Long)] = {
    if (succeed) Future.successful(("tempfile1", 12345)) else Future.failed(new Exception(s"failed to create temp file"))
  }

  // Delete a file. This is a "cleanup" action, so it returns a Try[Unit].
  def deleteFile(fileName: String, succeed: Boolean = true): Try[Unit] = {
    if (succeed) Success(()) else Failure(new Exception(s"failed to create $fileName"))
  }

  // Create and write a (non-temporary) file whose name is fixed in advance. Returns file size.
  def writeFile(data: String, succeed: Boolean = true): Future[Long] = {
    if (succeed) Future.successful(56789) else Future.failed(new Exception(s"failed to create file"))
  }

  // Put some entry into the database. Returns the new database ID for the new entry.
  def createDatabaseEntry(info: String, succeed: Boolean = true): Future[Long] = {
    if (succeed) Future.successful(9876543210L) else Future.failed(new Exception(s"failed to create database entry $info"))
  }

  // Delete a database entry. This is a "cleanup" action, so it returns a Try[Unit].
  def deleteDatabaseEntry(id: Long, succeed: Boolean = true): Try[Unit] = {
    if (succeed) Success(()) else Failure(new Exception(s"failed to delete database entry ID $id"))
  }

  // Convert some parts of this API into the transaction monad type.

  def txWriteTmpFile(data: String, succeed: Boolean = true): Tx[(String, Long)] =
    Tx.step(writeTmpFile(data, succeed)) withCleanup { case (fileName, _) ⇒ deleteFile(fileName) }

  def txWriteFile(data: String, succeed: Boolean = true): Tx[Long] =
    Tx.step(writeFile(data, succeed)) withCleanup { _ ⇒ deleteFile("blah") }

  def txCreateDBEntry(info: String, succeed: Boolean = true): Tx[Long] =
    Tx.step(createDatabaseEntry(info, succeed))

  it should "execute several actions that succeed" in {
    // Write temporary file, create database entry, write real file.
    val txSaga1: Tx[(String, Long, Long)] = for {
      (tmpFileName, tmpFileLength) ← txWriteTmpFile("xyz")
      newVersionId ← txCreateDBEntry(tmpFileName)
    } yield (tmpFileName, tmpFileLength, newVersionId)

    txSaga1.run.run { case (n, l, id) ⇒ Future.successful(println(s"Saga 1 yields $n, $l, $id")) }
  }
}
