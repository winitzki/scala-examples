package example

import example.TxF.stepWithCleanup
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/*
An implementation of a "transaction monad" without using continuations.

The type is a Future[(A, Unit ⇒ Unit)].

The value of type `Unit ⇒ Unit` accumulates all cleanups that have become necessary because
the pipeline was successful up to the current point.

The `flatMap` method is redefined so that a downstream failure will cause the cleanups to be run.
A downstream success will cause the current cleanup to be appended.

In this way, cleanup steps will be automatically run after the last successful step.
 
The order of the cleanup steps can be reversed by redefining the cleanup concatenation operation.

Drawbacks of this implementation:

- No cancellation or interruption facilities.
- Errors during cleanup are logged but otherwise ignored.
 */

// Convenience class to hold cleanup procedures.
final case class TxCleanup(run: Unit ⇒ Unit) {
  // TxCleanup is a monoid: it has `combine` and `empty`.
  // The order of cleanups is determined here: `c1 |+| c2` first runs `c1.run` and then `c2.run`. 
  def |+|(c: TxCleanup): TxCleanup = TxCleanup(this.run andThen c.run)

  // Run the cleanup but catch and log errors.
  def log(c: TxCleanup): TxCleanup = TxCleanup(_ ⇒ TxCleanup.logFailure(Try(c.run(()))))

  // Convenience method: Run the cleanup and log any failures.
  def execute(): Unit = log(this).run(())
}

object TxCleanup {
  // Stub method: log any failures while running a cleanup procedure.
  def logFailure[A](t: Try[A]): Unit = t match {
    case Failure(ex) ⇒ println(s"Failure during cleanup: $ex")
    case _ ⇒
  }

  val empty: TxCleanup = TxCleanup(_ ⇒ ()) // Empty cleanup.
}

final case class TxF[A](task: Future[(A, TxCleanup)])(implicit executionContext: ExecutionContext) {
  def map[B](f: A ⇒ B): TxF[B] = TxF(task.map { case (a, cleanup) ⇒ (f(a), cleanup) })

  def flatMap[B](f: A ⇒ TxF[B]): TxF[B] = {
    val newTask: Future[(B, TxCleanup)] = task.flatMap { case (a, cleanup1) ⇒
      f(a).task
        // f(a).task is a newly computed Future[(B, TxCleanup)]. We examine the success of that Future.
        // If successful, concatenate the cleanups and continue. If failed, execute the cleanup.
        .transform({ case (b, cleanup2) ⇒ (b, cleanup1 |+| cleanup2) }, { ex ⇒ cleanup1.execute(); ex })
    }
    TxF(newTask)
  }

  def withFilter(p: A ⇒ Boolean): TxF[A] = TxF(task.map { case (a, cleanup) ⇒
    if (p(a)) (a, cleanup) else {
      cleanup.run(())
      throw new Exception(s"Invalid match with value $a")
    }
  })

  // Convenience method: convert a `TxF[A]` into a `Future[A]`.
  def future: Future[A] = task.map(_._1)

  // Convenience syntax: `withCleanup`. Adds a cleanup operation to an existing `TxF` transaction.
  def withCleanup(undo: A ⇒ Try[Unit]): TxF[A] = for {
    a ← this
    _ ← stepWithCleanup(Future.successful(a))(undo)
  } yield a
}

object TxF {
  // Convenience method: create a `Tx` step that has a no-op cleanup.
  def step[A](step: Future[A])(implicit executionContext: ExecutionContext) = TxF(step.map(a ⇒ (a, TxCleanup.empty)))

  // Convenience method: create a `Tx` step from a future value (that may fail) and a cleanup function (that may also fail).
  def stepWithCleanup[A](step: Future[A])(undo: A ⇒ Try[Unit])(implicit executionContext: ExecutionContext): TxF[A] = TxF(step.map(a ⇒ (a, TxCleanup(_ ⇒ TxCleanup.logFailure(undo(a))))))
}

// Unit tests.

class FutureWithCleanup extends FlatSpec with Matchers {

  import scala.concurrent.ExecutionContext.Implicits.global

  behavior of "asynchronous transactions with automatic rollback"

  // The tests will use the following fake API. Each API call may succeed or fail, according to `succeed`.

  /** ** BEGIN fake API for file manipulation ****/

  // Create and write a temporary file whose name is unknown in advance. Returns file name and file size.
  def writeTmpFile(data: String, succeed: Boolean = true): Future[(String, Long)] = {
    if (succeed) Future.successful(("tempfile1", 12345L)) else Future.failed(new Exception(s"failed to create temp file"))
  }

  // Delete a file. This is a "cleanup" action, so it returns a Try[Unit].
  def deleteFile(fileName: String, succeed: Boolean = true): Try[Unit] = {
    if (succeed) {
      println(s"Deleting $fileName")
      Success(())
    } else Failure(new Exception(s"failed to delete $fileName"))
  }

  // Create and write a (non-temporary) file whose name is fixed in advance. Returns file size.
  def writeFile(data: String, succeed: Boolean = true): Future[Option[Long]] = {
    if (succeed) Future.successful(None) else Future.failed(new Exception(s"failed to create file"))
  }

  // Put some entry into the database. Returns the new database ID for the new entry.
  def createDatabaseEntry(info: String, succeed: Boolean = true): Future[Long] = {
    if (succeed) Future.successful(9876543210L) else Future.failed(new Exception(s"failed to create database entry $info"))
  }

  // Delete a database entry. This is a "cleanup" action, so it returns a Try[Unit].
  def deleteDatabaseEntry(id: Long, succeed: Boolean = true): Try[Unit] = {
    if (succeed) {
      println(s"Deleting DB entry $id")
      Success(())
    } else Failure(new Exception(s"failed to delete database entry ID $id"))
  }

  /** ** END fake API for file manipulation ****/

  // Convert some parts of this API into the transaction monad type.
  // We may use `stepWithCleanup` to define the cleanup together with an action,
  // or we can define an action with `Tx.step` and add a cleanup later using `withCleanup`.

  def txWriteTmpFile(data: String, succeed: Boolean = true, cleanupSucceed: Boolean = true): TxF[(String, Long)] =
    TxF.stepWithCleanup(writeTmpFile(data, succeed)) { case (fileName, _) ⇒ deleteFile(fileName, cleanupSucceed) }

  def txWriteFile(data: String, succeed: Boolean = true): TxF[Option[Long]] =
    TxF.step(writeFile(data, succeed)) withCleanup { _ ⇒ deleteFile("blah") }

  def txCreateDBEntry(info: String, succeed: Boolean = true): TxF[Long] =
    TxF.step(createDatabaseEntry(info, succeed))

  // DSL replaces:
  /*
  val (tmpFileName, tmpFileLength) = writeTmpFile("xyz")
  
  by 
  (tmpFileName, tmpFileLength) ← txWriteTmpFile("xyz")
  
   */

  it should "execute several actions that succeed" in {
    // Write temporary file, create database entry, write real file.
    val txSaga1: TxF[(String, Long, Long)] = for {
      (tmpFileName, tmpFileLength) ← txWriteTmpFile("xyz")
      versionId ← txCreateDBEntry(tmpFileName)
    } yield (tmpFileName, tmpFileLength, versionId)

    // Run and wait for results.
    Await.result(txSaga1.future, Duration.Inf) shouldEqual (("tempfile1", 12345L, 9876543210L))
  }

  it should "execute 3 actions, the last one fails, with cleanup that succeeds" in {
    val txSaga2: TxF[Long] = for {
      (tmpFileName, _) ← txWriteTmpFile("xyz")
      // Let's add a cleanup to this step.
      versionId ← txCreateDBEntry(tmpFileName) withCleanup (deleteDatabaseEntry(_))
      _ ← txWriteFile("xyz", succeed = false) // This fails. The previous 2 steps are rolled back.
    } yield versionId

    println(
      """
        |Expecting output:
        |******
        | Deleting DB entry 9876543210
        | Deleting tempfile1
        |******
      """.stripMargin)

    // Run and wait for results. This future will fail.
    the[Exception] thrownBy Await.result(txSaga2.future, Duration.Inf) should have message "failed to create file"
  }

  it should "execute 3 actions, the last one fails, with 1st cleanup that fails" in {
    val txSaga2: TxF[Long] = for {
      (tmpFileName, _) ← txWriteTmpFile("xyz", cleanupSucceed = false)
      versionId ← txCreateDBEntry(tmpFileName) withCleanup (deleteDatabaseEntry(_))
      _ ← txWriteFile("xyz", succeed = false) // This fails. The previous 2 steps are rolled back.
    } yield versionId

    println(
      """
        |Expecting output:
        |******
        | Deleting DB entry 9876543210
        | Failure during cleanup: java.lang.Exception: failed to delete tempfile1
        |******
      """.stripMargin)

    // Run and wait for results. This future will fail.
    the[Exception] thrownBy Await.result(txSaga2.future, Duration.Inf) should have message "failed to create file"
  }

  it should "execute actions and fail due to match error" in {
    val txSaga3 = for {
      (tmpFileName, _) ← txWriteTmpFile("xyz")
      versionId ← txCreateDBEntry(tmpFileName) withCleanup (deleteDatabaseEntry(_))
      Some(_) ← txWriteFile("xyz", succeed = true) // This fails. All 3 steps are rolled back.
    } yield versionId

    println(
      """
        |Expecting output:
        |******
        | Deleting blah
        | Deleting DB entry 9876543210
        | Deleting tempfile1
        |******
      """.stripMargin)

    // Run and wait for results. This future will fail.
    the[Exception] thrownBy Await.result(txSaga3.future, Duration.Inf) should have message "Invalid match with value None"
  }

  it should "execute actions and fail due to failing `if` guard condition" in {
    val txSaga4 = for {
      // x ← txSaga3
      (tmpFileName, _) ← txWriteTmpFile("xyz")
      versionId ← txCreateDBEntry(tmpFileName) withCleanup (deleteDatabaseEntry(_))
      if versionId > 0 // This passes.
      result ← txWriteFile("xyz", succeed = true)
      if result.nonEmpty // This fails. All 3 steps are rolled back.
    } yield versionId

    println(
      """
        |Expecting output:
        |******
        | Deleting blah
        | Deleting DB entry 9876543210
        | Deleting tempfile1
        |******
      """.stripMargin)

    // Run and wait for results. This future will fail.
    the[Exception] thrownBy Await.result(txSaga4.future, Duration.Inf) should have message "Invalid match with value None"
  }
}
