package example

import example.Tx.ContFut
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, Promise}
import scala.util.{Failure, Success, Try}

/*
A simplistic implementation of a "transaction monad" along the lines of https://www.fos.kuis.kyoto-u.ac.jp/~igarashi/papers/pdf/contextflow-REBLS17.pdf

The main idea: when running a value of type `(A ⇒ R) ⇒ R`, we somehow obtain `A`, then we call
the continuation `A ⇒ R` and return the resulting `R`.

If our continuation type were just `(A ⇒ R) ⇒ R`, we could do nothing else other than pass on the same value `R`.

But when our return type is actually `Future[R]` or `Try[R]` or some other type that encapsulates failure,
instead of just plain `R`, we can modify that value before passing it on.

So, in this case, we check whether running `A ⇒ Future[R]` returns a failure, and if so, we can
run our cleanup on our value of `A` before we return the failed `Future[R]`.

If there was a previous `(A ⇒ Future[R]) ⇒ Future[R]` that called us, they will receive the failure
and execute their cleanup, and so on.

In this way, cleanup steps will be automatically run in reverse order.
 
Drawbacks of this implementation:

- Not stack-safe! Can throw stack overflow exception if too many actions are chained together.
- No cancellation or interruption facilities.
- Steps always return `Future`, cleanup actions always return `Try`.
 */

/* Define the trait `Tx[A]` for convenience.
   The type `Tx[A]` is equivalent to `forall R: (A ⇒ Future[R]) ⇒ Future[R]`.
   This type is the result of applying the continuation monad transformer to the `Future` monad.

   For convenience, we have defined the type alias
    `type ContFut[R, X] = (X ⇒ Future[R]) ⇒ Future[R]`.
   To define a value of this type, we will write `new Tx[A] { def run[R]: ContFut[R, A] = ??? }`.
*/

trait Tx[A] {
  self ⇒
  // This method needs to be overridden.
  def run[R]: ContFut[R, A]

  final def map[B](f: A ⇒ B): Tx[B] = new Tx[B] {
    def run[R]: ContFut[R, B] = { g ⇒ self.run(f andThen g) }
  }

  final def flatMap[B](f: A ⇒ Tx[B]): Tx[B] = new Tx[B] {
    def run[R]: ContFut[R, B] = { g ⇒ self.run(f(_).run(g)) }
  }

  // Necessary for Scala for/yield syntax to work. This is a lawful `filter`,
  // so we may now use `if` in the `for`/`yield` blocks.
  final def withFilter(p: A ⇒ Boolean): Tx[A] = new Tx[A] {
    override def run[R]: ContFut[R, A] = { g ⇒
      self.run { a ⇒
        if (p(a)) g(a) else Future.failed(new Exception(s"Invalid match with value $a"))
      }
    }
  }
}

// Convenience methods.
object Tx {

  type ContFut[R, X] = (X ⇒ Future[R]) ⇒ Future[R]

  // Stub method: log failure while cleaning up.
  def log[A](t: Try[A]): Unit = t match {
    case Failure(ex) ⇒ println(s"Failure during cleanup: $ex")
    case _ ⇒
  }

  // Convenience method: create a `Tx` step that has a no-op cleanup.
  def step[A](step: ⇒ Future[A]): Tx[A] = stepWithCleanup(step)(_ ⇒ Success(()))

  // Convenience method: create a `Tx` step from a future value (that may fail) and a cleanup function (that may also fail).
  def stepWithCleanup[A](step: ⇒ Future[A])(undo: A ⇒ Try[Unit]): Tx[A] = new Tx[A] {
    override def run[R]: ContFut[R, A] = restOfPipeline ⇒
      for {
        a ← step // We fail if this fails.
        result ← restOfPipeline(a).transform(identity, { ex ⇒ log(undo(a)); ex })
      } yield result
  }

  // Convenience syntax: `withCleanup`. Adds a cleanup operation to an existing `Tx` step.
  implicit class TxOps[A](tx: Tx[A]) {
    def withCleanup(undo: A ⇒ Try[Unit]): Tx[A] = for {
      a ← tx
      _ ← stepWithCleanup(Future.successful(a))(undo)
    } yield a

    // Convenience method: convert a `Tx[A]` into a `Future[A]`.
    def future: Future[A] = tx.run { a ⇒ Future.successful(a) }
  }

}

// Unit tests.

class TransactionMonad extends FlatSpec with Matchers {

  behavior of "asynchronous transactions with automatic rollback"

  // The tests will use the following fake API. Each API call may succeed or fail, according to `succeed`.

  /**** BEGIN fake API for file manipulation ****/
  
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

  /**** END fake API for file manipulation ****/

  // Convert some parts of this API into the transaction monad type.
  // We may use `stepWithCleanup` to define the cleanup together with an action,
  // or we can define an action with `Tx.step` and add a cleanup later using `withCleanup`.

  def txWriteTmpFile(data: String, succeed: Boolean = true, cleanupSucceed: Boolean = true): Tx[(String, Long)] =
    Tx.stepWithCleanup(writeTmpFile(data, succeed)) { case (fileName, _) ⇒ deleteFile(fileName, cleanupSucceed) }

  def txWriteFile(data: String, succeed: Boolean = true): Tx[Option[Long]] =
    Tx.step(writeFile(data, succeed)) withCleanup { _ ⇒ deleteFile("blah") }

  def txCreateDBEntry(info: String, succeed: Boolean = true): Tx[Long] =
    Tx.step(createDatabaseEntry(info, succeed))

  // DSL replaces:
  /*
  val (tmpFileName, tmpFileLength) = writeTmpFile("xyz")
  
  by 
  (tmpFileName, tmpFileLength) ← txWriteTmpFile("xyz")
  
   */
  
  it should "execute several actions that succeed" in {
    // Write temporary file, create database entry, write real file.
    val txSaga1: Tx[(String, Long, Long)] = for {
      (tmpFileName, tmpFileLength) ← txWriteTmpFile("xyz")
      versionId ← txCreateDBEntry(tmpFileName)
    } yield (tmpFileName, tmpFileLength, versionId)

    // Run and wait for results.
    Await.result(txSaga1.future, Duration.Inf) shouldEqual (("tempfile1", 12345L, 9876543210L))
  }

  it should "execute 3 actions, the last one fails, with cleanup that succeeds" in {
    val txSaga2: Tx[Long] = for {
      (tmpFileName, _) ← txWriteTmpFile("xyz")
      // Let's add a cleanup to this step.
      versionId ← txCreateDBEntry(tmpFileName) withCleanup (deleteDatabaseEntry(_))
      _ ← txWriteFile("xyz", succeed = false) // This fails. The previous 2 steps are rolled back.
    } yield versionId

    // Run and wait for results. This future will fail.
    the[Exception] thrownBy Await.result(txSaga2.future, Duration.Inf) should have message "failed to create file"
    /*   Prints output:

Deleting DB entry 9876543210
Deleting tempfile1

     */
  }

  it should "execute 3 actions, the last one fails, with 1st cleanup that fails" in {
    val txSaga2: Tx[Long] = for {
      (tmpFileName, _) ← txWriteTmpFile("xyz", cleanupSucceed = false)
      versionId ← txCreateDBEntry(tmpFileName) withCleanup (deleteDatabaseEntry(_))
      _ ← txWriteFile("xyz", succeed = false) // This fails. The previous 2 steps are rolled back.
    } yield versionId

    // Run and wait for results. This future will fail.
    the[Exception] thrownBy Await.result(txSaga2.future, Duration.Inf) should have message "failed to create file"
    /*   Prints output:

Deleting DB entry 9876543210
Failure during cleanup: java.lang.Exception: failed to delete tempfile1

     */
  }

  it should "execute actions and fail due to match error" in {
    val txSaga3 = for {
      (tmpFileName, _) ← txWriteTmpFile("xyz")
      versionId ← txCreateDBEntry(tmpFileName) withCleanup (deleteDatabaseEntry(_))
      Some(_) ← txWriteFile("xyz", succeed = true) // This fails. All 3 steps are rolled back.
    } yield versionId

    // Run and wait for results. This future will fail.
    the[Exception] thrownBy Await.result(txSaga3.future, Duration.Inf) should have message "Invalid match with value None"
    /*   Prints output:

Deleting blah
Deleting DB entry 9876543210
Deleting tempfile1

    */
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

    // Run and wait for results. This future will fail.
    the[Exception] thrownBy Await.result(txSaga4.future, Duration.Inf) should have message "Invalid match with value None"
    /*   Prints output:
    
Deleting blah
Deleting DB entry 9876543210
Deleting tempfile1

    */
  }
}
