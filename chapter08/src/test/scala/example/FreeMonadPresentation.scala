package example

import cats.data.Writer
import cats.{Monad, ~>}
import org.apache.hadoop.conf.Configuration
import org.apache.hadoop.fs.{Path, FSDataOutputStream => OutS, FileSystem => FS}
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Try

class FreeMonadPresentation extends FlatSpec with Matchers {
  // Prepare a sample HDFS config, using a local filesystem.
  val hdfsConfig = new Configuration()
  hdfsConfig.set("fs.defaultFS", "file:///")
  val localFs: FS = FS.get(hdfsConfig)
  val url = "./target/tmp/test-hdfs-file.txt"
  val testPath = new Path(url)
  val someBytes = "test".getBytes

  behavior of "hdfs operations"

  it should "delete file, create file, write to file, read from file" in {
    localFs.delete(testPath, false)
    val out = localFs.create(testPath)
    out.write(someBytes)

    out.hflush()
    out.hsync()
    out.close()

    val read = localFs.open(testPath)

    val length = localFs.getFileStatus(testPath).getLen
    val target = new Array[Byte](length.toInt)
    read.readFully(target)
    read.close()

    new String(target) shouldEqual new String(someBytes)

    localFs.delete(testPath, false)
  }

  it should "throw exception when trying to read a non-existing file" in {
    localFs.delete(testPath, false)
    the[java.io.FileNotFoundException] thrownBy localFs.open(testPath) should have message "File target/tmp/test-hdfs-file.txt does not exist"
  }

  behavior of "HDFS programs as a free monad"

  // Define a data type for HDFS operations.

  sealed trait HdfsOps[A]

  final case class Delete(fs: FS, path: Path) extends HdfsOps[Boolean]

  final case class Create(fs: FS, path: Path) extends HdfsOps[OutS]

  final case class Write(out: OutS, body: Array[Byte]) extends HdfsOps[Unit]

  final case class Read(fs: FS, path: Path) extends HdfsOps[Array[Byte]]

  // We could imagine a sequence of HDFS operations as a list:
  lazy val ops: List[HdfsOps[_]] = List(
    Delete(localFs, testPath)
    , Create(localFs, testPath) // This needs to return an `OutS`. How?
    , Write(???, someBytes) // This `Write` needs to use the `OutS` just returned. But how???
    , Read(localFs, testPath) // This `Read` needs to return a result.
  )

  /* 
  This list cannot encode the dependence of later operations on the results of earlier operations.
  The list merely puts all operations as values in a sequence, with no meaningful connection.
  
  One way of expressing the sequential connection is by using `flatMap`:
  
  Create(fs, path).flatMap(out ⇒ Write(out, someBytes).flatMap(_ ⇒ ...))
  
  Note that Create(fs, path) is of type HdfsOps[OutS].
  If only `flatMap` could have the type signature
  HdfsOps[A].flatMap(A ⇒ HdfsOps[B]) : HdfsOps[B],
  we would be able to compose several operations into a "declarative HDFS program",
  so that our test would be rewritten like this:
  
  val hdfsProgram = for {
    _         ← Delete(fs, path)
    out       ← Create(fs, path)
    _         ← Write(out, someBytes)
    readBytes ← Read(fs, path)
    _         ← Delete(fs, path)
  } yield readBytes
  
  // And then we could run it somehow:
  val result: Array[Byte] = hdfsProgram.run(???)
  
  Note that `HdfsOps` is not a functor because of non-generic type arguments!
  So we cannot have a flatMap with the signature above.
  The first step is to convert `HdfsOps` into values of some monad.
  */

  // Define a free monad construction. We want `FreeMonad[F, A]` to be a monad in A,
  // and also we want to be able to convert values of type `F[A]` into `FreeMonad[F, A]`.

  // The implementation shown here is "completely lazy":
  // `pure` and `flatMap` perform no computations at all.

  // Define `pure` in a companion object.
  object FreeMonad {
    def pure[F[_], A](a: A): FreeMonad[F, A] = Pure(a)
  }

  sealed trait FreeMonad[F[_], A] {
    def flatMap[B](afb: A ⇒ FreeMonad[F, B]): FreeMonad[F, B] = FlatMap(this, afb)

    // Also need to define `map`. Let's just define `map` via `flatMap` and `pure`.
    def map[B](f: A ⇒ B): FreeMonad[F, B] = flatMap(f andThen FreeMonad.pure)
  }
  // pure: A => M[A]
  final case class Pure[F[_], A](a: A) extends FreeMonad[F, A]
  // flatMap: (M[A] , A ⇒ M[B]) ⇒ M[B] 
  final case class FlatMap[F[_], A, B](fa: FreeMonad[F, A], afb: A ⇒ FreeMonad[F, B]) extends FreeMonad[F, B]

  final case class Wrap[F[_], A](fa: F[A]) extends FreeMonad[F, A]

  // Define an automatic conversion from F[A] to FreeMonad[F, A].
  implicit def wrapInFreeMonad[F[_], A](fa: F[A]): FreeMonad[F, A] = Wrap(fa)

  // Check that we can write "HDFS programs" now.
  val hdfsProgram: FreeMonad[HdfsOps, Array[Byte]] = for {
    _   ← Delete(localFs, testPath)
    out ← Create(localFs, testPath)
    _   ← Write(out, someBytes)
    readBytes ← Read(localFs, testPath)
    _   ← Delete(localFs, testPath)
  } yield readBytes

  // Let's visualize the value of a shorter program:
  val x1 = for {
    _ ← Delete(localFs, testPath)
    out ← Create(localFs, testPath)
    _ ← Write(out, someBytes)
  } yield ()
  // This is equivalent to:
  val x2 = Delete(localFs, testPath).flatMap(_ ⇒ Create(localFs, testPath).flatMap(out ⇒ Write(out, someBytes)))
  // and is actually just a few nested case classes and some functions that return more nested case classes:
  val x3 =
    FlatMap(
      Wrap(Delete(localFs, testPath))
      , (_: Boolean) ⇒ FlatMap(
        Wrap(Create(localFs, testPath))
        , (out: OutS) ⇒ Wrap(Write(out, someBytes))
      )
    )

  // How to run such a program? We need to be able to "run" each of the HDFS operations.

  // The way to do this is to define a transformation from HdfsOps[A] to some other monad, say M[A].
  // The monad `M` could actually perform some operations, or could just write a log file, etc.

  // If we have such a transformation, we can transform arbitrary "HDFS programs" into `M` values.
  // The transformation is commonly called an "interpreter" and has type `for all A:  F[A] ⇒ M[A]`,
  // i.e. the function should work separately for each type A. To implement this, we use `cats.~>`.
  // Suggestive syntax: F ~> M  is `for all X: F[X] ⇒ M[X]`.

  def interpret[F[_], M[_] : Monad, A, C](program: FreeMonad[F, A], interpreter: F ~> M): M[A] = program match {
    case Pure(a) ⇒ Monad[M].pure(a)
    case FlatMap(fa: FreeMonad[F, C], afb: (C ⇒ FreeMonad[F, A])) ⇒
      val mc: M[C] = interpret[F, M, C, C](fa, interpreter)

      Monad[M].flatMap(mc)(c ⇒ interpret(afb(c), interpreter))

    case Wrap(fa) ⇒ interpreter(fa)
  }

  // We can choose the target monad at will. Consider two examples: `Writer` and `Try`.

  // Convert HdfsOps to `Writer[String, ?]`. This interpreter will only create log messages.
  // We use `cats.~>` for the transformation of HdfsOps[A] to Try[A] for all A.
  val toWriter: HdfsOps ~> Writer[String, ?] = new (HdfsOps ~> Writer[String, ?]) {
    override def apply[A](fa: HdfsOps[A]): Writer[String, A] = (fa match {
      case Delete(fs, path) ⇒ Writer(s"deleting $path\n", true)
      case Create(fs, path) ⇒ Writer(s"creating $path\n", null)
      case Write(out, body) ⇒ Writer(s"writing ${new String(body)}\n", ())
      case Read(fs, path) ⇒ Writer(s"reading from $path\n", "<unknown>".getBytes)
    }).map(_.asInstanceOf[A]) // Seems to be required.
  }

  val expectedLog =
    """deleting target/tmp/test-hdfs-file.txt
      |creating target/tmp/test-hdfs-file.txt
      |writing test
      |reading from target/tmp/test-hdfs-file.txt
      |deleting target/tmp/test-hdfs-file.txt
      |""".stripMargin


  it should "run the HDFS program using `toWriter` as the interpreter" in {
    import cats.instances.all._

    val logWriter: Writer[String, Array[Byte]] = interpret(hdfsProgram, toWriter)
    // Extract the value out of the writer.
    val log: String = logWriter.run._1
    log shouldEqual expectedLog
  }

  // Convert HdfsOps to `Try`, actually performing all operations and catching any exceptions.
  val toTry: HdfsOps ~> Try = new (HdfsOps ~> Try) {
    override def apply[A](fa: HdfsOps[A]): Try[A] = (fa match {

      case Delete(fs, path) ⇒ Try(fs.delete(path, false))
      case Create(fs, path) ⇒ Try(fs.create(path))
      case Write(out, body) ⇒ Try {
        out.write(body)

        out.hflush()
        out.hsync()
        out.close()
      }
      case Read(fs, path) ⇒ Try {
        val read = fs.open(path)
        val length = fs.getFileStatus(path).getLen
        val target = new Array[Byte](length.toInt)
        read.readFully(target)
        read.close()
        target
      }
    }).map(_.asInstanceOf[A])
  }

  it should "run the HDFS program using `toTry` as the interpreter" in {
    import cats.instances.all._

    val tryResult: Try[Array[Byte]] = interpret(hdfsProgram, toTry)
    new String(tryResult.get) shouldEqual "test"
  }

  // The implementation of the free monad in `cats` is `cats.Free`. Let's implement the same thing using `cats.Free`.

  it should "do the same using cats.free" in {
    import cats.free.Free
    import cats.instances.all._

    // Define an automatic conversion from F[A] to cats.Free[F, A]. The "wrapper" is called `liftF`.
    implicit def wrapInFreeMonad[F[_], A](fa: F[A]): Free[F, A] = Free.liftF(fa)

    // We can now write "HDFS programs".
    val catsHdfsProg = for {
      _ ← Delete(localFs, testPath)
      out ← Create(localFs, testPath)
      _ ← Write(out, someBytes)
      readBytes ← Read(localFs, testPath)
      _ ← Delete(localFs, testPath)
    } yield readBytes

    // Run using `toWriter` and check the results. The "runner" is called `foldMap` in `cats`.
    catsHdfsProg.foldMap(toWriter).run._1 shouldEqual expectedLog
  }

}
