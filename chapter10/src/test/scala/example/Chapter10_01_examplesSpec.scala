package example

import WuZip.WuZipSyntax
import cats.Functor
import cats.derived.pure
import cats.syntax.functor._
import org.scalatest.{FlatSpec, Matchers, run}
import io.chymyst.ch._
import shapeless.ops.record.Extractor
import spire.math.Algebraic.Expr.Mul

class Chapter10_01_examplesSpec extends FlatSpec with Matchers {

  behavior of "declarative DSLs"

  it should "implement a simple DSL for complex numbers" in {
    sealed trait Prg
    case class Str(s: String) extends Prg
    case class Mul(p1: Prg, p2: Prg) extends Prg
    case class Conj(p: Prg) extends Prg

    val regexp = """\s*([-0-9.]+)\s*([-+])\s*([0-9.]+)\s*\*\s*i\s*""".r

    def run(prg: Prg): (Double, Double) = prg match {
      case Str(s) ⇒ s match {
        case regexp(x, sgn, y) ⇒
          val sign = if (sgn == "+") 1 else -1
          (x.toDouble, sign * y.toDouble)
      }
      case Mul(p1, p2) ⇒
        val (x1, y1) = run(p1)
        val (x2, y2) = run(p2)
        (x1 * x2 - y1 * y2, x1 * y2 + x2 * y1)
      case Conj(p1) ⇒
        val (x, y) = run(p1)
        (x, -y)
    }

    val prg: Prg = Conj(Mul(
      Str("1 + 2 * i"), Str("3-4*i")
    ))

    run(prg) shouldEqual ((11.0, -2.0))
  }

  it should "implement a simple DSL for file operations" in {
    sealed trait Prg
    case class Bind(p: Prg, f: String ⇒ Prg) extends Prg
    case class Literal(s: String) extends Prg
    case class Path(p: Prg) extends Prg
    case class Read(p: Prg) extends Prg

    // Mock filesystem: file paths and file contents are strings.
    val mockFs: Map[String, String] = Map("/file1" → "/file2", "/file2" → "text")

    def run(prg: Prg): String = prg match {
      case Bind(p, f) ⇒ run(f(run(p)))
      case Literal(s) ⇒ s
      case Path(p) ⇒ run(p)
      case Read(p) ⇒ mockFs(run(p)) // Cannot guarantee that p is a Path.
    }

    val prg: Prg = Bind(Read(Path(Literal("/file1"))), { str ⇒ // Should read value "/file2".
      if (str.nonEmpty)
        Read(Path(Literal(str)))
      else Literal("Error: empty path")
    })

    run(prg) shouldEqual "text"

    // The DSL is not type-safe: it allows us to write nonsensical programs.

    val wrongPrg: Prg = Read(Read(Read(Literal("/file1"))))

    the[Exception] thrownBy run(wrongPrg) should have message "key not found: text"
  }

  it should "implement a type-safe DSL for file operations" in {
    // Mock file path.
    final case class FPath(s: String)
    // Mock filesystem: file paths and file contents are strings.
    val mockFs: Map[String, String] = Map("/file1" → "/file2", "/file2" → "text")

    sealed trait Prg[A]
    case class Bind(p: Prg[String], f: String ⇒ Prg[String]) extends Prg[String]
    case class Literal(s: String) extends Prg[String]
    case class Path(s: Prg[String]) extends Prg[FPath]
    case class Read(p: Prg[FPath]) extends Prg[String]

    def run(prg: Prg[String]): String = prg match {
      case Bind(p, f) ⇒ run(f(run(p)))
      case Literal(s) ⇒ s
      // Impossible to have Path(p) here since it is not of type Prg[String]. The only way of having a Read() is a Read(Path()).
      case Read(Path(p)) ⇒ mockFs(run(p))
    }

    val prg: Prg[String] = Bind(Read(Path(Literal("/file1"))), { str ⇒ // Should read value "/file2".
      if (str.nonEmpty)
        Read(Path(Literal(str)))
      else Literal("Error: empty path")
    })

    run(prg) shouldEqual "text"

    // The DSL is type-safe: nonsensical programs fail to compile.

    """val wrongPrg: Prg[String] = Read(Read(Read(Literal("/file1"))))""" shouldNot compile
  }

  it should "implement a monadic DSL for file operations" in {
    // Mock file path.
    final case class FPath(s: String)
    // Mock filesystem: file paths and file contents are strings.
    val mockFs: Map[String, String] = Map("/file1" → "/file2", "/file2" → "text")

    sealed trait Prg[A]
    case class Bind[A, B](p: Prg[A], f: A ⇒ Prg[B]) extends Prg[B]
    case class Literal[A](a: A) extends Prg[A]
    case class Path(s: String) extends Prg[FPath]
    case class Read(p: FPath) extends Prg[String]

    object Prg {
      def pure[A](a: A): Prg[A] = Literal(a)
    }

    implicit class PrgOps[A](prg: Prg[A]) {
      def flatMap[B](f: A ⇒ Prg[B]): Prg[B] = Bind(prg, f)

      def map[B](f: A ⇒ B): Prg[B] = prg.flatMap(f andThen Prg.pure)
    }

    // Ugliness with type casts!
    def run[A](prg: Prg[A]): A = prg match {
      case b: Bind[_, A] ⇒ b match {
        case Bind(p, f) ⇒ run(f(run(p)))
      }
      case Literal(s) ⇒ s
      case Path(p) ⇒ FPath(p).asInstanceOf[A]
      case Read(p) ⇒ mockFs(p.s).asInstanceOf[A]
    }

    def readPath(p: String): Prg[String] = for {
      path ← Path(p)
      str ← Read(path)
    } yield str

    val prg: Prg[String] = for {
      str ← readPath("/file1")
      result ← if (str.nonEmpty)
        readPath(str)
      else Prg.pure("Error: empty path")
    } yield result

    run(prg) shouldEqual "text"
  }

  it should "implement monadic DSL as a type constructor parameterized by operations" in {
    sealed trait DSL[F[_], A] 
    case class Bind[F[_], A, B](p: DSL[F, A], f: A ⇒ DSL[F, B]) extends DSL[F, B]
    case class Literal[F[_], A](a: A) extends DSL[F, A]
    case class Ops[F[_], A](f: F[A]) extends DSL[F, A]

    object DSL {
      def pure[F[_], A](a: A): DSL[F, A] = Literal(a)
    }

    implicit class DSLOps[F[_], A](prg: DSL[F, A]) {
      def flatMap[B](f: A ⇒ DSL[F, B]): DSL[F, B] = Bind(prg, f)

      def map[B](f: A ⇒ B): DSL[F, B] = prg.flatMap(f andThen DSL.pure)
    }

    // Extract values of type A from an F[A], for all A.
    trait Extractor[F[_]] {
      def apply[A](fa: F[A]): A
    }

    def run[F[_], A](extract: Extractor[F])(prg: DSL[F, A]): A = prg match {
      case b: Bind[F, _, A] ⇒ b match {
        case Bind(p, f) ⇒ run(extract)(f(run(extract)(p)))
      }
      case Literal(x) ⇒ x
      case Ops(f) ⇒ extract(f)
    }

    // Now use this code to define a monadic DSL for file operations.

    // Mock file path.
    final case class FPath(s: String)
    // Mock filesystem: file paths and file contents are strings.
    val mockFs: Map[String, String] = Map("/file1" → "/file2", "/file2" → "text")

    sealed trait FileOps[A]
    case class Path(s: String) extends FileOps[FPath]
    case class Read(p: FPath) extends FileOps[String]

    // Extractor can only produce String or FPath values.
    // Needs type casts.
    val fileOpsExtractor: Extractor[FileOps] = new Extractor[FileOps] {
      override def apply[A](fa: FileOps[A]): A = fa match {
        case Path(s) ⇒ FPath(s).asInstanceOf[A]
        case Read(p) ⇒ mockFs(p.s).asInstanceOf[A]
      }
    }

    // Write a DSL program as before.
    type Prg[A] = DSL[FileOps, A]
    def readPath(p: String): Prg[String] = for {
      path ← Ops(Path(p))
      str ← Ops(Read(path))
    } yield str

    val prg: Prg[String] = for {
      str ← readPath("/file1")
      result ← if (str.nonEmpty)
        readPath(str)
      else DSL.pure[FileOps, String]("Error: empty path") // Type parameters `[FileOps, String]` are required here! Otherwise Scala does not know that DSL.pure uses the type constructor `FileOps`.
    } yield result

    run(fileOpsExtractor)(prg) shouldEqual "text"
  }
}
