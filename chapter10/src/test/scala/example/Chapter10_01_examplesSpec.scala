package example

import WuZip.WuZipSyntax
import cats.Functor
import cats.syntax.functor._
import org.scalatest.{FlatSpec, Matchers}
import io.chymyst.ch._
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
    case class Path(s: Prg) extends Prg
    case class Read(p: Prg) extends Prg

    // Mock filesystem: file paths and file contents are strings.
    val mockFs: Map[String, String] = Map("/file1" → "/file2", "/file2" → "text")

    def run(prg: Prg): String = prg match {
      case Bind(p, f) ⇒ run(f(run(p)))
      case Literal(s) ⇒ s
      case Path(s) ⇒ run(s)
      case Read(p) ⇒ mockFs(run(p))
    }

    val prg: Prg = Bind(Read(Path(Literal("/file1"))), { str ⇒ // Should read value "/file2".
      if (str.nonEmpty)
        Read(Path(Literal(str)))
      else Literal("Error: empty path")
    })

    run(prg) shouldEqual "text"
  }

}
