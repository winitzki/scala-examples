package example

import com.eed3si9n.expecty.Expecty.expect
import org.scalatest.{FlatSpec, Matchers}

import java.nio.file.{Files, Paths}

object Chapter10_inline_code {
  sealed trait MonadDSL[F[_], A] {
    def flatMap[B](f: A => MonadDSL[F, B]): MonadDSL[F, B] = Bind(this)(f)

    def map[B](f: A => B): MonadDSL[F, B] = flatMap(f andThen MonadDSL.pure)
  }

  final case class Val[F[_], A](a: A) extends MonadDSL[F, A]

  final case class Bind[F[_], A, B](pa: MonadDSL[F, B])(val f: B => MonadDSL[F, A]) extends MonadDSL[F, A]

  final case class Op[F[_], A](op: F[A]) extends MonadDSL[F, A] // Wrap all domain-specific operations.

  trait Runner[F[_]] {
    def apply[X]: F[X] => X
  }

  object MonadDSL {
    def pure[F[_], A](a: A): MonadDSL[F, A] = Val(a)

    def run[F[_], A](runner: Runner[F]): MonadDSL[F, A] => A = {
      case Val(a) => a
      case bind@Bind(pa) => run(runner)(bind.f(run(runner)(pa)))
      case Op(op) => runner.apply(op)
    }
  }

  import java.nio.file.{Path => JPath}

  sealed trait PrgFileC[_]

  final case class Path(p: String) extends PrgFileC[JPath]

  final case class Read(p: JPath) extends PrgFileC[String]

  def runFileC[A]: PrgFileC[A] => A = {
    case Path(p) => Paths.get(p)
    case Read(p) => new String(Files.readAllBytes(p))
  }

  final case class Complex(x: Double, y: Double) {
    def +(other: Complex): Complex = Complex(x + other.x, y + other.y)

    def *(other: Complex): Complex = Complex(x * other.x - y * other.y, x * other.y + y * other.x)

    def conj: Complex = Complex(x, -y)

    def phase: Double = math.atan2(y, x) // Obtain the phase of a complex number.

    def rotate(alpha: Double): Complex = this * Complex(math.cos(alpha), math.sin(alpha))
  }

  sealed trait PrgComplexC[A]

  final case class Add(x: Complex, y: Complex) extends PrgComplexC[Complex]

  final case class Mul(x: Complex, y: Complex) extends PrgComplexC[Complex]

  final case class Conj(x: Complex) extends PrgComplexC[Complex]

  final case class Phase(p: Complex) extends PrgComplexC[Double]

  final case class Rotate(p: Complex, alpha: Phase) extends PrgComplexC[Complex]

  def runComplexC[A]: PrgComplexC[A] => A = {
    case Add(p1, p2) => p1 + p2
    case Mul(p1, p2) => p1 * p2
    case Conj(p) => p.conj
    case Phase(p) => p.phase
    case Rotate(p, Phase(a)) => p.rotate(a.phase)
  }


  val runnerComplex = new Runner[PrgComplexC] {
    def apply[X]: PrgComplexC[X] => X = runComplexC[X]
  }
  val runnerFile = new Runner[PrgFileC] {
    def apply[X]: PrgFileC[X] => X = runFileC[X]
  }

}

class Chapter10_inline_code extends FlatSpec with Matchers {

  import Chapter10_inline_code._

  it should "test that FileDSL works" in {
    type FileDSL[A] = MonadDSL[PrgFileC, A]

    def runFileDSL[A]: FileDSL[A] => A = MonadDSL.run(runnerFile)

    val prgFile1: FileDSL[String] = for {
      x <- Op(Path("test123.txt"))
      y <- Op(Read(x))
    } yield y

    Files.writeString(Paths.get("test123.txt"), "version = 1")
    expect(runFileDSL(prgFile1) == "version = 1")
    Files.delete(Paths.get("test123.txt"))
  }

  it should "test that ComplexDSL works" in {
    type PrgComplex[A] = MonadDSL[PrgComplexC, A]
    val prgComplex: PrgComplex[Complex] = for {
      x <- Op(Add(Complex(1, 1), Complex(0, 1)))
      y <- Op(Mul(x, Complex(3, -4)))
      z <- Op(Conj(y))
      r <- Op(Rotate(z, Phase(Complex(0, 1))))
    } yield r

    expect(MonadDSL.run(runnerComplex)(prgComplex) == Complex(x = 2.000000000000001, y = 11.0))
  }
}
