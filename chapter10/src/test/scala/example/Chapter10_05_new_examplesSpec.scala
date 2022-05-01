package example

import org.scalatest.{FlatSpec, Matchers}

class Chapter10_05_new_examplesSpec extends FlatSpec with Matchers {

  final case class Complex(x: Double, y: Double) {
    def +(other: Complex): Complex = Complex(x + other.x, y + other.y)

    def *(other: Complex): Complex = Complex(x * other.x - y * other.y, x * other.y + y * other.x)

    def conj: Complex = Complex(x, -y)

    def phase: Double = math.atan2(y, x) // Obtain the phase of a complex number.

    def rotate(alpha: Double): Complex = this * Complex(math.cos(alpha), math.sin(alpha))
  }

  sealed trait PrgComplex[A] {
    def flatMap[B](f: A => PrgComplex[B]): PrgComplex[B] = Bind(this)(f)

    def map[B](f: A => B): PrgComplex[B] = Bind(this)(f andThen PrgComplex.pure)
  }

  object PrgComplex {
    def pure[A](a: A): PrgComplex[A] = Val(a)
  }

  final case class Val[A](a: A) extends PrgComplex[A]

  final case class Bind[A, B](pa: PrgComplex[B])(val f: B => PrgComplex[A]) extends PrgComplex[A]

  final case class Add(x: PrgComplex[Complex], y: PrgComplex[Complex]) extends PrgComplex[Complex]

  final case class Mul(x: PrgComplex[Complex], y: PrgComplex[Complex]) extends PrgComplex[Complex]

  final case class Conj(x: PrgComplex[Complex]) extends PrgComplex[Complex]

  final case class Phase(p: PrgComplex[Complex]) extends PrgComplex[Double]

  final case class Rotate(p: PrgComplex[Complex], alpha: PrgComplex[Double]) extends PrgComplex[Complex]

  def runComplex[A]: PrgComplex[A] => A = {
    case Val(a) => a
    case bind@Bind(pa) => runComplex(bind.f(runComplex(pa)))
    case Add(p1, p2) => runComplex(p1) + runComplex(p2)
    case Mul(p1, p2) => runComplex(p1) * runComplex(p2)
    case Conj(p) => runComplex(p).conj
    case Phase(p) => runComplex(p).phase
    case Rotate(p, alpha) => runComplex(p).rotate(runComplex(alpha))
  }

  val prgComplex1: PrgComplex[Complex] = Conj(Mul(Add(Val(Complex(1, 1)), Val(Complex(0, 1))), Val(Complex(3, -4))))

  val prgComplex2: PrgComplex[Complex] = Rotate(prgComplex1, Phase(Val(Complex(0, 1))))

  assert(runComplex(prgComplex1) == Complex(x = 11.0, y = -2.0))

  assert(runComplex(prgComplex2) == Complex(x = 2.000000000000001, y = 11.0))

  def safeRotate(complex: Complex, phase: Complex): PrgComplex[Complex] =
    if (phase.x != 0 && phase.y != 0) Rotate(Val(complex), Phase(Val(phase)))
    else Val(complex)

  val prgComplex3: PrgComplex[Complex] = for {
    result1 ← prgComplex1
    phase ← Val(Complex(0, 1))
    result2 ← safeRotate(result1, phase)
  } yield result2

  assert(runComplex(prgComplex3) == runComplex(prgComplex2))

}
