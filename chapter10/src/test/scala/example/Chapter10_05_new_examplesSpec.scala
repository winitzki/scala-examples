package example

import org.scalatest.{FlatSpec, Matchers}

class Chapter10_05_new_examplesSpec extends FlatSpec with Matchers {

  final case class Complex(x: Double, y: Double) {
    def +(other: Complex): Complex = Complex(x + other.x, y + other.y)
    def *(other: Complex): Complex = Complex(x * other.x - y * other.y, x * other.y + y * other.x)
    def conj: Complex = Complex(x, -y)
    def phase: Double = math.atan2(y, x)         // Obtain the phase of a complex number.
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
    case Val(a)             => a
    case bind@Bind(pa)      => runComplex(bind.f(runComplex(pa)))
    case Add(p1, p2)        => (runComplex(p1) + runComplex(p2)).asInstanceOf[A]
    case Mul(p1, p2)        => (runComplex(p1) * runComplex(p2)).asInstanceOf[A]
    case Conj(p)            => runComplex(p).conj.asInstanceOf[A]
    case Phase(p)           => runComplex(p).phase.asInstanceOf[A]
    case Rotate(p, alpha)   => runComplex(p).rotate(runComplex(alpha)).asInstanceOf[A]
  }


}
