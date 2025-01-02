package example

import cats.Functor
import cats.syntax.functor._
import com.eed3si9n.expecty.Expecty.expect
import org.scalatest.{FlatSpec, Matchers}

import java.nio.file.{Files, Paths}
import scala.util.{Success, Try}

object Chapter10_inline_code {
  sealed trait MonadDSL[F[_], A] {
    def flatMap[B](f: A => MonadDSL[F, B]): MonadDSL[F, B] = Bind(this)(f)

    /*
    Alternatively:
      def flatMap[B](f: A => MonadDSL[F, B]): MonadDSL[F, B] = this match {
          case Val(a)          => f(a)
          case bind@Bind(pa)   => Bind(pa)(x => Bind(bind.f(x))(f))
          case _               => Bind(this)(f)
        }
     */
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

  val runnerFile = new Runner[PrgFileC] {
    def apply[X]: PrgFileC[X] => X = runFileC[X]
  }

  type FileDSL[A] = MonadDSL[PrgFileC, A]

  def runFileDSL[A]: FileDSL[A] => A = MonadDSL.run(runnerFile)

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

}

class Chapter10_inline_code extends FlatSpec with Matchers {

  import Chapter10_inline_code._

  it should "test that FileDSL works" in {

    val prgFile1: FileDSL[String] = for {
      x <- Op(Path("test123.txt"))
      y <- Op(Read(x))
    } yield y

    Files.writeString(Paths.get("test123.txt"), "version = 1")
    expect(runFileDSL(prgFile1) == "version = 1")
    Files.delete(Paths.get("test123.txt"))
  }

  it should "test that FileDSL works with implicit conversion" in {
    implicit def toOp[A](p: PrgFileC[A]): Op[PrgFileC, A] = Op(p)

    val prgFile2: FileDSL[String] = for {
      x <- Path("test1234.txt")
      y <- Read(x)
    } yield y

    Files.writeString(Paths.get("test1234.txt"), "version = 1")
    expect(runFileDSL(prgFile2) == "version = 1")
    Files.delete(Paths.get("test1234.txt"))
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

  it should "run into Try monad" in {
    trait RunnerTry[F[_]] {
      def run[X]: F[X] => Try[X]
    }
    val runnerFileTry = new RunnerTry[PrgFileC] {
      def run[X]: PrgFileC[X] => Try[X] = p => Try(runFileC(p))
    }

    def runTry[F[_], A](runner: RunnerTry[F]): MonadDSL[F, A] => Try[A] = {
      case Val(a) => Success(a)
      case bind@Bind(pa) => runTry(runner)(pa).flatMap(bind.f andThen runTry(runner)) // Use flatMap from Try.
      case Op(op) => runner.run(op)
    }
  }

  it should "run into another monad" in {
    trait RunnerM[F[_], M[_]] {
      def run[X]: F[X] => M[X]
    }
    import example.CatsMonad.CatsMonadSyntax
    import example.{CatsMonad => Monad}
    def runM[F[_], M[_] : Monad, A](runnerM: RunnerM[F, M]): MonadDSL[F, A] => M[A] = {
      case Val(a) => Monad[M].pure(a)
      case bind@Bind(pa) => runM(runnerM).apply(pa).flatMap(bind.f andThen runM(runnerM)) // Use flatMap from M.
      case Op(op) => runnerM.run(op)
    }
  }
}

object Chapter10_free_monad_encodings {
  abstract class Free1[F[_] : Functor, T] {
    def flatMap[A](f: T => Free1[F, A]): Free1[F, A] = this match {
      case Pure(t) => f(t)
      case Flatten(p) => Flatten(p.map(g => g.flatMap(f))) // Use F's Functor instance.
    }
  }

  final case class Pure[F[_] : Functor, T](t: T) extends Free1[F, T]

  final case class Flatten[F[_] : Functor, T](p: F[Free1[F, T]]) extends Free1[F, T]

  sealed trait Free2[F[_], T] {
    def flatMap[A](f: T => Free2[F, A]): Free2[F, A] = this match {
      case Return(t) => f(t)
      case Bind(p, g) => Bind(p, (b: Any) => g(b) flatMap f)
    }
  }

  final case class Return[F[_], T](t: T) extends Free2[F, T]

  final case class Bind[F[_], T, A](p: F[A], g: A => Free2[F, T]) extends Free2[F, T]

  sealed trait Free3[F[_], T] {
    def flatMap[A](f: T => Free3[F, A]): Free3[F, A] = FlatMap(this, f)
  }

  final case class Pure3[F[_], T](t: T) extends Free3[F, T]

  final case class Suspend[F[_], T](f: F[T]) extends Free3[F, T]

  final case class FlatMap[F[_], T, A](p: Free3[F, A], g: A => Free3[F, T]) extends Free3[F, T]

  sealed trait Free4[F[_], A] {
    def flatMap[B](f: A => Free4[F, B]): Free4[F, B] = Bind4(this, f)

    def map[B](f: A => B): Free4[F, B] = FMap(this, f)
  }

  import Chapter10_inline_code.Runner

  object Free4 {

    def run4[F[_], A](runner: Runner[F]): Free4[F, A] => A = {
      case Val(a) => a
      case Bind4(pa, f) => run4(runner)(f(run4(runner)(pa)))
      case FMap(pa, f) => f(run4(runner)(pa))
      case Op(op) => runner.apply(op)
    }
  }

  final case class Val[F[_], A](a: A) extends Free4[F, A]

  final case class Bind4[F[_], A, B](pa: Free4[F, B], f: B => Free4[F, A]) extends Free4[F, A]

  final case class FMap[F[_], A, B](pa: Free4[F, B], f: B => A) extends Free4[F, A]

  final case class Op[F[_], A](op: F[A]) extends Free4[F, A] // Wrap all domain-specific operations.

  def free4toFree3[F[_], A]: Free4[F, A] => Free3[F, A] = {
    case FMap(p, f) => FlatMap(free4toFree3(p), f andThen Pure3.apply)
    case Val(a) => Pure3(a)
    case Bind4(p, f) => FlatMap(free4toFree3(p), f andThen free4toFree3)
    case Op(op) => Suspend(op)
  }

  def free3toFree4[F[_], A]: Free3[F, A] => Free4[F, A] = {
    case Pure3(a) => Val(a)
    case FlatMap(p, f) => Bind4(free3toFree4(p), f andThen free3toFree4)
    case Suspend(op) => Op(op)
  }

  def free2toFree3[F[_], A]: Free2[F, A] => Free3[F, A] = {
    case Return(a) => Pure3(a)
    case Bind(p, g) => FlatMap(Suspend(p), g andThen free2toFree3)
  }

  def free3toFree2[F[_], A]: Free3[F, A] => Free2[F, A] = {
    case Pure3(a) => Return(a)
    case Suspend(f) => Bind[F, A, A](f, Return(_)) // Bind[F, T, A](p: F[A], g: A => Free2[F, T]) extends Free2[F, T]
    case FlatMap(p, g) => p match {
      case Pure3(t) => free3toFree2(g(t))
      case Suspend(f) => Bind(f, g andThen free3toFree2)
      case FlatMap(p2, g2) => free3toFree2(FlatMap(p2, (x: Any) => FlatMap(g2(x), g)))
    }
  }
}

class Chapter10_free_monad_encodings extends FlatSpec with Matchers {

}
