package example

import org.scalatest.{BeforeAndAfterEach, FlatSpec, Matchers}

class Chapter10_05_new_examplesSpec extends FlatSpec with Matchers with BeforeAndAfterEach {

  behavior of "PrgFile language"

  import java.nio.file.{Path => JPath}
  import java.nio.file.{Files, Paths}

  override def beforeEach(): Unit = {
    super.beforeEach()
    Files.write(Paths.get("config.txt"), "version = 1".getBytes)
    Files.write(Paths.get("config_location.txt"), "config.txt".getBytes)
  }

  override def afterEach(): Unit = {
    super.afterEach()
    Seq("config.txt", "config_location.txt").foreach { x ⇒ Files.delete(Paths.get(x)) }
  }

  it should "run monadic programs with PrgFile" in {
    import PrgFile._
    sealed trait PrgFile[A] {
      def flatMap[B](f: A => PrgFile[B]): PrgFile[B] = Bind(this)(f)

      def map[B](f: A => B): PrgFile[B] = Bind(this)(f andThen PrgFile.pure)
    }
    // Same case classes as before.
    object PrgFile {
      final case class Val[A](a: A) extends PrgFile[A]

      final case class Bind[A, B](pa: PrgFile[B])(val f: B => PrgFile[A]) extends PrgFile[A]

      final case class Path(p: PrgFile[String]) extends PrgFile[JPath]

      final case class Read(p: PrgFile[JPath]) extends PrgFile[String]

      def pure[A](a: A): PrgFile[A] = Val(a)

      def runFile[A]: PrgFile[A] => A = {
        case Val(a) => a
        case bind@Bind(pa) => runFile(bind.f(runFile(pa)))
        case Path(p) => Paths.get(runFile(p))
        case Read(p) => new String(Files.readAllBytes(runFile(p)))
      }

    }

    val prg: PrgFile[String] = for {
      p <- Path(Val("config_location.txt"))
      r <- if (Files.exists(p)) Read(Val(p)) else Val("No file.")
    } yield r

    assert(runFile(prg) == "config.txt")

    def readFileContents(filename: String): PrgFile[String] = for {
      path <- Path(Val(filename))
      text <- if (Files.exists(path)) Read(Val(path)) else Val("No file.")
    } yield text

    assert(runFile(readFileContents("config_location.txt")) == "config.txt")

    val prg2: PrgFile[String] = for {
      str <- readFileContents("config_location.txt")
      result <- if (str.nonEmpty) readFileContents(str) else Val("No filename.")
    } yield result

    assert(runFile(prg2) == "version = 1")
  }

  it should "refactor PrgFile DSL to free monad, step 1" in {
    import PrgFile._
    sealed trait PrgFile[A] {
      def flatMap[B](f: A => PrgFile[B]): PrgFile[B] = Bind(this)(f)

      def map[B](f: A => B): PrgFile[B] = Bind(this)(f andThen PrgFile.pure)
    }

    object PrgFile {
      final case class Val[A](a: A) extends PrgFile[A]

      final case class Bind[A, B](pa: PrgFile[B])(val f: B => PrgFile[A]) extends PrgFile[A]

      final case class Op[A](op: PrgFileC[A]) extends PrgFile[A]

      def pure[A](a: A): PrgFile[A] = Val(a)

      def runFile[A]: PrgFile[A] => A = {
        case Val(a) => a
        case bind@Bind(pa) => runFile(bind.f(runFile(pa)))
        case Op(op) ⇒ PrgFileC.runFile(op)
      }
    }

    sealed trait PrgFileC[A]

    object PrgFileC {
      final case class Path(p: PrgFile[String]) extends PrgFileC[JPath]

      final case class Read(p: PrgFile[JPath]) extends PrgFileC[String]

      def runFile[A]: PrgFileC[A] ⇒ A = {
        case Path(p) => Paths.get(PrgFile.runFile(p))
        case Read(p) => new String(Files.readAllBytes(PrgFile.runFile(p)))
      }
    }

    import PrgFileC.{Path, Read}

    def readFileContents(filename: String): PrgFile[String] = for {
      path <- Op(Path(Val(filename)))
      text <- if (Files.exists(path)) Op(Read(Val(path))) else Val("No file.")
    } yield text

    assert(runFile(readFileContents("config_location.txt")) == "config.txt")

    val prg2: PrgFile[String] = for {
      str <- readFileContents("config_location.txt")
      result <- if (str.nonEmpty) readFileContents(str) else Val("No filename.")
    } yield result

    assert(runFile(prg2) == "version = 1")
  }

  it should "refactor PrgFile DSL to free monad, step 2" in {

    import PrgFile._
    sealed trait PrgFile[A] {
      def flatMap[B](f: A => PrgFile[B]): PrgFile[B] = Bind(this)(f)

      def map[B](f: A => B): PrgFile[B] = Bind(this)(f andThen PrgFile.pure)
    }

    object PrgFile {
      final case class Val[A](a: A) extends PrgFile[A]

      final case class Bind[A, B](pa: PrgFile[B])(val f: B => PrgFile[A]) extends PrgFile[A]

      final case class Op[A](op: PrgFileC[A]) extends PrgFile[A]

      def pure[A](a: A): PrgFile[A] = Val(a)

      def runFile[A]: PrgFile[A] => A = {
        case Val(a) => a
        case bind@Bind(pa) => runFile(bind.f(runFile(pa)))
        case Op(op) ⇒ PrgFileC.runFile(op)
      }
    }

    sealed trait PrgFileC[A]

    object PrgFileC {
      final case class Path(p: String) extends PrgFileC[JPath]

      final case class Read(p: JPath) extends PrgFileC[String]

      def runFile[A]: PrgFileC[A] ⇒ A = {
        case Path(p) => Paths.get(p)
        case Read(p) => new String(Files.readAllBytes(p))
      }
    }

    import PrgFileC.{Path, Read}

    def readFileContents(filename: String): PrgFile[String] = for {
      path <- Op(Path(filename))
      text <- if (Files.exists(path)) Op(Read(path)) else Val("No file.")
    } yield text

    assert(runFile(readFileContents("config_location.txt")) == "config.txt")

    val prg2: PrgFile[String] = for {
      str <- readFileContents("config_location.txt")
      result <- if (str.nonEmpty) readFileContents(str) else Val("No filename.")
    } yield result

    assert(runFile(prg2) == "version = 1")
  }

  behavior of "PrgComplex language"

  final case class Complex(x: Double, y: Double) {
    def +(other: Complex): Complex = Complex(x + other.x, y + other.y)

    def *(other: Complex): Complex = Complex(x * other.x - y * other.y, x * other.y + y * other.x)

    def conj: Complex = Complex(x, -y)

    def phase: Double = math.atan2(y, x) // Obtain the phase of a complex number.

    def rotate(alpha: Double): Complex = this * Complex(math.cos(alpha), math.sin(alpha))
  }

  it should "run monadic programs with PrgComplex" in {
    // Need to define all case classes inside an `object`, or else have a weird runtime error "Duplicated method name Bind$module$ in class ..."!
    // There is some problem with Scala's case classes defined within a block { ... } scope.
    object PrgComplex {
      final case class Val[A](a: A) extends PrgComplex[A]

      final case class Bind[A, B](pa: PrgComplex[B])(val f: B => PrgComplex[A]) extends PrgComplex[A]

      final case class Add(x: PrgComplex[Complex], y: PrgComplex[Complex]) extends PrgComplex[Complex]

      final case class Mul(x: PrgComplex[Complex], y: PrgComplex[Complex]) extends PrgComplex[Complex]

      final case class Conj(x: PrgComplex[Complex]) extends PrgComplex[Complex]

      final case class Phase(p: PrgComplex[Complex]) extends PrgComplex[Double]

      final case class Rotate(p: PrgComplex[Complex], alpha: PrgComplex[Double]) extends PrgComplex[Complex]

      def pure[A](a: A): PrgComplex[A] = Val(a)
    }

    import PrgComplex._

    sealed trait PrgComplex[A] {
      def flatMap[B](f: A => PrgComplex[B]): PrgComplex[B] = Bind(this)(f)

      def map[B](f: A => B): PrgComplex[B] = Bind(this)(f andThen PrgComplex.pure)
    }

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

    def safeRotate(prg: PrgComplex[Complex], phase: Complex): PrgComplex[Complex] =
      if (phase.x != 0 || phase.y != 0) Rotate(prg, Phase(Val(phase)))
      else prg

    val prgComplex3: PrgComplex[Complex] = for {
      phase ← Val(Complex(0, 1))
      result2 ← safeRotate(prgComplex1, phase)
    } yield result2

    assert(runComplex(prgComplex3) == runComplex(prgComplex2))
  }

  it should "refactor the PrgComplex DSL to free monad" in {

    sealed trait PrgComplexOps[A]
    object PrgComplexOps {
      final case class Add(x: Complex, y: Complex) extends PrgComplexOps[Complex]

      final case class Mul(x: PrgComplex[Complex], y: PrgComplex[Complex]) extends PrgComplexOps[Complex]

      final case class Conj(x: PrgComplex[Complex]) extends PrgComplexOps[Complex]

      final case class Phase(p: PrgComplex[Complex]) extends PrgComplexOps[Double]

      final case class Rotate(p: PrgComplex[Complex], alpha: PrgComplex[Double]) extends PrgComplexOps[Complex]
    }

    object PrgComplex {
      final case class Val[A](a: A) extends PrgComplex[A]

      final case class Bind[A, B](pa: PrgComplex[B])(val f: B => PrgComplex[A]) extends PrgComplex[A]

      final case class Ops[A](op: PrgComplexOps[A]) extends PrgComplex[A]

      def pure[A](a: A): PrgComplex[A] = Val(a)
    }

    import PrgComplex._
    import PrgComplexOps._

    sealed trait PrgComplex[A] {
      def flatMap[B](f: A => PrgComplex[B]): PrgComplex[B] = Bind(this)(f)

      def map[B](f: A => B): PrgComplex[B] = Bind(this)(f andThen PrgComplex.pure)
    }

    //    def runComplex[A]: PrgComplex[A] => A = {
    //      case Val(a) => a
    //      case bind@Bind(pa) => runComplex(bind.f(runComplex(pa)))
    //      case Ops(Add(p1, p2)) => runComplex(p1) + runComplex(p2)
    //      case Mul(p1, p2) => runComplex(p1) * runComplex(p2)
    //      case Conj(p) => runComplex(p).conj
    //      case Phase(p) => runComplex(p).phase
    //      case Rotate(p, alpha) => runComplex(p).rotate(runComplex(alpha))
    //    }


  }
}
