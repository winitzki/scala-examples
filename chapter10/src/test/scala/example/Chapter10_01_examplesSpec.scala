package example

import WuZip.WuZipSyntax
import cats.{Functor, Monoid}
import cats.derived.pure
import cats.kernel.Semigroup
import cats.syntax.monoid._
import cats.syntax.functor._
import org.scalatest.{FlatSpec, Matchers, run}
import io.chymyst.ch._

import scala.annotation.tailrec
import scala.util.Try

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
    // Important: The trait is parameterized by F, but not by A. The `apply` method is parameterized by A.
    // This is important because we need to extract values from F[A] for any A, since the DSL program may have values of arbitrary types before computing the final value,
    // so that run[A]:DSL[A] ⇒ A may need to call Extractor.apply[B] several times with different types B.
    // The type of `apply: F[A] ⇒ A` is similar to a natural transformation between F and the Id functor,
    // except that F is not a functor, so there are no laws with fmap that the transformation would have to satisfy.
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

    // Map to Either[Throwable, A] to catch errors.
    trait Ex[F[_]] {
      def apply[A](fa: F[A]): Either[Throwable, A]
    }

    def runE[F[_], A](extract: Ex[F])(prg: DSL[F, A]): Either[Throwable, A] = prg match {
      case b: Bind[F, _, A] ⇒ b match {
        case Bind(p, f) ⇒ runE(extract)(p).flatMap(f andThen runE[F, A](extract)) // The type parameters `[F, A]` are required here!
      }
      case Literal(x) ⇒ Right(x)
      case Ops(f) ⇒ extract(f)
    }

    val fileOpsErrExtractor: Ex[FileOps] = new Ex[FileOps] {
      override def apply[A](fa: FileOps[A]): Either[Throwable, A] = fa match {
        case Path(s) ⇒ Right(FPath(s).asInstanceOf[A])
        case Read(p) ⇒ Try(mockFs(p.s).asInstanceOf[A]).toEither
      }
    }

    // Run the same DSL program `prg` with a different target monad.
    runE(fileOpsErrExtractor)(prg) shouldEqual Right("text")
  }
  /*
  To prove that the monad laws hold after evaluating a DSL program into a (lawful) monad M:
  
  Take an arbitrary DSL program `prg: DSL[F, A]`, which is being interpreted into a monad M, and denote for brevity run(prg) = m: M[A].
  
  First, show that any monadic operation on `prg` is translated into the same monadic operation on `m`.
  
  If prg = DSL.pure(x) then prg = Literal(x) and then run(prg) = M.pure(x). So, run(DSL.pure(x)) = M.pure(x)
  
  If prg2 = prg.map(f) where f: A ⇒ B, then prg2 = Bind(prg, f andThen DSL.pure) and so
  
    run(prg2) = run(prg).flatMap(f andThen DSL.pure andThen run)
              = m.flatMap(f andThen M.pure)
              = m.map(f)
  
  and now `.map` is directly in the monad M.
  
  If prg2 = prg.flatMap(f) where f: A ⇒ DSL[F, B], then we need to interpret the result value of f into the monad M;
  this gives a function g : A ⇒ M[B] = { x ⇒ run(f(x)) }
  then we get prg2 = Bind(prg, f) and so

    run(prg2) = run(prg).flatMap(f andThen run)
              = m.flatMap(a ⇒ run(f(a))) = m.flatMap(g).

  Here `.flatMap` is in M.

  Since g is the function of type A ⇒ M[B] that corresponds to `f` after interpreting the result of `f` into the monad M,
  we find that the result of flatMap on `prg` and `f` is interpreted into the result of M.flatMap on `m` and `g`.
  
  Now consider various laws.

  Right identity law: flatMap(pure) = id; verified in the slides.
  
  Left identity law: pure(x).flatMap(f) = f(x)
  Apply run() to both sides; need to show that run(pure(x).flatMap(f)) = run(f(x)):
  
  run(pure(x).flatMap(f)) = run(Bind(Literal(x), f)) =  run(Literal(x)).flatMap(f andThen run)
     = M.pure(x).flatMap(f andThen run) = run(f(x)) since M.pure(x).flatMap(g) = g.
  
  Naturality for pure: DSL.pure(f(x)) = DSL.fmap(f)(DSL.pure(x))
  
  Apply run() to both sides: M.pure(f(x)) = M.fmap(f)(M.pure(x)) which holds. 
  
  Associativity for flatMap: flm(f andThen flm g) = (flm f) andThen (flm g)
  Apply both sides to some `prg`, and then apply run() to both sides:
  
  run(prg.flatMap(f andThen flm g)) = run( prg.flatMap(f).flatMap(g) ) 
  
  m.flatMap(f andThen (flm g) andThen run) = run ( prg.flatMap(f) ).flatMap(g andThen run) = m.flatMap(f andThen run).flatMap(g andThen run)

  Now we need to rewrite this into the associativity law for M.
  
  Rewrite (f andThen (flm g) andThen run) as a ⇒ run(f(a).flatMap(g)) or a ⇒ run(f(a)).flatMap(g andThen run)  or 
  equivalently a ⇒ (f andThen run)(a).flatMap(g andThen run)   or (f andThen run) andThen M.flm(g andThen run)
  
  Now consider the associativity law for M[A]:
  
  m.flatMap(fm andThen M.flm gm) = m.flatMap(fm).flatMap(gm)
  
  We see that the associativity law holds after interpreting.
  
  We do not need to verify naturality laws since naturality follows from the fact that we use purely type-parametric code.
   */

  it should "implement free monoid in the tree encoding" in {
    sealed trait FM[Z] // tree encoding
    case class Empty[Z]() extends FM[Z]
    case class Wrap[Z](z: Z) extends FM[Z]
    case class Comb[Z](x: FM[Z], y: FM[Z]) extends FM[Z]

    object FM {
      def empty[Z]: FM[Z] = Empty()
    }

    implicit class FMOps[Z](x: FM[Z]) {
      def |+|(y: FM[Z]): FM[Z] = Comb(x, y)

      // We could also make the encoding less redundant by checking whether x or y are `Empty`, or whether one of them is a `Mul`.
    }

    def run[M: Monoid, Z](extract: Z ⇒ M)(fm: FM[Z]): M = fm match {
      case Empty() ⇒ Monoid[M].empty
      case Wrap(z) ⇒ extract(z)
      case Comb(x, y) ⇒ run(extract)(x) |+| run(extract)(y)
    }

    // Example: A free monoid over Either[Int, String], reduced to the standard Int monoid.

    implicit val monoidInt: Monoid[Int] = new Monoid[Int] {
      override def empty: Int = 0

      override def combine(x: Int, y: Int): Int = x + y
    }

    type Z = Either[Int, String]

    val extract: Z ⇒ Int = {
      case Left(i) ⇒ i
      case Right(str) ⇒ str.length
    }

    val freeMonoidValue: FM[Z] = Wrap[Z](Left(12)) |+| Wrap(Right("abc")) |+| Empty() |+| Wrap(Right("q"))
    run(extract)(freeMonoidValue) shouldEqual 16

    /*
    Verify that the monoid laws hold after run(), interpreting into a lawful monoid M:

    For brevity, write run(x) rather than run(extract)(x). - Assume that `extract` is made into an implicit argument.
    
    Associativity law:
    
    run( (x |+| y) |+| z ) = run ( Comb(Comb(x, y)), z) ) = run( Comb(x, y) ) |+| run(z) = run(x) |+| run(y) |+| run(z).
    
    Similarly run( x |+| (y |+| z) ) = run(x) |+| run(y) |+| run(z).
    
    This is associative since the law holds for the monoid M.
    
    Identity laws: for any x : FM[Z],
    
    run(empty |+| x) = run(Comb(Empty, x)) = run(Empty) |+| run(x) = M.empty |+| run(x) = run(x)
    
    Similarly for run(x |+| empty). 
    */
  }

  it should "implement free monoid in the reduced encoding" in {
    // The reduced encoding means that the free monoid generated by Z is just List[Z].
    type FM[Z] = List[Z]

    object FM {
      def empty[Z]: FM[Z] = Nil
    }

    implicit class FMOps[Z](x: FM[Z]) {
      def |+|(y: FM[Z]): FM[Z] = x ++ y
    }

    def run[M: Monoid, Z](extract: Z ⇒ M)(fm: FM[Z]): M = fm.foldLeft(Monoid[M].empty) { (m, z) ⇒ m |+| extract(z) }

    // Example: A free monoid over Either[Int, String], reduced to the standard Int monoid.

    implicit val monoidInt: Monoid[Int] = new Monoid[Int] {
      override def empty: Int = 0

      override def combine(x: Int, y: Int): Int = x + y
    }

    def wrap[Z](z: Z): FM[Z] = List(z)

    type Z = Either[Int, String]

    val extract: Z ⇒ Int = {
      case Left(i) ⇒ i
      case Right(str) ⇒ str.length
    }

    val freeMonoidValue: FM[Z] = wrap[Z](Left(12)) |+| wrap(Right("abc")) |+| List() |+| wrap(Right("q"))
    run(extract)(freeMonoidValue) shouldEqual 16
  }

  it should "implement free semigroup via continuation encoding" in {

    import cats.syntax.semigroup._
    // We are going to implement a free semigroup generated by type `Z = Int`.
    type Z = Int

    // Typeclass: We have ExZ[S] if S is a type such that a value of S can be extracted from a value of Z.
    trait ExZ[S] {
      def wrap: Z ⇒ S
    }

    // Constructor: wrap(Z)
    def wrap[S: ExZ](z: Z): S = implicitly[ExZ[S]].wrap(z)

    // We can now define values of type FM[Z] by using `of` and `|+|`.
    // But they must be all `def` methods with typeclass constraints.
    // The full type of this `x` is ∀S.(Z ⇒ S) ⇒ (S ⇒ S ⇒ S) ⇒ S
    def x[S: ExZ : Semigroup] = wrap[S](1) |+| wrap[S](2)

    def y[S: ExZ : Semigroup] = x[S] |+| wrap[S](3)

    // Interpret `y` into a specific semigroup.
    // Declare `String` as a semigroup and as an instance of `ExZ`.
    implicit val exZString: ExZ[String] = new ExZ[String] {
      override def wrap: Z ⇒ String = _.toString
    }

    // Hide the implicits in this scope. Otherwise, there is a conflict between implicits for ExZ[String].
    implicit val semigroupString: Semigroup[String] = _ + _

    // Run computation.
    y[String] shouldEqual "123"

  }
}
