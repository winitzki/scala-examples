package example

import cats.{Applicative, Bifunctor, Bitraverse, Contravariant, Eval, Functor, Monoid, Traverse, ~>}
import org.scalatest.{FlatSpec, Matchers}
import cats.syntax.functor._
import cats.syntax.contravariant._
import cats.instances._
import cats.syntax.bifunctor._
import cats.syntax.traverse._
import cats.syntax.monoid._
import cats.syntax.bitraverse._
import WuZip.WuZipSyntax
import io.chymyst.ch._
import SafeCompose._
import cats.data.Func

class Chapter10_03_examplesSpec extends FlatSpec with Matchers {

  behavior of "examples of free typeclass constructions"

  it should "implement a free contrafunctor" in {
    // Methods:
    // F[A] × (B ⇒ A) ⇒ F[B]

    // Tree encoding:  FreeCF[F, B] ≡ F[B] + ∃A. FreeCF[F, A] × (B ⇒ A)
    sealed trait FreeCF[F[_], B]
    case class Wrap[F[_], B](fb: F[B]) extends FreeCF[F, B]
    case class Contramap[F[_], B, A](ca: FreeCF[F, A], f: B ⇒ A) extends FreeCF[F, B]

    // Create FreeCF out of F.
    def wrap[F[_], B](fb: F[B]): FreeCF[F, B] = Wrap(fb)

    // Contrafunctor instance.
    implicit def contraFree[F[_]]: Contravariant[FreeCF[F, ?]] = new Contravariant[FreeCF[F, ?]] {
      def contramap[A, B](fa: FreeCF[F, A])(f: B ⇒ A): FreeCF[F, B] = Contramap(fa, f)
    }

    // Interpret into a specific contrafunctor.
    def run[F[_], C[_] : Contravariant, X](ex: F ~> C, cf: FreeCF[F, X]): C[X] = cf match {
      case Wrap(fb) ⇒ ex(fb)
      case Contramap(ca, f) ⇒ run(ex, ca).contramap(f)
    }

    // Reduced encoding:  FreeCF[F, B] ≡ ∃A. F[A] × (B ⇒ A)
    sealed trait FreeCFR[F[_], B]
    case class Reduced[F[_], B, A](fa: F[A], f: B ⇒ A) extends FreeCFR[F, B]


    // Create FreeCFR out of F.
    def wrapR[F[_], B](fb: F[B]): FreeCFR[F, B] = Reduced(fb, identity)

    // Contrafunctor instance.
    implicit def contraFreeR[F[_]]: Contravariant[FreeCFR[F, ?]] = new Contravariant[FreeCFR[F, ?]] {
      def contramap[A, B](fa: FreeCFR[F, A])(f: B ⇒ A): FreeCFR[F, B] = fa match {
        case Reduced(fx, g) ⇒ Reduced(fx, f before g) // Stack-safe. 
      }
    }

    // Interpret into a specific contrafunctor.
    def runR[F[_], C[_] : Contravariant, X](ex: F ~> C, cf: FreeCFR[F, X]): C[X] = cf match {
      case Reduced(fa, f) ⇒ ex(fa).contramap(f)
    }

    // Example: logger with a prefix. The prefix will contain a message and a timestamp.
    // Writer functor:
    type Wr[X] = (X, Long)

    // We will make a free contrafunctor over the writer functor, FreeCFR[Wr, X].

    // Specific contrafunctor:
    type Logger[A] = A ⇒ String
    implicit val contravariantLogger: Contravariant[Logger] = new Contravariant[Logger] {
      override def contramap[A, B](fa: Logger[A])(f: B ⇒ A): Logger[B] = fa after f
    }

    // Extractor: from identity functor to Logger, A ⇒ Logger[A].
    // The value `p: A` determines the prefix string, consisting of a message and a timestamp.
    def prefixLogger[A]: Wr[A] ⇒ Logger[A] = p ⇒ a ⇒ s"[${p._1}:${p._2}] $a"

    // Create a value of free contrafunctor over Wr, by wrapping a Wr value.

    val wr: Wr[String] = ("message1", 12345L)

    val c1 = wrapR(wr)

    // `c1` is a contrafunctor, has `contramap`.
    val c2 = c1.contramap { x: Int ⇒ s"Items: $x" }

    // Interpret into the logger.
    val result: Logger[Int] = runR(new ~>[Wr, Logger] {
      override def apply[A](fa: Wr[A]): Logger[A] = prefixLogger(fa)
    }, c2)

    // Can use the logger now.
    result(123) shouldEqual "[message1:12345] Items: 123"
  }

  it should "implement a free pointed functor" in {
    // Methods:
    // A ⇒ F[A]
    // F[A] × (A ⇒ B) ⇒ F[B]

    // Tree encoding:  FreePF[F, B] ≡ B + F[B] + ∃A. FreePF[F, A] × (A ⇒ B)
    sealed trait FreePF[F[_], B]
    case class Wrap[F[_], B](fb: F[B]) extends FreePF[F, B]
    case class Point[F[_], B](b: B) extends FreePF[F, B]
    case class Map[F[_], B, A](fpa: FreePF[F, A], f: A ⇒ B) extends FreePF[F, B]

    // Reduced encoding:  FreePFR[F, B] ≡ B + ∃A. F[A] × (A ⇒ B)
    sealed trait FreePFR[F[_], A]
    case class PointR[F[_], B](b: B) extends FreePFR[F, B]
    case class Reduced[F[_], B, A](fa: F[A], f: A ⇒ B) extends FreePFR[F, B]

    // Implement a functor instance.
    implicit def functorFreePFR[F[_]]: Functor[FreePFR[F, ?]] = new Functor[FreePFR[F, ?]] {
      def map[A, B](fra: FreePFR[F, A])(f: A ⇒ B): FreePFR[F, B] = fra match {
        case PointR(x) ⇒ PointR(f(x))
        case Reduced(fa, g) ⇒ Reduced(fa, g before f)
      }
    }

  }

  it should "implement a free filterable functor" in {
    // Methods:
    // F[A] × (A ⇒ 1 + B) ⇒ F[B]

    // Tree encoding:  FreeFi[F, B] ≡ F[B] + ∃A. FreeFi[F, A] × (A ⇒ 1 + B)
    sealed trait FreeFi[F[_], B]
    case class Wrap[F[_], B](fb: F[B]) extends FreeFi[F, B]
    case class MapOpt[F[_], B, A](fia: FreeFi[F, A], f: A ⇒ Option[B]) extends FreeFi[F, B]

    // Reduced encoding:  FreeFiR[F, B] ≡ ∃A. F[A] × (A ⇒ 1 + B)
    sealed trait FreeFiR[F[_], A]
    case class Reduced[F[_], B, A](fa: F[A], f: A ⇒ B) extends FreeFiR[F, B]

  }

  it should "implement a free monadic functor" in {
    // Methods:
    // A ⇒ F[A]
    // F[A] × (A ⇒ F[B]) ⇒ F[B]
   
    // Tree encoding:  FreeM[F, B] ≡ B + F[B] + ∃A. FreeM[F, A] × (A ⇒ FreeM[F, B])
    sealed trait FreeM[F[_], B]
    case class Pure[F[_], B](b: B) extends FreeM[F, B]
    case class Wrap[F[_], B](fb: F[B]) extends FreeM[F, B]
    case class FlatMap[F[_], B, A](fma: FreeM[F, A], f: A ⇒ FreeM[F, B]) extends FreeM[F, B]

    // Reduced encoding:  FreeMR[F, B] ≡ B + ∃A. F[A] × (􏰂A ⇒ FreeMR[F, B])􏰃
    sealed trait FreeMR[F[_], B]
    case class PureR[F[_], B](b: B) extends FreeMR[F, B]
    case class Reduced[F[_], B, A](fa: F[A], f: A ⇒ FreeMR[F, B]) extends FreeMR[F, B]
  }

  it should "implement a free applicative functor" in {
    // Methods:
    // A ⇒ F[A]
    // F[A] × F[A ⇒ B] ⇒ F[B]

  }

  it should "combine two operation constructors in a free functor" in {
    // Methods:
    // F[A] × (A ⇒ B) ⇒ F[B]

  }

  it should "combine a free monad and a free applicative functor" in {
    // Methods:
    // A ⇒ F[A]
    // F[A] × (A ⇒ F[B]) ⇒ F[B]
    // F[A] × F[A ⇒ B] ⇒ F[B]
  }
}
