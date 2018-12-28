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
    type CWr[X] = FreeCFR[Wr, X] // Helps with IntelliJ.

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

    val c1: CWr[String] = wrapR(wr)

    // `c1` is a contrafunctor, has `contramap`.
    val c2 = c1.contramap { x: Int ⇒ s"Items: $x" } // IntelliJ does not understand this unless FreeCFR[Wr, ?] is declared as a type.

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

  }

  it should "implement a free filterable functor" in {
    // Methods:
    // F[A] × (A ⇒ B) ⇒ F[B]
    // F[A] × (A ⇒ 1 + B) ⇒ F[B]

  }

  it should "implement a free monadic functor" in {
    // Methods:
    // A ⇒ F[A]
    // F[A] × (A ⇒ F[B]) ⇒ F[B]

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
