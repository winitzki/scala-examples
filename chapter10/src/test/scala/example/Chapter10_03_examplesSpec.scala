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

    // Tree encoding:  FreeAp[F, B] ≡ B + F[B] + ∃A. FreeAp[F, A] × FreeAp[F, A ⇒ B]
    sealed trait FreeAp[F[_], B]
    case class Pure[F[_], B](b: B) extends FreeAp[F, B]
    case class Wrap[F[_], B](fb: F[B]) extends FreeAp[F, B]
    case class Ap[F[_], B, A](fpa: FreeAp[F, A], ff: FreeAp[F, A ⇒ B]) extends FreeAp[F, B]

    // Reduced encoding:  FreeApR[F, B] ≡ B + ∃A. F[A] × FreeApR[F, A ⇒ B]
    sealed trait FreeApR[F[_], B]
    case class PureR[F[_], B](b: B) extends FreeApR[F, B]
    case class Reduced[F[_], B, A](fa: F[A], f: FreeApR[F, A ⇒ B]) extends FreeApR[F, B]
  }

  it should "prove laws for tree-encoded free instances of inductive typeclasses" in {
    // Consider an inductive typeclass C with methods (operations) C[X] ⇒ X.
    // Given the method functor C[_], we construct the free instance Free[C[_], Z] generated by type Z.
    // ("free instance of C over Z")

    // The type definition of tree-encoded Free[C, Z] is recursive:
    // Free[C, Z] ≡ Z + C[ Free[C, Z] ]
    sealed trait Free[C[_], Z]
    case class Wrap[C[_], Z](z: Z) extends Free[C, Z]
    case class Ops[C[_], Z](cf: C[Free[C, Z]]) extends Free[C, Z]

    // Prove that Free[C, Z] is an instance of typeclass C generated by Z.
    // This means we have a value of type C[X] ⇒ X for X = Free[C, Z] (methods of the typeclass)
    // and that we can inject a value of Z into Free[C, Z].
    def ops[C[_], Z](cf: C[Free[C, Z]]): Free[C, Z] = Ops(cf)

    def wrap[C[_], Z]: Z ⇒ Free[C, Z] = Wrap.apply

    // Interpret a Free[C, Z] into an instance P : C.
    def run[C[_] : Functor, P, Z](ex: Z ⇒ P)(implicit opsP: C[P] ⇒ P): Free[C, Z] ⇒ P = {
      case Wrap(z) ⇒ ex(z)
      case Ops(cf) ⇒ opsP(cf.map(run[C, P, Z](ex)))
    }

    // Functor instance for Free[C, Z] with respect to Z.
    implicit def functorFreeCZ[C[_]: Functor]: Functor[Free[C, ?]] = new Functor[Free[C, ?]] {
     def map[A, B](fa: Free[C, A])(f: A ⇒ B): Free[C, B] = fa match {
       case Wrap(z) ⇒ Wrap(f(z))
       case Ops(cf) ⇒ Ops(cf.map(fca ⇒ map(fca)(f))) // Recursive call of `map`.
     }
    }
    
    // Law 1: run(wrap) = id when we set P = Free[C, Z].
    // Both sides of law 1 are functions of type Free[C, Z] ⇒ Free[C, Z].
    /* Instead of `implicit opsP: C[P] ⇒ P` we use `ops` in run().
    run(wrap)(opsP = ops)(freeCZ) = freeCZ match {
      case Wrap(z) ⇒ wrap(z) // = Wrap(z)
      case Ops(cf) ⇒ ops(cf.map(run(wrap))) // Induction assumption: run(wrap) = id.
      // Therefore, this is equal to ops(cf) = Ops(cf).
    }
     */
    
    // Law 2: fmap f ◦ run g = run (f ◦ g) for all f: Y ⇒ Z, g: Z ⇒ P and any type P of typeclass C.
    /* Both sides of law 2 are functions of type Free[C, Z] ⇒ P. Apply both sides to a `freeCZ`.
    Compute fmap(f)(freeCZ) = freeCZ.map(f) = freeCZ match {
       case Wrap(z) ⇒ Wrap(f(z))
       case Ops(cf) ⇒ Ops(cf.map(fca ⇒ map(fca)(f))) // Recursive call of `map`.
    }
    
    Compute run(g) of this:
    freeCZ match {
       case Wrap(z) ⇒ g(f(z))
       case Ops(cf) ⇒ opsP(cf.map(fca ⇒ map(fca)(f)).map(run(g)))
    }
    
    Compute run(f ◦ g)(opsP)(freeCZ) = freeCZ match {
      case Wrap(z) ⇒ g(f(z))
      case Ops(cf) ⇒ opsP(cf.map(run(f ◦ g)))
    }
    
    Need to demonstrate equality of cf.map(fca ⇒ map(fca)(f)).map(run(g)) and cf.map(run(f ◦ g)) in the recursive calls.
    By induction assumption, we may assume that law 2 holds for the recursive calls of `map`.
    Therefore we may assume that cf.map(run(f ◦ g)) equals cf.map(map(f) ◦ run(g)).
    But this is exactly what remained to be demonstrated. Q.E.D.
     */
    
    // Universal property: run[P](g) ◦ f = run[Q] (g ◦ f)
    //    for any P, Q of typeclass C, and for any g : Z ⇒ P and any typeclass-preserving f: P ⇒ Q.
    // Typeclass-preserving property: ops[P] ◦ f = fmap f ◦ ops[Q]
    // or equivalently  f(opsP(x)) = opsQ(x.map(f)) for x: C[P].
    
    /* Both sides of the law are functions of type Free[C, Z] ⇒ Q.
       Apply both sides to an arbitrary freeCZ.
       Left side:
       f(run[P](g)(opsP)(freeCZ)) = freeCZ match {
         case Wrap(z) ⇒ f(g(z))
         case Ops(cf) ⇒ f(opsP(cf.map(run(g))))
         
         // This equals opsQ(cf.map(run(g)).map(f)) = opsQ( cf.map( run(g) ◦ f ) ).  
         // By inductive assumption, we can use the universal property for the recursive calls of `run`.
         // Therefore, this equals opsQ( cf.map(run(g ◦ f) ).

       }
       Right side:
       run[Q](g ◦ f)(opsQ)(freeCZ)) = freeCZ match {
         case Wrap(z) ⇒ f(g(z))
         case Ops(cf) ⇒ opsQ(cf.map(run(g ◦ f))))
         // Equals the above.
       }

    */ 
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
