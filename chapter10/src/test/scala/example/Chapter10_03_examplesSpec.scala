package example

import cats.syntax.contravariant._
import cats.syntax.functor._
import cats.{Applicative, Contravariant, Functor, ~>}
import example.SafeCompose._
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.{ExecutionContext, Future}

class Chapter10_03_examplesSpec extends FlatSpec with Matchers {

  behavior of "examples of free typeclass constructions"

  it should "implement a free contrafunctor" in {
    // Methods:
    // map: F[A] × (B ⇒ A) ⇒ F[B]

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
      def contramap[A, B](fa: Logger[A])(f: B ⇒ A): Logger[B] = fa after f
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
      def apply[A](fa: Wr[A]): Logger[A] = prefixLogger(fa)
    }, c2)

    // Can use the logger now.
    result(123) shouldEqual "[message1:12345] Items: 123"
  }

  it should "implement a free pointed functor" in {
    // Methods:
    // pure: A ⇒ F[A]
    // map: F[A] × (A ⇒ B) ⇒ F[B]

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
    // mapOpt: F[A] × (A ⇒ 1 + B) ⇒ F[B]

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
    // pure: A ⇒ F[A]
    // flatMap: F[A] × (A ⇒ F[B]) ⇒ F[B]

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
    // pure: A ⇒ F[A]
    // ap: F[A] × F[A ⇒ B] ⇒ F[B]

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
    implicit def functorFreeCZ[C[_] : Functor]: Functor[Free[C, ?]] = new Functor[Free[C, ?]] {
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

  // An "unfunctor" describing two operations: add a name; get name by id. 
  sealed trait UnF1[A]
  final case class AddName(name: String) extends UnF1[Long]
  final case class GetName(id: Long) extends UnF1[Option[String]]

  val UnF1toOption = new (UnF1 ~> Option) {
    def apply[A](fa: UnF1[A]): Option[A] = fa match {
      case AddName(_) ⇒ Some(1L).asInstanceOf[Option[A]]
      case GetName(_) ⇒ None
    }
  }

  // An "unfunctor" describing an operation: log a message. 
  sealed trait UnF2[A]
  final case class LogMessage(message: String) extends UnF2[Unit]

  val UnF2toOption = new (UnF2 ~> Option) {
    def apply[A](fa: UnF2[A]): Option[A] = fa match {
      case LogMessage(_) ⇒ None
    }
  }

  // An "unfunctor" describing one operation: generate a new id. 
  sealed trait UnF3[A]
  final case class FreshId() extends UnF3[Long]

  val UnF3toOption = new (UnF3 ~> Option) {
    def apply[A](fa: UnF3[A]): Option[A] = fa match {
      case FreshId() ⇒ None
    }
  }

  import cats.instances.option._

  it should "combine three operation constructors in a free functor using disjunction" in {

    // Define UnF as a disjunction of the three unfunctors.
    type UnF[A] = Either[UnF1[A], Either[UnF2[A], UnF3[A]]]

    // Define an interpreter for UnF.
    // This boilerplate code depends on the order of disjunctions and is a burden to maintain.
    val UnFOption = new ~>[UnF, Option] {
      def apply[A](fa: UnF[A]): Option[A] = fa match {
        case Left(unf1) ⇒ UnF1toOption(unf1)
        case Right(Left(unf2)) ⇒ UnF2toOption(unf2)
        case Right(Right(unf3)) ⇒ UnF3toOption(unf3)
      }
    }

    // Define a free functor based on UnF. Use reduced encoding.
    sealed trait FF[F[_], A]
    final case class Wrap[F[_], A](fa: F[A]) extends FF[F, A]
    final case class Map[F[_], B, A](fb: F[B], f: B ⇒ A) extends FF[F, A]

    implicit def functorFF[F[_]]: Functor[FF[F, ?]] = new Functor[FF[F, ?]] {
      def map[A, B](ffa: FF[F, A])(f: A ⇒ B): FF[F, B] = ffa match {
        case Wrap(fa) ⇒ Map(fa, f)
        case Map(fz, g) ⇒ Map(fz, f after g)
      }
    }

    def runFF[F[_], G[_] : Functor, A, B](ex: F ~> G, ffa: FF[F, A]): G[A] = ffa match {
      case Wrap(fa) ⇒ ex(fa)
      case Map(fb: F[B], f) ⇒ ex(fb).map(f)
    }

    // Helper functions: Lift values of UnF1, UnF2, UnF3 into the free functor.
    // This boilerplate code depends on the order of disjunctions and is a burden to maintain.
    implicit def LiftUnF1[A](unF1: UnF1[A]): FF[UnF, A] = Wrap(Left(unF1): UnF[A])
    implicit def LiftUnF2[A](unF2: UnF2[A]): FF[UnF, A] = Wrap(Right(Left(unF2)): UnF[A])
    implicit def LiftUnF3[A](unF3: UnF3[A]): FF[UnF, A] = Wrap(Right(Right(unF3)): UnF[A])

    // Define a computation with the free functor, and then interpret it into Option.
    val computation = for {
      // The implicit conversions will lift FreshId() into the free functor.
      x ← FreshId(): FF[UnF, Long] // Type annotation is required here.
      y = x + 1
    } yield y

    runFF(UnFOption, computation) shouldEqual None
  }

  it should "combine three operation constructors in a free functor using Church encoding" in {

    // Church encoding of the free functor using the "extractor type classes".

    // For each unfunctor, define an extractor type class.
    type ExF1[G[_]] = UnF1 ~> G
    type ExF2[G[_]] = UnF2 ~> G
    type ExF3[G[_]] = UnF3 ~> G

    // Typeclass-driven Church/tree encoding of free functor over 3 generators:
    // FreeFC[A] ≡ ∀G[_]: ExF1 : ExF2 : ExF3 : Functor. G[A]
    sealed trait FreeFC[A] {
      def run[G[_] : ExF1 : ExF2 : ExF3 : Functor]: G[A]
    }
    // Note: This is a fully generic encoding; works for any typeclass and for any number of generators.

    // Define functor instance for FreeFC[?].
    implicit def functorFF: Functor[FreeFC] = new Functor[FreeFC] {
      def map[A, B](fa: FreeFC[A])(f: A ⇒ B): FreeFC[B] = new FreeFC[B] {
        def run[G[_] : ExF1 : ExF2 : ExF3 : Functor]: G[B] = fa.run[G].map(f) // Not stack-safe.
      }
    }

    // Helper functions: Lift values of UnF1, UnF2, UnF3 into the free functor.
    // This boilerplate code does not depend on the order of the unfunctors.
    implicit def LiftUnF1[A](unF1: UnF1[A]): FreeFC[A] = new FreeFC[A] {
      def run[G[_] : ExF1 : ExF2 : ExF3 : Functor]: G[A] = implicitly[ExF1[G]].apply(unF1)
    }
    implicit def LiftUnF2[A](unF2: UnF2[A]): FreeFC[A] = new FreeFC[A] {
      def run[G[_] : ExF1 : ExF2 : ExF3 : Functor]: G[A] = implicitly[ExF2[G]].apply(unF2)
    }
    implicit def LiftUnF3[A](unF3: UnF3[A]): FreeFC[A] = new FreeFC[A] {
      def run[G[_] : ExF1 : ExF2 : ExF3 : Functor]: G[A] = implicitly[ExF3[G]].apply(unF3)
    }

    // Define a computation with the free functor, and then interpret it into Option.
    val computation = for {
      // The implicit conversions will lift FreshId() into the free functor.
      x ← FreshId(): FreeFC[Long] // Type annotation is required here.
      y = x + 1
    } yield y

    // Extractor typeclass instances for Option.
    implicit val optionExF1: ExF1[Option] = UnF1toOption
    implicit val optionExF2: ExF2[Option] = UnF2toOption
    implicit val optionExF3: ExF3[Option] = UnF3toOption

    computation.run[Option] shouldEqual None
  }

  it should "combine a free monad and a free applicative functor using tree encoding" in {
    // Methods:
    // pure:                     B ⇒ F[B]
    // flatMap: F[A] × (A ⇒ F[B]) ⇒ F[B]
    // ap:        F[A] × F[A ⇒ B] ⇒ F[B]

    // Tree encoding:
    sealed trait FreeMA[F[_], B]
    case class Wrap[F[_], B](fb: F[B]) extends FreeMA[F, B]
    case class Pure[F[_], B](x: B) extends FreeMA[F, B]
    case class FlatMap[F[_], B, A](fmx: FreeMA[F, A], fx2mb: A ⇒ FreeMA[F, B]) extends FreeMA[F, B]
    case class Ap[F[_], B, A](fmx: FreeMA[F, A], ffx2b: FreeMA[F, A ⇒ B]) extends FreeMA[F, B]

    def runFMAT[F[_], G[_] : CatsMonad : Applicative, A](ex: F ~> G)(fmat: FreeMA[F, A]): G[A] = fmat match {
      case Wrap(fa) ⇒ ex(fa)
      case Pure(b) ⇒ implicitly[Applicative[G]].pure(b)
      case FlatMap(fma, f) ⇒
        val g = runFMAT(ex)(fma)
        val newF = f andThen runFMAT(ex)
        implicitly[CatsMonad[G]].flatMap(g)(newF)
      case Ap(fma, ff) ⇒
        val g = runFMAT(ex)(fma)
        val gg = runFMAT(ex)(ff)
        implicitly[Applicative[G]].ap(gg)(g)
    }
    
    // Interpret FreeMA[F, ?] into Future[?], keeping track of parallelism.
    def runFMATFuture[F[_], A](ex: F ~> Future)(fmat: FreeMA[F, A])(implicit ec: ExecutionContext): Future[A] = fmat match {
      case Wrap(fa) ⇒ ex(fa)
      case Pure(b) ⇒ Future.successful(b)
      
      case FlatMap(fma, f: (Any ⇒ FreeMA[F, A])) ⇒ for {
        x ← runFMATFuture(ex)(fma)
        y = f(x)
        z ← runFMATFuture(ex)(y)
      } yield z
      
      case Ap(fma, ff: FreeMA[F, Any ⇒ A]) ⇒
        // Keep the parallelism of execution for `Future`s.
        val fmaFuture = runFMATFuture(ex)(fma)
        val ffFuture = runFMATFuture[F, Any ⇒ A](ex)(ff)
        for {
          x ← fmaFuture
          y ← ffFuture
        } yield y(x)
    }

    // Implement various typeclass instances for FreeMA[F, ?], preserving the parallel/sequential execution.

    // Implement Functor with some simplifications.
    implicit def functorFreeMA[F[_]]: Functor[FreeMA[F, ?]] = new Functor[FreeMA[F, ?]] {
      def map[B, C](fa: FreeMA[F, B])(f: B ⇒ C): FreeMA[F, C] = fa match {
        case Wrap(fb) ⇒ Ap(Wrap(fb), Pure(f))
        case Pure(x) ⇒ Pure(f(x))
        case FlatMap(fmx, fx2mb: (Any ⇒ FreeMA[F, B])) ⇒
          // (fmx: FreeMA[A]).flatMap(fx2mb: A ⇒ FreeMA[B]).map(f: B ⇒ C) is transformed into
          // (fmx: FreeMA[A]).flatMap(x ⇒ fx2mb(x) map f)
          FlatMap[F, C, Any](fmx, x ⇒ map(fx2mb(x))(f))

        case Ap(fmx, ffx2b: FreeMA[F, Any ⇒ B]) ⇒
          // (fmx: FreeMA[A]).ap(ffx2b: FreeMA[A ⇒ B]).map(f: B ⇒ C) is transformed into
          // (fmx: FreeMA[A]).ap(q: FreeMA[A ⇒ C]) where q is computed as
          val q = map[Any ⇒ B, Any ⇒ C](ffx2b) { (r: Any ⇒ B) ⇒ (x: Any) ⇒ f(r(x)) }
          Ap(fmx, q)
      }
    }

    // Implement CatsMonad with some simplifications.
    implicit def catsMonadFreeMA[F[_]]: CatsMonad[FreeMA[F, ?]] = new CatsMonad[FreeMA[F, ?]] {
      def pure[A](x: A): FreeMA[F, A] = Pure(x)

      def flatMap[B, C](fa: FreeMA[F, B])(f: B ⇒ FreeMA[F, C]): FreeMA[F, C] = fa match {
        case Pure(x) ⇒ f(x) // Pure(x: B) . flatMap(f: B ⇒ FreeMA[F, B])
        case Wrap(fb) ⇒ FlatMap(Wrap(fb), f)
        case FlatMap(fmx, fx2m) ⇒
          // (fmx: FreeMA[A]).flatMap(f: A ⇒ FreeMA[B]).flatMap(g: B ⇒ FreeMA[C]) is transformed into
          // (fmx: FreeMA[A]).flatMap(x ⇒ f(x) flatMap g) 
          FlatMap(fmx, (x: Any) ⇒ flatMap(fx2m(x))(f))

        case Ap(fmx, ffx2b) ⇒
        // (fmx: FreeMA[A]).ap(ffx2b: FreeMA[A ⇒ B]).flatMap(f: B ⇒ FreeMA[C]) is transformed into
        // (fmx.ap(...): FreeMA[B]).flatMap(f: B ⇒ FreeMA[C])
          FlatMap(Ap(fmx, ffx2b), f)
      }
    }

    // Implement Applicative with minimal simplifications.
    implicit def wuZipFreeMA[F[_]]: WuZip[FreeMA[F, ?]] = new WuZip[FreeMA[F, ?]] {
      val ffma: Functor[FreeMA[F, ?]] = functorFreeMA[F]

      def wu: FreeMA[F, Unit] = Pure(())

      def zip[A, B](fa: FreeMA[F, A], fb: FreeMA[F, B]): FreeMA[F, (A, B)] = (fa, fb) match {
        case (Pure(x), Pure(y)) ⇒ Pure((x, y))
        case (Pure(x), _) ⇒ ffma.map(fb) { b ⇒ (x, b) }
        case (_, Pure(y)) ⇒ ffma.map(fa) { a ⇒ (a, y) }
        case (_, _) ⇒ Ap(fa, ffma.map[B, A ⇒ (A, B)](fb) { b ⇒ a ⇒ (a, b) })
      }
    }
    import WuZip._
    import CatsMonad.CatsMonadSyntax

    // Define a DSL, as an unfunctor with various operations.
    sealed trait DSL[A]
    case class MakeId[A]() extends DSL[Long]
    case class GetName[A](id: Long) extends DSL[String]
    case class Validate[A](name: String) extends DSL[Boolean]
    case class CloseSession[A](status: Boolean) extends DSL[Unit]
    
    // Convert the unfunctor into a free applicative/monadic functor.
    type FDSL[A] = FreeMA[DSL, A]
    
    // Helper function for lifting operations into the functor.
    implicit def w[A](dsl: DSL[A]): FDSL[A] = Wrap(dsl)
    
    val x =  (GetName(1L): FDSL[String]) zip  GetName(2L) 
    
    
    
    // Define a computation.
    val computation = for {
      id ← MakeId(): FDSL[Long]
//      (x, y, z) = (id+1, id+2, id+3) // Generate 3 new IDs.
//      names ← w(GetName(x)) zip w(GetName(y)) zip w(GetName(z))
    } yield ???
  }
}
