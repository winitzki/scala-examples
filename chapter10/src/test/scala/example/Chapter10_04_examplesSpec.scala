package example

import cats.syntax.functor._
import cats.{Applicative, Functor, ~>}
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

// Tree encoding of the free monad/applicative:
// Methods:
// pure:                     B ⇒ F[B]
// flatMap: F[A] × (A ⇒ F[B]) ⇒ F[B]
// ap:        F[A] × F[A ⇒ B] ⇒ F[B]
sealed trait FreeMA[F[_], B]
final case class Wrap[F[_], B](fb: F[B]) extends FreeMA[F, B]
case class Pure[F[_], B](x: B) extends FreeMA[F, B]
case class FlatMap[F[_], B, A](fmx: FreeMA[F, A], fx2mb: A ⇒ FreeMA[F, B]) extends FreeMA[F, B]
case class Ap[F[_], B, A](fmx: FreeMA[F, A], ffx2b: FreeMA[F, A ⇒ B]) extends FreeMA[F, B]

class Chapter10_04_examplesSpec extends FlatSpec with Matchers {

  behavior of "free monad/applicative"
  
  it should "combine a free monad and a free applicative functor using tree encoding" in {

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
    import CatsMonad.CatsMonadSyntax
    import WuZip._

    // Define a DSL, as an unfunctor with various operations.
    sealed trait DSL[A] {
      def lift: FDSL[A] = Wrap(this)
    }
    case class MakeId[A]() extends DSL[Long]
    case class FindName[A](id: Long) extends DSL[String]
    case class Validate[A](name: String) extends DSL[Boolean]
    case class CloseSession[A](status: Boolean) extends DSL[Boolean]

    // Convert the unfunctor into a free applicative/monadic functor.
    type FDSL[A] = FreeMA[DSL, A]

    // Helper function for lifting operations into the functor.
    implicit def wrap[A](dsl: DSL[A]): FDSL[A] = Wrap(dsl)

    // Define a computation.

    // Part of the logic is to validate names. 
    def validate(id: Long): FDSL[Boolean] = for {
      x ← FindName(id).lift
      y ← Validate(x).lift
    } yield y

    // Returns FDSL[Boolean].
    val computation = for {
      id ← MakeId().lift
      (x, y, z) = (id + 1, id + 2, id + 3) // Generate 3 new IDs.
      ((rx, ry), rz) ←  validate(x) zip validate(y) zip validate(z)
      status ← CloseSession(rx && ry && rz).lift
    } yield status

    def interpreter(implicit ec: ExecutionContext): DSL ~> Future = new (DSL ~> Future) {
      def apply[A](fa: DSL[A]): Future[A] = fa match {
        case MakeId() ⇒ Future(scala.util.Random.nextLong().asInstanceOf[A])
        case FindName(id) ⇒ Future(s"name ${id % 4}".asInstanceOf[A])
        case Validate(name) ⇒ Future((name.length % 2 == 0).asInstanceOf[A])
        case CloseSession(status) ⇒ Future.successful(status.asInstanceOf[A])
      }
    }
    import cats.instances.future._

    // CatsMonad instance for Future.
    implicit def catsMonadFuture(implicit ec: ExecutionContext): CatsMonad[Future] = new CatsMonad[Future] {
      def flatMap[A, B](fa: Future[A])(f: A ⇒ Future[B]): Future[B] = fa flatMap f

      def pure[A](x: A): Future[A] = Future.successful(x)
    }

    def interpreterFuture(implicit ec: ExecutionContext): FDSL ~> Future = new (FDSL ~> Future) {
      def apply[A](fa: FDSL[A]): Future[A] = runFMAT(interpreter)(fa)
    }

    // Run the computation.

    import scala.concurrent.ExecutionContext.Implicits.global
    val result = interpreterFuture.apply(computation)
    Await.result(result, Duration.Inf) shouldEqual false
  }
}
