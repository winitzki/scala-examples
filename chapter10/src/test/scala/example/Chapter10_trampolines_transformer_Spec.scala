package example

import cats.Functor

import example.CatsMonad.CatsMonadSyntax
import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec

// A free monad transformer with a reified flatMap and a stack-safe runner.
sealed abstract class FreeMonadTr[F[_] : Functor, M[_] : CatsMonad, A] {

  def flatMap[B](f: A => FreeMonadTr[F, M, B]): FreeMonadTr[F, M, B] =
    FreeMonadTr.flatMap(this, f)

  def map[B](f: A => B): FreeMonadTr[F, M, B] =
    this.flatMap(a => FreeMonadTr.pure(f(a)))

  def run(implicit copointedF: Copointed[F]): M[A] = FreeMonadTr.run(this)

  def hoist[N[_]](implicit catsMonadN: CatsMonad[N]): FreeMonadTr[F, N, A] =
    FreeMonadTr.hoist(this)
}

object FreeMonadTr {
  final case class Pure[F[_] : Functor, M[_] : CatsMonad, A]
  (
    pure: Either[A, F[FreeMonadTr[F, M, A]]]
  ) extends FreeMonadTr[F, M, A]

  final case class Wrap[F[_] : Functor, M[_] : CatsMonad, A]
  (
    wrapped: M[Either[A, F[FreeMonadTr[F, M, A]]]]
  ) extends FreeMonadTr[F, M, A]

  final case class FlatMap[F[_] : Functor, M[_] : CatsMonad, X, A]
  (
    arg: FreeMonadTr[F, M, X],
    f: X => FreeMonadTr[F, M, A],
  ) extends FreeMonadTr[F, M, A]


  def pure[F[_] : Functor, M[_] : CatsMonad, A](a: A): FreeMonadTr[F, M, A] =
    Wrap(CatsMonad[M].pure(Left(a)))

  def wrap[F[_] : Functor, M[_] : CatsMonad, A](code: F[FreeMonadTr[F, M, A]]): FreeMonadTr[F, M, A] =
    Wrap(CatsMonad[M].pure(Right(code)))

  def up[F[_] : Functor, M[_] : CatsMonad, A](ma: M[A]): FreeMonadTr[F, M, A] = Wrap(ma.flatMap(a => CatsMonad[M].pure(Left(a))))

  @tailrec private def resume[F[_] : Functor : Copointed, M[_] : CatsMonad, A](t: FreeMonadTr[F, M, A]): Pure[F, M, A] = t match {
    case Pure(x) => Pure(x)
    case Wrap(wrapped) => ???
    case FlatMap(arg, f) => arg match {
      case Pure(Left(a)) => resume(f(a))
      case Pure(Right(deferred)) => Pure[F, M, A](Right( Functor[F].map(deferred)(x => x flatMap f)))
      case Wrap(wrapped) => ???
      case FlatMap(b, g) =>  resume(b.flatMap(x => g(x) flatMap f))
    }
  }

  @tailrec def run[F[_] : Functor : Copointed, M[_] : CatsMonad, A](t: FreeMonadTr[F, M, A]): M[A] = resume(t).pure match {
    case Left(a) => CatsMonad[M].pure(a)
    case Right(deferred) =>
      val result: FreeMonadTr[F, M, A] = Copointed[F].run(deferred())
      run(result)
  }

  def hoist[F[_] : Functor, M[_] : CatsMonad, N[_] : CatsMonad, A](t: FreeMonadTr[F, M, A]): FreeMonadTr[F, N, A] = t match {
    case Pure(pure) => ???
    case Wrap(wrapped) => ???
    case FlatMap(arg, f) => ???
  }

  def flatMap[F[_] : Functor, M[_] : CatsMonad, A, B](t: FreeMonadTr[F, M, A], f: A => FreeMonadTr[F, M, B]): FreeMonadTr[F, M, B] =
    FlatMap(t, f)
}

class Chapter10_trampolines_transformer_Spec extends FlatSpec with Matchers {

  it should "create and run a large left-leaning flatMap tree" in {
    ???
  }
}