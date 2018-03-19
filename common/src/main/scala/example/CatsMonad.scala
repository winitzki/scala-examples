package example

import cats.{FlatMap, Functor, Monad}

// Adapter for cats.Monad that automatically defines tailRecM in a non-stack-safe manner.
// Use only as tutorial illustration or for testing purposes.
trait CatsMonad[F[_]] {
  def flatMap[A, B](fa: F[A])(f: A ⇒ F[B]): F[B]
  def pure[A](x: A): F[A]
}

object CatsMonad {
  implicit def toCatsMonad[F[_]: CatsMonad]: Monad[F] = new Monad[F] {
    val catsMonad: CatsMonad[F] = implicitly[CatsMonad[F]]

    override def tailRecM[A, B](a: A)(f: A ⇒ F[Either[A, B]]): F[B] = flatMap(f(a)) {
      case Right(b) ⇒ pure(b)
      case Left(nextA) ⇒ tailRecM(nextA)(f)
    }

    override def flatMap[A, B](fa: F[A])(f: A ⇒ F[B]): F[B] = catsMonad.flatMap[A,B](fa)(f)

    override def pure[A](x: A): F[A] = catsMonad.pure[A](x)
  }
}
