package example

import cats.Functor


abstract class Semimonad[F[_] : Functor] {
  val functorF: Functor[F] = implicitly[Functor[F]]

  def flatMap[A, B](fa: F[A])(f: A ⇒ F[B]): F[B]
}

object Semimonad {

  implicit class SemimonadSyntax[F[_] : Semimonad, A](fa: F[A]) {
    def flatMap[B](f: A ⇒ F[B]): F[B] = implicitly[Semimonad[F]].flatMap(fa)(f)
  }

  implicit class SemimonadFlatten[F[_]: Semimonad, A](fa: F[F[A]]) {
    def flatten: F[A] = implicitly[Semimonad[F]].flatMap(fa)(identity)
  }

}
