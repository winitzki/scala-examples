package example

import cats.Functor

abstract class Rigid[F[_] : Functor] {
  def fuseIn[A, B](kleisli: A ⇒ F[B]): F[A ⇒ B]

  // These methods do not need to be defined by implementations.
  // Any functor has a `fuseOut` method.
  def fuseOut[A, B](app: F[A ⇒ B]): A ⇒ F[B] = a ⇒ Functor[F].map(app)(ab ⇒ ab(a))

  // Law: `fuseIn andThen fuseOut = identity`

  // Any rigid functor is pointed.
  def point[A](a: A): F[A] = Functor[F].map(fuseIn[F[A], A](identity))(_ ⇒ a)

  // Wrapped unit: a unique value of type F[Unit].
  def wu: F[Unit] = Functor[F].map(fuseIn[F[Unit], Unit](identity))(_ ⇒ Unit)
}

object Rigid {

  def apply[F[_] : Rigid]: Rigid[F] = implicitly[Rigid[F]]

  implicit class RigidSyntax[F[_] : Rigid, A, B](f: A ⇒ F[B]) {
    def fuseIn: F[A ⇒ B] = Rigid[F].fuseIn(f)
  }

}

