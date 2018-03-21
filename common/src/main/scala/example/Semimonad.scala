package example

import cats.{FlatMap, Functor}


abstract class Semimonad[F[_] : Functor] {
  val functorF: Functor[F] = implicitly[Functor[F]]

  def flatMap[A, B](fa: F[A])(f: A ⇒ F[B]): F[B]
}

object Semimonad {

  implicit class SemimonadSyntax[F[_] : Semimonad, A](fa: F[A]) {
    def flatMap[B](f: A ⇒ F[B]): F[B] = implicitly[Semimonad[F]].flatMap(fa)(f)
  }

  implicit class SemimonadFlatten[F[_] : Semimonad, A](fa: F[F[A]]) {
    def flatten: F[A] = implicitly[Semimonad[F]].flatMap(fa)(identity)
  }

  implicit def toCatsFlatMap[F[_] : Semimonad]: FlatMap[F] = new FlatMap[F] {
    val semimonadF: Semimonad[F] = implicitly[Semimonad[F]]

    override def tailRecM[A, B](a: A)(f: A ⇒ F[Either[A, B]]): F[B] = {
      val fab: F[Either[A, B]] = f(a)
      val btofb: B ⇒ F[B] = b ⇒ semimonadF.functorF.map(fab)(x ⇒ b)
      flatMap(fab) {
        case Right(b) ⇒ btofb(b)
        case Left(nextA) ⇒ tailRecM(nextA)(f)
      }
    }

    override def flatMap[A, B](fa: F[A])(f: A ⇒ F[B]): F[B] = semimonadF.flatMap(fa)(f)

    override def map[A, B](fa: F[A])(f: A ⇒ B): F[B] = semimonadF.functorF.map(fa)(f)
  }

}
