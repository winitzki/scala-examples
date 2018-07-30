package example

import cats.{Applicative, Functor}

// Define the Applicative type class in a simpler way using `wu` and `zip`.
abstract class WuZip[F[_]](implicit val functorF: Functor[F]) {
  def wu: F[Unit]

  def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)]
}

object WuZip {

  def apply[F[_] : WuZip]: WuZip[F] = implicitly[WuZip[F]]

  // Define a `cats` `Applicative` instance through `wu` and `zip`.
  implicit def toCatsApplicative[F[_]](implicit catsZippableF: WuZip[F]): Applicative[F] = new Applicative[F] {
    val functorF: Functor[F] = catsZippableF.functorF

    override def pure[A](x: A): F[A] = functorF.map(catsZippableF.wu)(_ ⇒ x)

    override def ap[A, B](ff: F[A ⇒ B])(fa: F[A]): F[B] = functorF.map(catsZippableF.zip(ff, fa)) { case (f, x) ⇒ f(x) }
  }
}
