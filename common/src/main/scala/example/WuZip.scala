package example

import cats.{Applicative, Functor}

// Define the Applicative type class in a simpler way using `wu` and `zip`.
abstract class WuZip[F[_]](implicit val functorF: Functor[F]) {
  def wu: F[Unit]

  def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)]

  // Define `pure` through `wu`.
  def pure[A](x: A): F[A] = functorF.map(wu)(_ ⇒ x)
}

object WuZip {

  def apply[F[_] : WuZip]: WuZip[F] = implicitly[WuZip[F]]

  // If desired, can define a `cats` `Applicative` instance through `wu` and `zip`.
  def toCatsApplicative[F[_] : WuZip]: Applicative[F] = new Applicative[F] {
    override def pure[A](x: A): F[A] = WuZip[F].pure(x)

    override def ap[A, B](ff: F[A ⇒ B])(fa: F[A]): F[B] =
      WuZip[F].functorF.map(WuZip[F].zip(ff, fa)) { case (f, x) ⇒ f(x) }
  }
}
