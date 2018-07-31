package example

import cats.{Applicative, Contravariant, Functor, Invariant}

// Define the Applicative type class in a simpler way using `wu` and `zip`.
abstract class WuZip[F[_]](implicit val functorF: Functor[F]) {
  def wu: F[Unit]

  def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)]

  // Define `pure` through `wu`.
  def pure[A](x: A): F[A] = functorF.map(wu)(_ ⇒ x)
}

object WuZip {
  // Convenience methods.
  def apply[F[_] : WuZip]: WuZip[F] = implicitly[WuZip[F]]

  def wU[F[_] : WuZip]: F[Unit] = apply[F].wu

  // If desired, can define a `cats` `Applicative` instance through `wu` and `zip`.
  def toCatsApplicative[F[_] : WuZip]: Applicative[F] = new Applicative[F] {
    override def pure[A](x: A): F[A] = WuZip[F].pure(x)

    override def ap[A, B](ff: F[A ⇒ B])(fa: F[A]): F[B] =
      WuZip[F].functorF.map(WuZip[F].zip(ff, fa)) { case (f, x) ⇒ f(x) }
  }

  // Syntax: infix `zip`.
  implicit class WuZipSyntax[F[_] : WuZip, A](fa: F[A]) {
    def zip[B](fb: F[B]): F[(A, B)] = WuZip[F].zip(fa, fb)
  }

}

abstract class ContraWuZip[F[_]](implicit val contrafunctorF: Contravariant[F]) {
  def wu: F[Unit]

  def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)]

  // Define `pure`: F[A] through `wu`.
  def pure[A]: F[A] = contrafunctorF.contramap(wu)(_ ⇒ ())
}

object ContraWuZip {
  def apply[F[_] : ContraWuZip]: ContraWuZip[F] = implicitly[ContraWuZip[F]]

  def wU[F[_] : ContraWuZip]: F[Unit] = apply[F].wu

  // Syntax: infix `zip`.
  implicit class ContraWuZipSyntax[F[_] : ContraWuZip, A](fa: F[A]) {
    def zip[B](fb: F[B]): F[(A, B)] = ContraWuZip[F].zip(fa, fb)
  }

  // The `cats` library has `ContravariantSemigroupal` but not `ContraApplicative`.
}

abstract class ProWuZip[F[_]](implicit val profunctorF: Invariant[F]) {
  def wu: F[Unit]

  def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)]

  // Define `pure`: F[A] through `wu`.
  def pure[A](a: A): F[A] = profunctorF.imap(wu)(_ ⇒ a)(_ ⇒ ())
}

object ProWuZip {
  def apply[F[_] : ProWuZip]: ProWuZip[F] = implicitly[ProWuZip[F]]

  def wU[F[_] : ProWuZip]: F[Unit] = apply[F].wu

  // Syntax: infix `zip`.
  implicit class ProWuZipSyntax[F[_] : ProWuZip, A](fa: F[A]) {
    def zip[B](fb: F[B]): F[(A, B)] = ProWuZip[F].zip(fa, fb)
  }

  // The `cats` library has `InvariantSemigroupal` but not `ProApplicative`.
}
