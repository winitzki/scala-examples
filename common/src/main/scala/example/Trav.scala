package example

import cats.Functor
import cats.kernel.Monoid
import cats.syntax.functor._
import cats.syntax.monoid._

abstract class Trav[L[_] : Functor] {
  def seq[F[_] : WuZip : Functor, A](lfa: L[F[A]]): F[L[A]]

  def trav[F[_] : WuZip : Functor, A, B](la: L[A])(f: A ⇒ F[B]): F[L[B]] = seq[F, B](la map f)
}

object Trav {
  // Convenience method: find a Trav evidence.
  def apply[L[_] : Trav]: Trav[L] = implicitly[Trav[L]]

  implicit class TravSyntax[L[_] : Trav : Functor, F[_] : WuZip : Functor, A](lfa: L[F[A]]) {
    def seq: F[L[A]] = Trav[L].seq[F, A](lfa)
  }

  // Convert monoids to WuZip.
  def monoidFunctor[Z: Monoid]: Functor[Lambda[X ⇒ Z]] =   new Functor[Lambda[X ⇒ Z]] {
    override def map[A, B](fa: Z)(f: A ⇒ B): Z = fa
  }
  def monoidWuZip[Z: Monoid]: WuZip[Lambda[X ⇒ Z]] = {
    implicit val functorZ = monoidFunctor[Z]
    new WuZip[Lambda[X ⇒ Z]] {
      override def wu: Z = Monoid[Z].empty

      override def zip[A, B](fa: Z, fb: Z): Z = fa |+| fb
    }
  }

  implicit class TravSyntax2[L[_] : Trav : Functor, A](la: L[A]) {
    def trav[F[_] : WuZip : Functor, B](f: A ⇒ F[B]): F[L[B]] = Trav[L].trav[F, A, B](la)(f)

    def foldMap[Z: Monoid](f: A ⇒ Z): Z = {
      implicit val functorZ = monoidFunctor[Z]
      implicit val wuZipZ = monoidWuZip[Z]
      Trav[L].trav[Lambda[X ⇒ Z], A, Z](la)(f)
    }
  }

}
