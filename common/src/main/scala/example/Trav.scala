package example

import cats.Functor
import cats.syntax.functor._

abstract class Trav[L[_] : Functor] {
  def seq[F[_] : WuZip : Functor, A](lfa: L[F[A]]): F[L[A]]

  def trav[F[_] : WuZip : Functor, A, B](la: L[A])(f: A ⇒ F[B]): F[L[B]] = seq[F, B](la map f)
}

object Trav {
  // Convenience method: find a Trav evidence.
  def apply[L[_]: Trav]: Trav[L] = implicitly[Trav[L]]

  implicit class TravSyntax[L[_] : Trav : Functor, F[_] : WuZip : Functor, A](lfa: L[F[A]]) {
    def seq: F[L[A]] = Trav[L].seq[F, A](lfa)
  }

}
