package example

import cats.Functor
import cats.syntax.functor._

trait Filterable[F[_]] {
  def flatten[A](fa: F[Option[A]]): F[A]
}

// Syntax for PTVFs.
object Filterable {

  implicit class FilterableSyntax1[F[_], A](fa: F[Option[A]])(implicit ev: Filterable[F]) {
    def flatten: F[A] = ev.flatten(fa)
  }

  implicit class FilterableSyntax2[F[_], A](fa: F[A])(implicit ev: Filterable[F], evF: Functor[F]) {
    def filter(p: A ⇒ Boolean): F[A] = fa.map[Option[A]](a ⇒ Some(a).filter(p)).flatten

    def withFilter(p: A ⇒ Boolean): F[A] = filter(p)

    def mapOption[B](f: A ⇒ Option[B]): F[B] = fa.map(f).flatten
  }

}
