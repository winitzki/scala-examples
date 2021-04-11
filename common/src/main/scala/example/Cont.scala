package example

import cats.Functor
import io.chymyst.ch.implement

final case class Cont[R, A](run: (A ⇒ R) ⇒ R) {
  def withFilter(p: A ⇒ Boolean): Cont[R, A] = this
}
// A simple implementation of the Continuation monad.
object Cont {

  implicit def functorCont[R]: Functor[Cont[R, *]] = new Functor[Cont[R, *]] {
    override def map[A, B](fa: Cont[R, A])(f: A ⇒ B): Cont[R, B] = implement
  }

  implicit def semimonadCont[R]: Semimonad[Cont[R, *]] = new Semimonad[Cont[R, *]] {
    override def flatMap[A, B](fa: Cont[R, A])(f: A ⇒ Cont[R, B]): Cont[R, B] = implement
  }
}
