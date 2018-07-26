package example

import io.chymyst.ch.implement
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.Future

final case class Cont[R, A](run: (A ⇒ R) ⇒ R) {
  def withFilter(p: A ⇒ Boolean): Cont[R, A] = this

  def map[B](f: A ⇒ B): Cont[R, B] = implement

  def flatMap[B](f: A ⇒ Cont[R, B]): Cont[R, B] = implement
}

trait Tx[A] { self ⇒
  def run[R]: Cont[Future[R], A]
  
  def map[B](f: A ⇒ B): Tx[B] = new Tx[B] {
    def run[R]: Cont[Future[R], B] = self.run.map(f)
  }
  
  def flatMap[B](f: A ⇒ Tx[B]): Tx[B] = new Tx[B] {
    def run[R]: Cont[Future[R], B] = self.run.flatMap(a ⇒ f(a).run)
  }
}

class TransactionMonad extends FlatSpec with Matchers {

  behavior of "transaction chain"
  
  
}
