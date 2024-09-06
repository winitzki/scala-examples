package example

import cats.Functor
import cats.syntax.functor._
import example.CatsMonad.CatsMonadSyntax
import org.scalatest.{FlatSpec, Matchers}

import scala.util.{Failure, Success, Try}

final case class SeqTry[A](value: Seq[Try[A]]) extends AnyVal {
  def map[B](f: A ⇒ B): SeqTry[B] = SeqTry(value.map(_.map(f)))

  def flatMap[B](f: A ⇒ SeqTry[B]): SeqTry[B] = SeqTry(value.flatMap(t ⇒ SeqTry.traverse((x: A) ⇒ f(x).value)(t).map(_.flatten)))

  def withFilter(p: A ⇒ Boolean): SeqTry[A] = SeqTry(value.filter(_.filter(p).isSuccess))
}

object SeqTry {
  def pure[A](x: A): SeqTry[A] = SeqTry(Seq(Success(x)))

  def sequence[A]: Try[Seq[A]] ⇒ Seq[Try[A]] = {
    case Success(v) ⇒ v.map(Success.apply)
    case Failure(e) ⇒ Seq(Failure(e))
  }

  def traverse[A, B](f: A ⇒ Seq[B]): Try[A] ⇒ Seq[Try[B]] = {
    case Success(v) ⇒ f(v).map(Success.apply)
    case Failure(e) ⇒ Seq(Failure(e))
  }

  implicit def tryToSeqTry[A]: Try[A] ⇒ SeqTry[A] = t ⇒ SeqTry(sequence(t.map(x ⇒ Seq(x))))

  implicit def seqToSeqTry[A]: Seq[A] ⇒ SeqTry[A] = s ⇒ SeqTry(s.map(Success.apply))

  //  implicit def trySeqToSeqTry[A]: Try[Seq[A]] ⇒ SeqTry[A] = ts ⇒ SeqTry(sequence(ts))

  implicit class TryWithLoggedFailure[A](t: Try[A]) {
    def logFailure(logFunction: Throwable ⇒ Unit): Try[A] = t.recoverWith { case throwable: Throwable ⇒
      logFunction(throwable)
      Failure(throwable) // Do not actually recover, just log the failure.
    }
  }

  implicit class LiftTry[A](val x: Try[A]) extends AnyVal {
    def up: SeqTry[A] = x
  }

  implicit class LiftSeq[A](val x: Seq[A]) extends AnyVal {
    def up: SeqTry[A] = x
  }

}

class SeqTryRecoverSpec extends FlatSpec with Matchers {

  import SeqTry._

  behavior of "Seq/Try monad transformer with recovery"

  it should "perform a sequence of operations that may fail" in {
    // Write a for/yield block, but each expression on the right-hand side is "lifted" using the special `.up` method.
    // The right-hand side can be a Try, a List, or a SeqTry.
    // Iterations continue over failures. Failures can be recovered using Try().recover(), or just logged using `Try().logFailure()`.
    val result: SeqTry[Int] = for {
      values ← Try(List(1, 2, 3)).up
      x ← values.up
      y ← Try(6 / (x - 1)).logFailure(_ ⇒ println(s"failure with $x")).up
      // The first will fail, the others will not. But iterations will continue. The result is that y = [<failure>, 6, 3].
      z ← (1 to y).up // The result is z = [<failure>, 1, 2, 3, 4, 5, 6, 1, 2, 3]
      t ← Try(24 / math.abs(z - 2)).up // Some of them will fail but iterations will continue.
      // The result is: t = [<failure>, 24, <failure>, 24, 12, 8, 6, 24, <failure>, 24]
    } yield t

    // To verify the results quickly, replace the errors by the special `failure` value -12345678 that
    // will not be normally present in the output.
    val failure = -12345678
    result.value.map(_.recover { case _ ⇒ failure }).map(_.get) shouldEqual List(failure, 24, failure, 24, 12, 8, 6, 24, failure, 24)
  }

}


class CoprodSpec extends FlatSpec with Matchers {

  type M[A] = Coprod[Seq, Try, A]

  implicit val functorSeq: Functor[Seq] = new Functor[Seq] {
    override def map[A, B](fa: Seq[A])(f: A => B): Seq[B] = fa.map(f)
  }

  implicit val catsMonadSeq: CatsMonad[Seq] = new CatsMonad[Seq] {
    override def flatMap[A, B](fa: Seq[A])(f: A => Seq[B]): Seq[B] = fa.flatMap(f)

    override def pure[A](x: A): Seq[A] = Seq(x)
  }

  implicit val functorTry: Functor[Try] = new Functor[Try] {
    override def map[A, B](fa: Try[A])(f: A => B): Try[B] = fa.map(f)
  }
  implicit val catsMonadTry: CatsMonad[Try] = new CatsMonad[Try] {
    override def flatMap[A, B](fa: Try[A])(f: A => Try[B]): Try[B] = fa.flatMap(f)

    override def pure[A](x: A): Try[A] = Success(x)
  }

  implicit class COps1[A](x: Seq[A]) {
    def up: M[A] = Coprod.inLeft[Seq, Try, A].apply(x)
  }

  implicit class COps2[A](x: Try[A]) {
    def up: M[A] = Coprod.inRight[Seq, Try, A].apply(x)
  }

  def toList[A]: M[A] => Seq[A] = { ma =>
    ma.run[Seq].apply(new NatTrans[Seq, Seq] {
      override def run[A]: Seq[A] => Seq[A] = identity
    })(new NatTrans[Try, Seq] {
      override def run[A]: Try[A] => Seq[A] = {
        case Failure(_) => Seq()
        case Success(x) => Seq(x)
      }
    })
  }

  val test: M[Int] = for {
    x <- List(1, 2, 3).up
    y <- Try(100 / (2 - x)).up
  } yield y

  it should "convert a sequence but exclude failures" in {
    val result: Seq[Int] = toList(test)
    result shouldEqual Seq(100, -100)
  }

}

trait NatTrans[F[_], G[_]] {
  def run[A]: F[A] => G[A]
}

abstract class Coprod[R[_] : CatsMonad : Functor, S[_] : CatsMonad : Functor, A] {
  def run[T[_] : CatsMonad : Functor]: NatTrans[R, T] => NatTrans[S, T] => T[A]
}

object Coprod {
  def inLeft[R[_] : CatsMonad : Functor, S[_] : CatsMonad : Functor, A]: R[A] => Coprod[R, S, A] = ra => new Coprod[R, S, A] {
    override def run[T[_] : CatsMonad : Functor]: NatTrans[R, T] => NatTrans[S, T] => T[A] = rt => st => rt.run(ra)
  }

  def inRight[R[_] : CatsMonad : Functor, S[_] : CatsMonad : Functor, A]: S[A] => Coprod[R, S, A] = sa => new Coprod[R, S, A] {
    override def run[T[_] : CatsMonad : Functor]: NatTrans[R, T] => NatTrans[S, T] => T[A] = rt => st => st.run(sa)
  }


  implicit def functorCoprod[R[_] : CatsMonad : Functor, S[_] : CatsMonad : Functor]: Functor[Lambda[X => Coprod[R, S, X]]] = new Functor[Lambda[X => Coprod[R, S, X]]] {
    override def map[A, B](fa: Coprod[R, S, A])(f: A => B): Coprod[R, S, B] = new Coprod[R, S, B] {
      override def run[T[_] : CatsMonad : Functor]: NatTrans[R, T] => NatTrans[S, T] => T[B] = nts => ntt => fa.run[T].apply(nts)(ntt).map(f)
    }
  }

  implicit def catsMonadCoprod[R[_] : CatsMonad : Functor, S[_] : CatsMonad : Functor]: CatsMonad[Lambda[X => Coprod[R, S, X]]] = new CatsMonad[Lambda[X => Coprod[R, S, X]]] {

    override def pure[A](x: A): Coprod[R, S, A] = new Coprod[R, S, A] {
      override def run[T[_] : CatsMonad : Functor]: NatTrans[R, T] => NatTrans[S, T] => T[A] = nts => ntt => CatsMonad[T].pure(x)
    }

    override def flatMap[A, B](fa: Coprod[R, S, A])(f: A => Coprod[R, S, B]): Coprod[R, S, B] = new Coprod[R, S, B] {
      override def run[T[_] : CatsMonad : Functor]: NatTrans[R, T] => NatTrans[S, T] => T[B] = nts => ntt => fa.run[T].apply(nts)(ntt).flatMap(a => f(a).run[T].apply(nts)(ntt))
    }
  }

}

class CoprodStackSpec extends FlatSpec with Matchers {

  case class State[A](run: Int => (A, Int))

  implicit val functorState: Functor[State] = new Functor[State] {
    override def map[A, B](fa: State[A])(f: A => B): State[B] = State { i =>
      fa.run(i) match {
        case (x, newState) => (f(x), newState)
      }
    }
  }

  implicit val catsMonadState: CatsMonad[State] = new CatsMonad[State] {

    override def flatMap[A, B](fa: State[A])(f: A => State[B]): State[B] = State { i =>
      fa.run(i) match {
        case (x, newState) => f(x).run(newState)
      }
    }

    override def pure[A](x: A): State[A] = State { i => (x, i) }
  }

  implicit val functorOption: Functor[Option] = new Functor[Option] {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  implicit val catsMonadOption: CatsMonad[Option] = new CatsMonad[Option] {

    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

    override def pure[A](x: A): Option[A] = Some(x)
  }

  type Stack0[A] = Coprod[State, Option, A]

  val stateStateTrans: NatTrans[State, State] = new NatTrans[State, State] {
    override def run[B]: State[B] => State[B] = identity
  }

  val optionStateTrans: NatTrans[Option, State] = new NatTrans[Option, State] {
    override def run[B]: Option[B] => State[B] = {
      case None => State { i => (null.asInstanceOf[B], i) }
      case Some(x) => State { i => (x, i) }
    }
  }

  def toState0[A](s: Stack0[A]): State[A] = s.run[State].apply(stateStateTrans)(optionStateTrans)

  case class Stack1[A](run: Int => (Option[A], Int)) { // OptionT[State][A] = S[Option[A]]
    def toState: State[A] = State { i =>
      run(i) match {
        case (o, newState) => (o.getOrElse(null.asInstanceOf[A]), newState)
      }
    }
  }

  case class Stack2[A](run: Int => Option[(A, Int)]) { // StateT[Option][A]
    def toState: State[A] = State { i =>
      run(i).getOrElse((null.asInstanceOf[A], i))
    }
  }

  // Define typeclass instances and lifting operations for the two monad stacks.

  implicit val functorStack1: Functor[Stack1] = new Functor[Stack1] {
    override def map[A, B](fa: Stack1[A])(f: A => B): Stack1[B] = Stack1 { i =>
      val (x, newState) = fa.run(i)
      (x.map(f), newState)
    }
  }

  implicit val catsMonadStack1: CatsMonad[Stack1] = new CatsMonad[Stack1] {

    override def flatMap[A, B](fa: Stack1[A])(f: A => Stack1[B]): Stack1[B] = Stack1 { i =>
      val (x, newState) = fa.run(i)
      x.map(f) match {
        case Some(value) => value.run(newState)
        case None => (None, newState)
      }
    }

    override def pure[A](x: A): Stack1[A] = Stack1 { i => (Some(x), i) }
  }

  implicit val functorStack2: Functor[Stack2] = new Functor[Stack2] {
    override def map[A, B](fa: Stack2[A])(f: A => B): Stack2[B] = Stack2 { i =>
      fa.run(i).map { case (x, newState) => (f(x), newState) }
    }
  }

  implicit val catsMonadStack2: CatsMonad[Stack2] = new CatsMonad[Stack2] {
    override def flatMap[A, B](fa: Stack2[A])(f: A => Stack2[B]): Stack2[B] = Stack2 { i =>
      fa.run(i).flatMap { case (x, newState) => f(x).run(newState) }
    }

    override def pure[A](x: A): Stack2[A] = Stack2 { i => Some(x, i) }
  }

  implicit class COps1[A](x: State[A]) {
    def up0: Stack0[A] = Coprod.inLeft[State, Option, A].apply(x)

    def up1: Stack1[A] = Stack1 { i =>
      x.run(i) match {
        case (y, newState) => (Some(y), newState)
      }
    }

    def up2: Stack2[A] = Stack2 { i =>
      x.run(i) match {
        case (y, newState) => Some((y, newState))
      }
    }
  }

  implicit class COps2[A](x: Option[A]) {
    def up0: Stack0[A] = Coprod.inRight[State, Option, A].apply(x)

    def up1: Stack1[A] = Stack1 { i => (x, i) }

    def up2: Stack2[A] = Stack2 { i => x.map((_, i)) }
  }


  val test0: Stack0[(Int, Int)] = for {
    x <- Option(1).up0
    y <- State { i => (x, i + 1) }.up0
    _ <- (None: Option[Int]).up0
    z <- State { i => ((y, i + 10), i + 10) }.up0
  } yield z


  val test1: Stack1[(Int, Int)] = for {
    x <- Option(1).up1
    y <- State { i => (x, i + 1) }.up1
    _ <- (None: Option[Int]).up1
    z <- State { i => ((y, i + 10), i + 10) }.up1
  } yield z


  val test2: Stack2[(Int, Int)] = for {
    x <- Option(1).up2
    y <- State { i => (x, i + 1) }.up2
    _ <- (None: Option[Int]).up2
    z <- State { i => ((y, i + 10), i + 10) }.up2
  } yield z

  it should "execute monadic program in Stack0" in {
   toState0(test0).run(0) shouldEqual (((1, 11), 11))
  }

  it should "execute monadic program in Stack1" in {
    test1.toState.run(0) shouldEqual ((null, 1))
  }

  it should "execute monadic program in Stack2" in {
    test2.toState.run(0) shouldEqual ((null, 0))
  }

}

import scala.concurrent.{ExecutionContext, Future}

object FutureCloseable {
  def using[T, R <: AutoCloseable](r: => R)(program: R => Future[T])(implicit ec: ExecutionContext): Future[T] = {
    val futureResult: Future[T] = for {
      resource ← Future(r) // If this fails, we will just return the resulting failed future value.
      // Only recover the given program. Do not recover from failure creating the resource.
      result ← program(resource).recoverWith {
        case e: Throwable ⇒
          Try(resource.close()) match {
            case Failure(e2) ⇒
              e.addSuppressed(e2)
            case Success(_) ⇒
          }
          Future.failed(e)
      }
    } yield {
      Try(resource.close()) // We ignore any exceptions here.
      result
    }

    futureResult
  }
}

// Problem: this does not work together with continuations.
