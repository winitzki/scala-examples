package example

import cats.{FlatMap, Functor, Monad}
import org.scalacheck.Arbitrary
import org.scalatest.Matchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks


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

trait CheckSemimonadLaws extends Matchers with GeneratorDrivenPropertyChecks {

  def checkSemimonadLaws[F[_] : Semimonad, A, B, C](implicit fa: Arbitrary[F[A]], ab: Arbitrary[A => F[B]], bc: Arbitrary[B => F[C]]) = {
    import Semimonad.SemimonadSyntax
    forAll { (f: A => F[B], g: B => F[C], fa: F[A]) =>
      fa.flatMap(x => f(x).flatMap(g)) shouldEqual fa.flatMap(f).flatMap(g)
    }
  }

  def checkMonadLaws[F[_], A, B]()(implicit mf: Monad[F],
                                   aa: Arbitrary[A], af: Arbitrary[F[A]], ab: Arbitrary[A => F[B]]) = {
    import cats.syntax.flatMap._
    forAll { (x: A, g: A => F[B]) =>
      mf.pure(x).flatMap(g) shouldEqual g(x)   // Left identity law.
    }
    forAll { (fa: F[A]) =>
      fa.flatMap(mf.pure[A]) shouldEqual fa   // Right identity law.
    }
  }
}
