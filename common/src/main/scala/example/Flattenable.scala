package example

import cats.Functor
import cats.syntax.functor._
import org.scalacheck.Arbitrary
import org.scalatest.{Assertion, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

abstract class Flattenable[F[_]](implicit val functor: Functor[F]) {
  def flatten[A](fa: F[F[A]]): F[A]
}

object Flattenable {

  // Define `.flatten` syntax.
  implicit class Syntax1[F[_], A](ffa: F[F[A]])(implicit ev: Flattenable[F]) {
    implicit val functor: Functor[F] = ev.functor

    def flatten: F[A] = ev.flatten(ffa)
  }

}

trait FlattenableLawChecking extends Matchers with GeneratorDrivenPropertyChecks {
  def checkFlattenLaws[F[_], A, B](faEqual: (F[A], F[A]) ⇒ Assertion = (x: F[A], y: F[A]) ⇒ x shouldEqual y)(implicit
    ff: Flattenable[F]
    , evffb: Arbitrary[F[F[B]]]
    , evfffa: Arbitrary[F[F[F[A]]]] // This can be slow with shapeless case class derivations!
    , evba: Arbitrary[B ⇒ A]
  ): Assertion = {
    import Flattenable._
    implicit val functorF: Functor[F] = ff.functor
    // Naturality law.
    forAll { (ffb: F[F[B]], ba: B ⇒ A) ⇒
      val fbfa: F[B] ⇒ F[A] = fb ⇒ fb.map(ba)
      val ffbffa: F[F[B]] ⇒ F[F[A]] = ffb ⇒ ffb.map(fbfa)

      // IntelliJ does not fully understand this syntax, but the code works.
      faEqual(fbfa(ffb.flatten), ffbffa(ffb).flatten)
    }

    // Associativity law.

    val flattenLifted: F[F[F[A]]] ⇒ F[F[A]] = fffa ⇒ ff.functor.map(fffa)(ff.flatten)

    forAll { fffa: F[F[F[A]]] ⇒ faEqual(ff.flatten(flattenLifted(fffa)), ff.flatten(ff.flatten(fffa))) }

  }

}
