package example

import cats.Functor
import cats.evidence.Is
import cats.syntax.functor._
import Filterable.CurriedFlip
import org.scalacheck.Arbitrary
import org.scalatest.{Assertion, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

abstract class Flattenable[F[_]](implicit val functor: Functor[F]) {
  def flatten[A](fa: F[F[A]]): F[A]
}

object Flattenable {

  // Define `.flatten` syntax. Does not work!
//  implicit class Syntax1[F[_], A](ffa: F[F[A]])(implicit ev: Flattenable[F]) {
//    implicit val functor: Functor[F] = ev.functor
//
//    def flatten: F[A] = ev.flatten(ffa)
//  }

}

trait FlattenableLawChecking extends Matchers with GeneratorDrivenPropertyChecks {
  def checkFlattenLaws[F[_], A, B](faEqual: (F[A], F[A]) ⇒ Assertion = (x: F[A], y: F[A]) ⇒ x shouldEqual y)(implicit
    ff: Flattenable[F]
    , evffb: Arbitrary[F[F[B]]]
    , evfffa: Arbitrary[F[F[F[A]]]]
    , evba: Arbitrary[B ⇒ A]
  ): Assertion = {
    import Flattenable._
    // Naturality law.

    forAll { (ffb: F[F[B]], ba: B ⇒ A) ⇒
      val fbfa: F[B] ⇒ F[A] = fb ⇒ ff.functor.map(fb)(ba)
      val ffbffa: F[F[B]] ⇒ F[F[A]] = ffb ⇒ ff.functor.map(ffb)(fbfa)
      faEqual(fbfa(ff.flatten(ffb)), ff.flatten(ffbffa(ffb)))
    }


    // Associativity law.

    val flattenLifted: F[F[F[A]]] ⇒ F[F[A]] = fffa ⇒ ff.functor.map(fffa)(ff.flatten)

    forAll { fffa: F[F[F[A]]] ⇒ faEqual(ff.flatten(flattenLifted(fffa)), ff.flatten(ff.flatten(fffa))) }

  }

}
