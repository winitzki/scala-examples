package example

import cats.Functor
import cats.syntax.functor._
import org.scalacheck.Arbitrary
import org.scalatest.{Assertion, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

/*
If a PTVF p1 is defined for all the same types as p2 and also for some other types,
we say that p2 requires p1.
We want to express this. Also, we want to use both p1 and p2 on types for which p2 is defined,
by just using a p2 type class constraint, without explicitly adding a p1 type class constraint.
How to express this?
scalaz and cats use traits that extend each other - and then you can't add syntax, and have to override the functions in the trait. This looks suspicious. Is there another way? Yes.
 */

abstract class FilterableWithFilter[F[_]](implicit val functor: Functor[F]) {
  // For functors that implement `withFilter` directly.
  def withFilter[A](p: A ⇒ Boolean)(fa: F[A]): F[A]
}

// A `Functor` instance is required for creating a `Filterable` instance.
abstract class Filterable[F[_]](implicit val functor: Functor[F]) {
  // The `flatten` is the easiest type signature to implement.
  def flatten[A](fa: F[Option[A]]): F[A]
}

// Syntax for PTVFs.
object Filterable {

  implicit class FilterableSyntax1[F[_], A](fa: F[Option[A]])(implicit ev: Filterable[F]) {
    def flatten: F[A] = ev.flatten(fa)
  }

  implicit class FilterableSyntax2[F[_], A](fa: F[A])(implicit ev: Filterable[F]) {
    def mapOption[B](f: A ⇒ Option[B]): F[B] = ev.functor.map(fa)(f).flatten

    def filter(p: A ⇒ Boolean): F[A] = mapOption(a ⇒ Some(a).filter(p))

    def withFilter(p: A ⇒ Boolean): F[A] = filter(p)
  }

  implicit class FilterableSyntax3[F[_], A](fa: F[A])(implicit ev: FilterableWithFilter[F]) {

    implicit val functor: Functor[F] = ev.functor

    def mapOption[B](f: A ⇒ Option[B]): F[B] = fa.map(f).flatten

    def withFilter(p: A ⇒ Boolean): F[A] = ev.withFilter(p)(fa)

    def filter(p: A ⇒ Boolean): F[A] = withFilter(p)

    def flatten[B](implicit aOpt: A =:= Option[B]): F[B] =
      fa.map(aOpt.apply).withFilter(_.nonEmpty).map { case Some(x) ⇒ x }
  }

}

trait FilterableLawChecking extends Matchers with GeneratorDrivenPropertyChecks {
  // Check the four laws for `filter`.
  def checkFilterableLawsWithFilter[F[_] : FilterableWithFilter, A, B](fcEqual: (F[B], F[B]) ⇒ Assertion = (x: F[B], y: F[B]) ⇒ x shouldEqual y)(implicit
    ff: cats.Functor[F],
    faEv: Arbitrary[F[A]],
    fcEv: Arbitrary[F[B]],
    abEv: Arbitrary[A ⇒ B],
    aEv: Arbitrary[A ⇒ Boolean],
    bEv: Arbitrary[B ⇒ Boolean]
  ): Assertion = {
    import Filterable._

    // Naturality law.
    forAll { (f: A ⇒ B, p: B ⇒ Boolean, fa: F[A]) ⇒
      fcEqual(fa.map(f).filter(p), fa.filter(f andThen p).map(f))
    }

    // Conjunction law.
    forAll { (p1: B ⇒ Boolean, p2: B ⇒ Boolean, fa: F[B]) ⇒
      fcEqual(fa.filter(p1).filter(p2), fa.filter(b ⇒ p1(b) && p2(b)))
    }

    // Identity law.
    forAll { (fb: F[B]) ⇒ fcEqual(fb.filter(_ ⇒ true), fb) }

    // Partial function law.
    forAll { (f: A ⇒ B, p: A ⇒ Boolean, fa: F[A]) ⇒
      fcEqual(fa.filter(p).map(f), fa.filter(p).map[B] { case x if p(x) ⇒ f(x) })
    }
  }

  // Check the two laws for `mapOption`.
  def checkFilterableLaws[F[_] : Filterable, A, B, C](fcEqual: (F[C], F[C]) ⇒ Assertion = (x: F[C], y: F[C]) ⇒ x shouldEqual y)(implicit
    faEv: Arbitrary[F[A]],
    fcEv: Arbitrary[F[C]],
    abEv: Arbitrary[A ⇒ Option[B]],
    bcEv: Arbitrary[B ⇒ Option[C]]
  ): Assertion = {
    import Filterable._
    // Identity law.
    forAll { (fc: F[C]) ⇒ fcEqual(fc.mapOption(Some.apply[C]), fc) }

    // Composition law.
    forAll { (f: A ⇒ Option[B], g: B ⇒ Option[C], fa: F[A]) ⇒
      fcEqual(fa.mapOption(f).mapOption(g), fa.mapOption(x ⇒ f(x) flatMap g))
    }
  }

}
