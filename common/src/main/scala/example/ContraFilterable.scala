package example

import cats.{Contravariant, Functor}
import cats.syntax.functor._
import cats.syntax.contravariant._
import org.scalacheck.Arbitrary
import org.scalatest.{Assertion, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import Filterable.{flip, optB}

abstract class ContraFilterableWithFilter[F[_]](implicit val contrafunctor: Contravariant[F]) {
  // For functors that implement `withFilter` directly.
  def withFilter[A](p: A ⇒ Boolean)(fa: F[A]): F[A]
}

// A `Contravariant` instance is required for creating a `Filterable` instance.
abstract class ContraFilterable[F[_]](implicit val contrafunctor: Contravariant[F]) {
  // The `inflate` is the easiest type signature to implement.
  def inflate[A](fa: F[A]): F[Option[A]]
}

// Syntax for PTVFs.
object ContraFilterable {

  // Define `.inflate` syntax.
  implicit class Syntax1[C[_], A](fa: C[A])(implicit ev: ContraFilterable[C]) {
    def inflate: C[Option[A]] = ev.inflate(fa)
  }

  // Define `.contramapOption`, `.filter` and `.withFilter` syntax if we already have `.inflate` syntax.
  implicit class Syntax2[C[_], A](fa: C[A])(implicit ev: ContraFilterable[C]) {
    implicit val contrafunctor: Contravariant[C] = ev.contrafunctor

    def contramapOption[B](f: B ⇒ Option[A]): C[B] = fa.inflate.contramap(f)

    def filter(p: A ⇒ Boolean): C[A] = fa.inflate.contramap[A](optB(p))

    def withFilter(p: A ⇒ Boolean): C[A] = filter(p)
  }

  // Define `.contramapOption`, `.withFilter`, `.filter`, and `.inflate` syntax if we already have `withFilter`.
  implicit class Syntax3[C[_], A](fa: C[A])(implicit ev: ContraFilterableWithFilter[C]) {
    implicit val contrafunctor: Contravariant[C] = ev.contrafunctor

    def contramapOption[B](f: B ⇒ Option[A]): C[B] = fa.inflate.contramap(f)

    def withFilter(p: A ⇒ Boolean): C[A] = ev.withFilter(p)(fa)

    def filter(p: A ⇒ Boolean): C[A] = withFilter(p)

    def inflate: C[Option[A]] = fa.contramap[Option[A]](_.get).withFilter(_ ⇒ true)
  }

}

trait ContraFilterableLawChecking extends Matchers with GeneratorDrivenPropertyChecks {
  // Check the four laws for `filter`.
  def checkContraFilterableLawsWithFilter[C[_] : ContraFilterableWithFilter, A, B](fcEqual: (C[B], C[B]) ⇒ Assertion = (x: C[B], y: C[B]) ⇒ x shouldEqual y)(implicit
    ff: cats.Contravariant[C],
    faEv: Arbitrary[C[A]],
    fcEv: Arbitrary[C[B]],
    abEv: Arbitrary[B ⇒ A],
    aEv: Arbitrary[A ⇒ Boolean],
    bEv: Arbitrary[B ⇒ Boolean]
  ): Assertion = {
    import ContraFilterable._

    // Naturality / parametricity law.
    forAll { (f: B ⇒ A, p: A ⇒ Boolean, fa: C[A]) ⇒
      fcEqual(fa.filter(p).contramap(f), fa.contramap(f).filter(f andThen p))
    }

    // Conjunction law.
    forAll { (p1: B ⇒ Boolean, p2: B ⇒ Boolean, fa: C[B]) ⇒
      fcEqual(fa.filter(p1).filter(p2), fa.filter(b ⇒ p1(b) && p2(b)))
    }

    // Identity law.
    forAll { (fb: C[B]) ⇒ fcEqual(fb.filter(_ ⇒ true), fb) }

    // Partial function law.
    forAll { (f: B ⇒ A, p: B ⇒ Boolean, fa: C[A]) ⇒
      fcEqual(fa.contramap(f).filter(p), fa.contramap[B] { case x if p(x) ⇒ f(x) }.filter(p))
    }
  }

  // Check the two laws for `contramapOption`.
  def checkContraFilterableLaws[D[_] : ContraFilterable, A, B, C](fcEqual: (D[C], D[C]) ⇒ Assertion = (x: D[C], y: D[C]) ⇒ x shouldEqual y)(implicit
    faEv: Arbitrary[D[A]],
    fcEv: Arbitrary[D[C]],
    abEv: Arbitrary[B ⇒ Option[A]],
    bcEv: Arbitrary[C ⇒ Option[B]]
  ): Assertion = {
    import ContraFilterable._

    // Identity law.
    forAll { (fc: D[C]) ⇒ fcEqual(fc.contramapOption(Some.apply[C]), fc) }

    // Composition law.
    forAll { (f: B ⇒ Option[A], g: C ⇒ Option[B], fa: D[A]) ⇒
      fcEqual(fa.contramapOption(f).contramapOption(g), fa.contramapOption(x ⇒ g(x) flatMap f))
    }
  }

}
