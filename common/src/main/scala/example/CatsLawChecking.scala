package example

import org.scalacheck.Arbitrary
import org.scalatest.{Assertion, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

trait CatsLawChecking extends Matchers with GeneratorDrivenPropertyChecks {

  def checkCatsMonoidLaws[M: Arbitrary](dataIsEqual: (M, M) ⇒ Assertion = (x: M, y: M) ⇒ x shouldEqual y)(implicit mm: cats.Monoid[M]): Assertion = {
    // Left and right identity laws.
    forAll { (m: M) ⇒
      dataIsEqual(mm.combine(mm.empty, m), m)
      dataIsEqual(mm.combine(m, mm.empty), m)
    }

    // Associativity law.
    forAll { (a: M, b: M, c: M) ⇒
      dataIsEqual(mm.combine(a, mm.combine(b, c)), mm.combine(mm.combine(a, b), c))
    }
  }

  // A function that can check functor laws for *any* type constructor F[_].
  def checkCatsFunctorLaws[F[_], A, B, C](fcEqual: (F[C], F[C]) ⇒ Assertion = (x: F[C], y: F[C]) ⇒ x shouldEqual y)(implicit
    ff: cats.Functor[F],
    faEv: Arbitrary[F[A]],
    fcEv: Arbitrary[F[C]],
    abEv: Arbitrary[A ⇒ B],
    bcEv: Arbitrary[B ⇒ C]
  ): Assertion = {
    // Identity law.
    forAll { (fc: F[C]) ⇒ fcEqual(ff.map(fc)(identity[C]), fc) }

    // Composition law.
    forAll { (f: A ⇒ B, g: B ⇒ C, fa: F[A]) ⇒
      fcEqual(ff.map(ff.map(fa)(f))(g), ff.map(fa)(f andThen g))
    }
  }

  def checkCatsContraFunctorLaws[F[_], A, B, C](fcEqual: (F[C], F[C]) ⇒ Assertion = (x: F[C], y: F[C]) ⇒ x shouldEqual y)(implicit
    ff: cats.Contravariant[F],
    faEv: Arbitrary[F[A]],
    fcEv: Arbitrary[F[C]],
    baEv: Arbitrary[B ⇒ A],
    cbEv: Arbitrary[C ⇒ B]
  ): Assertion = {
    // Identity law.
    forAll { (fc: F[C]) ⇒ fcEqual(ff.contramap(fc)(identity[C]), fc) }

    // Composition law.
    forAll { (f: B ⇒ A, g: C ⇒ B, fa: F[A]) ⇒
      fcEqual(ff.contramap(ff.contramap(fa)(f))(g), ff.contramap(fa)(g andThen f))
    }
  }

  def checkCatsBifunctorLaws[Q[_, _], A, B, C, D, E, F](qEqual: (Q[E, F], Q[E, F]) ⇒ Assertion = (x: Q[E, F], y: Q[E, F]) ⇒ x shouldEqual y)(implicit
    ff: cats.Bifunctor[Q],
    acEv: Arbitrary[A ⇒ C],
    bdEv: Arbitrary[B ⇒ D],
    ceEv: Arbitrary[C ⇒ E],
    dfEv: Arbitrary[D ⇒ F],
    qabEv: Arbitrary[Q[A, B]],
    qefEv: Arbitrary[Q[E, F]]
  ): Assertion = {
    // Identity law.
    forAll { (qef: Q[E, F]) ⇒ qEqual(ff.bimap(qef)(identity[E], identity[F]), qef) }

    // Composition law.
    forAll { (fac: A ⇒ C, fbd: B ⇒ D, fce: C ⇒ E, fdf: D ⇒ F, qab: Q[A, B]) ⇒
      qEqual(ff.bimap(ff.bimap(qab)(fac, fbd))(fce, fdf), ff.bimap(qab)(fac andThen fce, fbd andThen fdf))
    }
  }

}
