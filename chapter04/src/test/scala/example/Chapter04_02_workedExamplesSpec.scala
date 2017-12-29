package example

class Chapter04_02_workedExamplesSpec extends LawChecking {

  behavior of "proving laws for functor combinations"

  it should "create a disjunction of functors" in {
    type FPlusG[F[_], G[_]] = ({type FPG[A] = Either[F[A], G[A]]})#FPG

    def makeFmap[F[_], G[_]](fMapF: FMap[F], fMapG: FMap[G]): FMap[FPlusG[F, G]] = new FMap[FPlusG[F, G]] {
      type FPG[A] = Either[F[A], G[A]]

      override def f[A, B]: (A => B) => FPG[A] => FPG[B] = f ⇒   {
        case Left(fa) ⇒ Left(fMapF.f(f)(fa))
        case Right(ga) ⇒ Right(fMapG.f(f)(ga))
      }
    }



  }

}
