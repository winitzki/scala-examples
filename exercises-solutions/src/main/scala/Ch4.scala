package swscala

import scala.reflect.ClassTag


object Ch4Ex1 {

  // Problem 1
  // Data[A] ≡ (1 + A) × (1 + A) × String
  // Covariant
  final case class P1Data[A](d1: Option[A], d2: Option[A], d3: String)

  def p1Fmap[A, B](f: A => B): P1Data[A] => P1Data[B] = { p1DA =>
    P1Data(p1DA.d1.map(f), p1DA.d2.map(f), p1DA.d3)
  }


  // Problem 2
  // Data[A] ≡ (A ⇒ String) ⇒ (A × (Int + A))
  // Covariant
  final case class P2Data[A](d: (A => String) => (A, Either[Int, A]))

  def p2Fmap[A, B](f: A => B): P2Data[A] => P2Data[B] = { p2DA =>
    val dB: (B => String) => (B, Either[Int, B]) = { bToString =>
      val aToString: A => String = a => bToString(f(a))
      val (a: A, intOrA: Either[Int, A]) = p2DA.d(aToString)
      val b: B = f(a)
      val iOrB: Either[Int, B] = intOrA match {
        case Left(i) => Left(i)
        case Right(a) => Right(f(a))
      }
      (b, iOrB)
    }
    P2Data(dB)
  }


  // Problem 3
  // Data[A, B] ≡ (A ⇒ String) × ((A + B) ⇒ Int)
  // Contravariant
  type P3Data[A, B] = ((A ⇒ String), (Either[A, B] ⇒ Int))

  // B => C
  def p3ContraFmapBC[A, B, C](f: C => B): P3Data[A, B] => P3Data[A, C] = { p3DAB =>
    val (aToString, aOrBToInt) = p3DAB
    val aOrCToInt: Either[A, C] => Int = {
      case Left(a) => aOrBToInt(Left(a))
      case Right(c) => aOrBToInt(Right(f(c)))
    }
    (aToString, aOrCToInt)
  }

  // A => C
  def p3ContraFmapAC[A, B, C](f: C => A): P3Data[A, B] => P3Data[C, B] = { p3DAB =>
    val (aToString, aOrBToInt) = p3DAB
    val cToString: C => String = c => aToString(f(c))
    val cOrBToInt: Either[C, B] => Int = {
      case Left(c) => aOrBToInt(Left(f(c)))
      case Right(b) => aOrBToInt(Right(b))
    }
    (cToString, cOrBToInt)
  }


  // Problem 4
  // Data[A] ≡ (1 + (A ⇒ String)) ⇒ (1 + (A ⇒ Int)) ⇒ Int
  // Covariant
  // The following will pass:
  // final case class P4Data[A](d: Option[A ⇒ String] ⇒ Option[A ⇒ Int] ⇒ Int)
  // import io.chymyst.ch._
  // def p4Fmap[A, B](f: A => B): P4Data[A] => P4Data[B] = implement
  // not pass: def p4ContraFmap[A, B](f: B => A): P4Data[A] => P4Data[B] = implement
  type P4Data[A] = Option[A => String] => Option[A => Int] => Int
  def p4Fmap[A, B](f: A => B): P4Data[A] => P4Data[B] = { p4DataA =>
    // p4DataA: Option[A => String] => Option[A => Int] => Int

    val p4DataB: Option[B ⇒ String] ⇒ Option[B ⇒ Int] ⇒ Int = { optBToString => optBToInt =>
      // optBToString: Option[B => String]
      // optBToInt: Option[B => Int]

      // First produce Option[A => Int] => Int using p4DataA and optBToString
      val optToIntA: Option[A => Int] => Int = optBToString match {
        case Some(bToStr) => {
          val aToStr: A => String = a => bToStr(f(a))
          p4DataA(Some(aToStr))
        }
        case None => {
          p4DataA(None)
        }
      }

      // Second produce Int from optBToInt using optToIntA and optBToInt
      val i2: Int = optBToInt match {
        case Some(bToInt) => {
          val aToInt: A => Int = a => bToInt(f(a))
          optToIntA(Some(aToInt))
        }
        case None => optToIntA(None)
      }
      // Must return integer
      i2
    }
    p4DataB
  }


  // Problem 5
  // Data[B] ≡ (B + (Int ⇒ B)) × (B + (String ⇒ B))
  // Covariant
  type P5Data[B] = (BOrXToB[B, Int], BOrXToB[B, String])

  // Sub-structure is covariant wrt B since it's not consumed
  type BOrXToB[B, X] = Either[B, X => B]

  def fmapBOrXToB[B, C, X](f: B => C): BOrXToB[B, X] => BOrXToB[C, X] = {
    case Left(b) => Left(f(b))
    case Right(xToB) => Right(x => f(xToB(x)))
  }

  def p5Fmap[B, C](f: B => C): P5Data[B] => P5Data[C] = { p5DB =>
    val (bOrIntToB, bOrStrToB) = p5DB
    (fmapBOrXToB(f)(bOrIntToB), fmapBOrXToB(f)(bOrStrToB))
  }


  // Problem 6
  // Short notation:
  // Result[A, B] = (A x B x C) + ((Int => A) x (Int => B)) + (A => A, A => B)
  // A is both consumed and produced, so it is invariant
  // B is never consumed, so it is covariant
  sealed trait Result[A, +B]
  case class P[A, +B](a: A, b: B, c: Int) extends Result[A,B]
  case class Q[A, +B](d: Int⇒A, e: Int⇒B) extends Result[A,B]
  case class R[A, +B](f: A⇒A, g: A⇒B) extends Result[A,B]

}


object Ch4Ex2 {

  /**
    Problem 1

    Given functors F[A] and G[A] prove F[A] x G[A] is also a functor
    We know following functions exist: fmapF(f): F[A] => F[B] and fmapG(f): G[A] => G[B]

    Define fmap<FxG>[A, B](f: A => B) = (p: F[A], q: G[A]) => (fmapF(f)(p), fmapG(f)(q))

    Check identity
    fmap(id[A])((p, q)) = (fmapF(id[A])(p), fmapG(id[A])(q)) = (p, q)

    Check composition
    (fmap<FxG>(f1) ◦ fmap<FxG>(f2))(p, q)
    = fmap(f2)(fmapF(f1)(p), fmapG(f1)(q))
    = ((fmapF(f1) ◦ fmapF(f2))(p), (fmapG(f1) ◦ fmapG(f2))(q)
    = (fmapF(f1 ◦ f2)(p), fmapG(f1 ◦ f2)(q))
    = fmap(f1 ◦ f2)(p, q)
    */

  // Problem 2
  // type FToG[A] = F[A] => G[A]
  // // functor
  // type F[A] = Option[A]
  // // functor
  // type G[A] = (A, Int)

  // def fmapF[A, B](f: A => B): Option[A] => Option[B] = {
  //   case Some(a) => Some(f(a))
  //   case None => None
  // }

  // def fmapG[A, B](f: A => B): (A, Int) => (B, Int) = {
  //   case (a, i) => (f(a), i)
  // }

  // Cannot implement fmap
  // def fmapFToG[A, B](f: A => B): FToG[A] => FToG[B] = { fToGA =>
  //   val fToGB: FToG[B] = (fB: F[B]) => {
  //     val bToA: B => A = ??? // Need a function that maps B to A
  //     val fA: F[A] = fmapF[B, A](bToA)(fB) // or we need contraFmapF[B, A](f)(fB)
  //     val gA: G[A] = fToGA(fA)
  //     val gB: G[B] = fmapG(f)(gA)
  //     gB
  //   }
  //   fToGB
  // }

  // Cannot implement contraFmap
  // def contraFmapFToG[A, B](f: B => A): FToG[A] => FToG[B] = { fToGA =>
  //   val fToGB: FToG[B] = (fB: F[B]) => {
  //     val fA: F[A] = fmapF[B, A](f)(fB)
  //     val gA: G[A] = fToGA(fA)
  //     val aToB: A => B = a => ???  // There is no correct way to produce a value of type B
  //     val gB: G[B] = fmapG(aToB)(gA)
  //     gB
  //   }
  //   fToGB
  // }

  // Since we cannot implement fmap for FToG[A], it is not a functor
  // and since we cannot implement contrafmap, it is not a contrafunctor

  /**
    Problem 3
    Give functor F[A] and contrafunctor G[A], show that F[A] => G[A] is a contrafunctor
    Following functions exist:
    fmapF[A, B](f: A => B): F[A] => F[B]
    contrafmapG[A, B](f: B => A): G[A] => G[B]

    Define contrafmap as follows:
    contrafmap<F=>G>(f: B => A)(p: F[A] => G[A]) = (q: F[B]) => contrafmapG(f)(p(fmapF(f)(q)))

    Check identity
    contrafmap(id[A])(p: F[A] => G[A]) = (q: F[A]) => contrafmapG(f)(p(fmapF(f)(q)))
                                       = (q) => p(q) = p

    Check composition
    contrafmap(f1) ◦ contrafmap(f2)(p: F[A] => G[A])
    = contrafmap(f2)((qx) => contrafmapG(f1)(p(fmapF(f1)(qx))))
    = qy => contrafmapG(f2)(r(fmapF(f2)(qy)))

    where
    r = qx => contrafmapG(f1)(p(fmapF(f1)(qx)))
    r(fmapF(f2)(qy)) = contrafmapG(f1)(p(fmapF(f1)(fmapF(f2)(qy))))

    substituting this back in,
    qy => contrafmapG(f2)(contrafmapG(f1)(p(fmapF(f1)(fmapF(f2)(qy)))))

    fmapF(f1)(fmapF(f2)(qy)) = fmapF(f2) ◦ fmapF(f1)(qy)
                             = fmapF(f2 ◦ f1)(qy)
    contrafmapG(f2)(contrafmapG(f1)(x)) = contrafmapG(f1) ◦ contrafmapG(f2)(x)
                                        = contrafmapG(f2 ◦ f1)(x)

    putting these together:
    qy => contrafmapG(f2 ◦ f1)(p(fmapF(f2 ◦ f1)(qy)))
    = contrafmap(f2 ◦ f1)(p)
    */
}
