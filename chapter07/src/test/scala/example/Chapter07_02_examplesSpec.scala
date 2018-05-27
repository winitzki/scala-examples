package example

import cats.syntax.monoid._
import cats.{Functor, Monad, Monoid, Semigroup, derive}
import org.scalatest.FlatSpec
import io.chymyst.ch

class Chapter07_02_examplesSpec extends FlatSpec with FlattenableLawChecking with CatsLawChecking {

  behavior of "associativity for semimonads"

  it should "check associativity law for Option monad" in {
    // Check by writing out the code.
    def ftn[A]: Option[Option[A]] ⇒ Option[A] = {
      case None ⇒ None
      case Some(oa) ⇒ oa
    }

    // Functor instance.
    def fmap[A, B](f: A ⇒ B): Option[A] ⇒ Option[B] = {
      case None ⇒ None
      case Some(a) ⇒ Some(f(a))
    }

    // Compute fmap(ftn) symbolically.
    def fmapFtn[A]: Option[Option[Option[A]]] ⇒ Option[Option[A]] = {
      /*
      case None ⇒ None
      case Some(a) ⇒ Some(ftn(a))
       */
      case None ⇒ None
      case Some(x) ⇒ Some(x match {
        case None ⇒ None
        case Some(oa) ⇒ oa
      })
    }

    // Compute ftn[F[A]] ◦ ftn[A] symbolically.
    def ftnFtn[A]: Option[Option[Option[A]]] ⇒ Option[A] = {
      /*
      case None ⇒ ftn(None)
      case Some(x) ⇒ ftn(x)
       */
      case None ⇒ None
      case Some(x) ⇒ x match {
        case None ⇒ None
        case Some(oa) ⇒ oa
      }
    }

    // Compute fmap(ftn) ◦ ftn symbolically.
    def fmapFtnFtn[A]: Option[Option[Option[A]]] ⇒ Option[A] = {
      case None ⇒ None
      case Some(x) ⇒ x match {
        case None ⇒ None
        case Some(oa) ⇒ oa
      }
    }

    // We see that the code for fmap(ftn) ◦ ftn is exactly the same, up to variable names, as for ftn[F[A]] ◦ ftn[A].
  }

  it should "check associativity law for Either monad" in {
    // The Either monad `Z + A` allows us to check associativity with very little work: 
    // - Notice that the type signature of `fmap(ftn) ◦ ftn` and of `ftn[F[A]] ◦ ftn[A]` is `Z + (Z + (Z + A)) ⇒ Z + A`.
    // - Using the Curry-Howard correspondence, show that there is _only one_ implementation of this type signature,
    //    namely, each Z goes to Z and A goes to A.
    // - Therefore, the code for fmap(ftn) ◦ ftn has to be exactly the same as the code for ftn[F[A]] ◦ ftn[A].

    // Verify that the type signature has only one implementation.
    def ff[Z, A] = ch.anyOfType[Either[Z, Either[Z, Either[Z, A]]] ⇒ Either[Z, A]]()

    ff.length shouldEqual 1

    // Here is how we can check the associativity by hand.
    def ftn[Z, A]: Either[Z, Either[Z, A]] ⇒ Either[Z, A] = {
      case Left(z) ⇒ Left(z)
      case Right(eza) ⇒ eza
    }

    // Functor instance.
    def fmap[Z, A, B](f: A ⇒ B): Either[Z, A] ⇒ Either[Z, B] = {
      case Left(z) ⇒ Left(z)
      case Right(x) ⇒ Right(f(x))
    }

    // Compute fmap(ftn) symbolically.
    def fmapFtn[Z, A]: Either[Z, Either[Z, Either[Z, A]]] ⇒ Either[Z, Either[Z, A]] = {
      case Left(z) ⇒ Left(z)
      case Right(x) ⇒ Right(x match {
        case Left(z) ⇒ Left(z)
        case Right(eza) ⇒ eza
      })
    }

    // Compute ftn[F[A]] ◦ ftn[A] symbolically.
    def ftnFtn[Z, A]: Either[Z, Either[Z, Either[Z, A]]] ⇒ Either[Z, A] = {
      case Left(z) ⇒ Left(z)
      case Right(x) ⇒ x match {
        case Left(z) ⇒ Left(z)
        case Right(eza) ⇒ eza
      }
    }

    // Compute fmap(ftn) ◦ ftn symbolically.
    def fmapFtnFtn[Z, A]: Either[Z, Either[Z, Either[Z, A]]] ⇒ Either[Z, A] = {
      case Left(z) ⇒ Left(z)
      case Right(x) ⇒ x match {
        case Left(z) ⇒ Left(z)
        case Right(eza) ⇒ eza
      }
    }

    // We see that the code for fmap(ftn) ◦ ftn is exactly the same, up to variable names, as for ftn[F[A]] ◦ ftn[A].
  }

  it should "check associativity for List monad" in {
    /*
    flatten: [ [x1, x2, ...], [y1, y2, ...], ...] ⇒ [x1, x2, ..., y1, y2, ...]
    fmap(flatten): [ [ [x11, x12, ...], [y11, y12, ...], ...], [ [x21, x22, ...], [y21, y22, ...], ...], ...] ⇒
      [ [x11, x12, ..., y11, y12, ...], [x21, x22, ..., y21, y22, ...], ...]  -- flattens the inner lists
    flatten[List[A]]: [ [ [x11, x12, ...], [y11, y12, ...], ...], [ [x21, x22, ...], [y21, y22, ...], ...], ...] ⇒
      [ [x11, x12, ...], [y11, y12, ...], ..., [x21, x22, ...], [y21, y22, ...], ...]  -- flattens the outer lists
    Applying `flatten` to these lists will yield the same result -- all lists flattened at all 3 levels:
    [ x11, x12, ..., y11, y12, ..., ..., x21, x22, ..., y21, y22, ..., ...]
     */

    val s = List(1, 2).map(x ⇒ List(10 + x, 20 + x).map(x ⇒ List(1 + x, 2 + x)))
    s shouldEqual List(List(List(12, 13), List(22, 23)), List(List(13, 14), List(23, 24)))

    s.flatten shouldEqual List(List(12, 13), List(22, 23), List(13, 14), List(23, 24))
    s.flatten.flatten shouldEqual List(12, 13, 22, 23, 13, 14, 23, 24)

    val sFmapFlatten = s.map(_.flatten)

    sFmapFlatten shouldEqual List(List(12, 13, 22, 23), List(13, 14, 23, 24))
    sFmapFlatten.flatten shouldEqual List(12, 13, 22, 23, 13, 14, 23, 24)
  }

  it should "check associativity for Writer monad" in {
    def ftn[W: Semigroup, A]: (((A, W), W)) ⇒ (A, W) = {
      case ((x, w1), w2) ⇒ (x, w1 |+| w2)
    }

    def fmap[W, A, B](f: A ⇒ B): ((A, W)) ⇒ (B, W) = {
      case (x, w) ⇒ (f(x), w)
    }

    // Compute ftn[F[A]] ◦ ftn symbolically.

    def ftnFtn[W: Semigroup, A]: ((((A, W), W), W)) ⇒ (A, W) = {
      case (((x, w1), w2), w3) ⇒ // ftn of ((x, w1), w2 |+| w3)
        (x, w1 |+| (w2 |+| w3))
    }

    // Compute fmap(ftn) symbolically.

    def fmapFtn[W: Semigroup, A]: ((((A, W), W), W)) ⇒ ((A, W), W) = {
      case (((x, w1), w2), w3) ⇒ ((x, w1 |+| w2), w3)
    }

    // Compute fmap(ftn) ◦ ftn symbolically.

    def fmapFtnFtn[W: Semigroup, A]: ((((A, W), W), W)) ⇒ (A, W) = {
      case (((x, w1), w2), w3) ⇒ // ftn of ((x, w1 |+| w2), w3)
        (x, (w1 |+| w2) |+| w3)
    }

    // We see that the code for fmap(ftn) ◦ ftn is exactly the same as for ftn[F[A]] ◦ ftn[A], if |+| is associative.
  }

  it should "check associativity for Reader monad" in {
    // The Reader monad R ⇒ A allows us to check associativity with very little work: 
    // - Notice that the type signature of `fmap(ftn) ◦ ftn` and of `ftn[F[A]] ◦ ftn[A]` is `(R ⇒ (R ⇒ (R ⇒ A))) ⇒ R ⇒ A`.
    // - Using the Curry-Howard correspondence, show that there is _only one_ implementation of this type signature,
    //    namely `{ rrra ⇒ r ⇒ rrra(r)(r)(r) }`.
    // - Therefore, the code for fmap(ftn) ◦ ftn has to be exactly the same as the code for ftn[F[A]] ◦ ftn[A].

    // Verify that the type signatures have only one implementation.

    def ff[R, A] = ch.anyOfType[(R ⇒ (R ⇒ A)) ⇒ R ⇒ A]()

    ff.length shouldEqual 1

    def fff[R, A] = ch.anyOfType[(R ⇒ (R ⇒ (R ⇒ A))) ⇒ R ⇒ A]()

    fff.length shouldEqual 1

    // Here is how we can check the associativity by hand.
    def ftn[R, A](rra: R ⇒ (R ⇒ A)): R ⇒ A = { r ⇒ rra(r)(r) }

    def fmap[R, A, B](f: A ⇒ B)(ra: R ⇒ A): R ⇒ B = { r ⇒ f(ra(r)) }

    // Compute ftn[F[A]] ◦ ftn symbolically.
    def ftnFtn[R, A](rrra: R ⇒ (R ⇒ (R ⇒ A))): R ⇒ A = {
      // ftn(r ⇒ rrra(r)(r))
      // r ⇒ (r ⇒ rrra(r)(r))(r)(r)
      r ⇒ rrra(r)(r)(r)
    }

    // Compute fmap(ftn) symbolically.
    def fmapFtn[R, A](rrra: R ⇒ (R ⇒ (R ⇒ A))): R ⇒ (R ⇒ A) = {
      // r1 ⇒ ftn(rrra(r1)) // definition of fmap(rrra)
      // r1 ⇒ (r ⇒ rrra(r1) (r) (r)) // definition of ftn
      r1 ⇒ r ⇒ rrra(r1)(r)(r)
    }

    // Compute fmap(ftn) ◦ ftn symbolically.
    def fmapFtnFtn[R, A](rrra: R ⇒ (R ⇒ (R ⇒ A))): R ⇒ A = {
      // ftn (r1 ⇒ r ⇒ rrra(r1)(r)(r)))
      // r2 ⇒ (r1 ⇒ r ⇒ rrra(r1)(r)(r)) (r2) (r2) 
      // r2 ⇒ rrra(r2)(r2)(r2)
      r ⇒ rrra(r)(r)(r)
    }

    // We see that the code for fmap(ftn) ◦ ftn is exactly the same as for ftn[F[A]] ◦ ftn[A].
  }

  it should "check associativity for State monad" in {
    // There are several implementations of the type signature for flatten, so we have to verify the law directly.

    def ftn[S, A](ssass: S ⇒ (S ⇒ (A, S), S)): S ⇒ (A, S) = { s ⇒
      val (sas, s1) = ssass(s)
      sas(s1)
    }

    def fmap[S, A, B](f: A ⇒ B)(sas: S ⇒ (A, S)): S ⇒ (B, S) = { s ⇒
      val (a, s1) = sas(s)
      (f(a), s1)
    }

    // Compute ftn[F[A]] ◦ ftn symbolically.
    def ftnFtn[S, A](sssasss: S ⇒ (S ⇒ (S ⇒ (A, S), S), S)): S ⇒ (A, S) = {
      /* ftn {
           s ⇒
            val (ssass, s1) = sssasss(s)
            ssass(s1)
          }
     */
      /* Substitute the definition of ftn:
       s ⇒
         val (sas, s2) = {
           val (ssass, s1) = sssasss(s)
           ssass(s1)
         }
         sas(s2)
      */
      // Simplify code:
      s ⇒
        val (ssass, s1) = sssasss(s)
        val (sas, s2) = ssass(s1)
        sas(s2)
    }

    // Compute fmap(ftn) symbolically.
    def fmapFtn[S, A](sssasss: S ⇒ (S ⇒ (S ⇒ (A, S), S), S)): S ⇒ (S ⇒ (A, S), S) = {
      /*
      fmap { ssass ⇒ s ⇒
        val (sas, s1) = ssass(s)
        sas(s1)
      }
      */
      s ⇒
        val (ssass, s1) = sssasss(s)
        ( { s3 ⇒
          val (sas, s2) = ssass(s3)
          sas(s2)
        }, s1)
    }

    // Compute fmap(ftn) ◦ ftn symbolically.
    def fmapFtnFtn[S, A](sssasss: S ⇒ (S ⇒ (S ⇒ (A, S), S), S)): S ⇒ (A, S) = {
      /*      s ⇒
              val (sas, s0) = fmapFtn(sssasss)(s)
             sas(s0)
      */
      /* Substitute fmapFtn(sssasss)(s):
      s ⇒
        val (sas, s0) = {
          val (ssass, s1) = sssasss(s)
          ( { s3: S ⇒
            val (sas, s2) = ssass(s3)
            sas(s2)
          }, s1)
        }
        sas(s0)
      */
      // Simplify code.
      /*
      s ⇒
        val (ssass, s1) = sssasss(s)
        val sas = { s3: S ⇒
            val (sas, s2) = ssass(s3)
            sas(s2)
        }
        sas(s1)
      */
      // Substitute s1 into sas(s1), i.e. replace s3 by s1.
      s ⇒
        val (ssass, s1) = sssasss(s)
        val (sas, s2) = ssass(s1)
        sas(s2)
    }

    // After identical transformations, the code for fmap(ftn) ◦ ftn is exactly the same as for ftn[F[A]] ◦ ftn[A].
  }

  it should "check associativity for Continuation monad" in {
    // Associativity for Continuation monad cannot be derived from the Curry-Howard correspondence:
    // There are 10 implementations of `ftn`'s type signature, and 56 implementations of `ftn ◦ ftn`'s type signature.

    // Verify that these type signatures have many implementations.
    type Cont[R, A] = (A ⇒ R) ⇒ R

    def ff[R, A] = ch.anyOfType[Cont[R, Cont[R, A]] ⇒ Cont[R, A]]()

    ff.length shouldEqual 10

    def fff[R, A] = ch.anyOfType[Cont[R, Cont[R, Cont[R, A]]] ⇒ Cont[R, A]]()

    fff.length shouldEqual 56

    // Verify associativity directly.
    // ((((A ⇒ R) ⇒ R) ⇒ R) ⇒ R) ⇒ (A ⇒ R) ⇒ R
    def ftn[R, A](cca: Cont[R, Cont[R, A]]): Cont[R, A] = { ar ⇒ cca(ca ⇒ ca(ar)) }

    // ((A ⇒ R) ⇒ R) ⇒ (B ⇒ R) ⇒ R
    def fmap[R, A, B](f: A ⇒ B)(ca: Cont[R, A]): Cont[R, B] = { br ⇒ ca(a ⇒ br(f(a))) }

    // Compute ftn[F[A]] ◦ ftn symbolically.
    def ftnFtn[R, A](ccca: Cont[R, Cont[R, Cont[R, A]]]): Cont[R, A] = {
      // ftn ( ar2 ⇒ ccca(ca2 ⇒ ca2(ar2)) )
      // ar ⇒ ( ar2 ⇒ ccca(ca2 ⇒ ca2(ar2)) ) (ca ⇒ ca(ar))
      // Substitute ar2 = ( ca ⇒ ca(ar) ).
      ar ⇒ ccca(ca2 ⇒ ca2(ca ⇒ ca(ar)))
    }

    // Compute fmap(ftn) symbolically.
    def fmapFtn[R, A](ccca: Cont[R, Cont[R, Cont[R, A]]]): Cont[R, Cont[R, A]] = {
      // br ⇒ ccca(a ⇒ br(ftn(a)))
      // Substitute ftn(a):
      br ⇒ ccca(a ⇒ br(ar ⇒ a(ca ⇒ ca(ar))))
    }

    // Compute fmap(ftn) ◦ ftn symbolically.
    def fmapFtnFtn[R, A](ccca: Cont[R, Cont[R, Cont[R, A]]]): Cont[R, A] = {
      // ftn(fmapFtn(ccca)) = ftn ( br ⇒ ccca(a ⇒ br(ar ⇒ a(ca ⇒ ca(ar)))) )
      // Substitute `ftn(x) = { ar1 ⇒ x(ca1 ⇒ ca1(ar1)) }`:
      // ar1 ⇒ x(ca1 ⇒ ca1(ar1))   where x = { br ⇒ ccca(a ⇒ br(ar ⇒ a(ca ⇒ ca(ar)))) }
      // Substitute br = { ca1 ⇒ ca1(ar1) } into x's body:
      // ar1 ⇒ ccca(a ⇒ br (ar ⇒ a(ca ⇒ ca(ar)))) where br = { ca1 ⇒ ca1(ar1) }
      // Simplify by substituting ar = ar1, get:
      // br(ar ⇒ a(ca ⇒ ca(ar))) = a(ca => ca(ar1))
      // So the final expression is 
      ar1 ⇒ ccca(a ⇒ a(ca ⇒ ca(ar1)))
    }
    
    // After renaming a to ca2, and ar1 to ar, we get exactly the same code as for ftn[F[A]] ◦ ftn.
  }

  it should "verify that associativity fails for incorrect implementation of flatten" in {
    // The functor F[A] = (A, W, W) is equivalent to Writer with (W, W) as the semigroup.
    // Except that (W, W) is not a semigroup when the binary operation is defined by `combine( (w1, w2), (w3, w4) ) = (w4, w3)`.
    // It is sufficient to verify that associativity fails for `combine`.

    def combine[W](p1: (W, W), p2: (W, W)): (W, W) = (p2._2, p2._1)

    combine((1, 2), (3, 4)) shouldEqual ((4, 3))

    // Associativity fails.
    combine((1, 2), combine((3, 4), (5, 6))) shouldEqual ((5, 6))
    combine(combine((1, 2), (3, 4)), (5, 6)) shouldEqual ((6, 5))
    combine((1, 2), combine((3, 4), (5, 6))) should not equal combine(combine((1, 2), (3, 4)), (5, 6))
  }

  it should "verify that associativity fails for reversing flatten for List" in {
    val s = List(1, 2).map(x ⇒ List(10 + x, 20 + x).map(x ⇒ List(1 + x, 2 + x)))
    s shouldEqual List(List(List(12, 13), List(22, 23)), List(List(13, 14), List(23, 24)))

    // Define flatten as `reverse andThen flatten`.
    s.reverse.flatten.reverse.flatten shouldEqual List(22, 23, 12, 13, 23, 24, 13, 14)

    s.map(_.reverse.flatten).reverse.flatten shouldEqual List(23, 24, 13, 14, 22, 23, 12, 13)

    s.reverse.flatten.reverse.flatten should not equal s.map(_.reverse.flatten).reverse.flatten
  }

  behavior of "misc. examples"

  it should "check semigroup laws for a non-monoid type" in {

    type TripleM[T] = (T, T, T)

    // If P is a monoid type then we can define P × P × P as a semigroup in a special way.
    implicit def tripleMonoid[P: Monoid]: Semigroup[TripleM[P]] = { (x: TripleM[P], y: TripleM[P]) ⇒ (x._1, x._2 combine y._2, y._3) }

    // Use the Int monoid as an example.

    implicit val monoidInt: Monoid[Int] = new Monoid[Int] {
      override def empty: Int = 0

      override def combine(x: Int, y: Int): Int = x + y
    }

    checkCatsSemigroupLaws[TripleM[Int]]()
  }

  it should "check associativity for a semimonad" in {
    type F[A] = Either[A, (A, A)]

    implicit val functorF: Functor[F] = derive.functor[F]
    //    implicit val functorF: Functor[F] = new Functor[F] {
    //      override def map[A, B](fa: F[A])(f: A => B): F[B] = implement
    //    }
    implicit val flattenableF: Flattenable[F] = new Flattenable[F] {

      private def extractLeft[A](fa: F[A]): A = fa match {
        case Left(x) ⇒ x
        case Right((x, _)) ⇒ x
      }

      private def extractRight[A](fa: F[A]): A = fa match {
        case Left(x) ⇒ x
        case Right((_, x)) ⇒ x
      }

      override def flatten[A](ffa: F[F[A]]): F[A] = extractLeft(ffa)

      //      override def flatten[A](ffa: F[F[A]]): F[A] = ffa match {
      //        case Left(fa) ⇒ fa
      //        case Right((fa1, fa2)) ⇒ Right(extractLeft(fa1), extractRight(fa2))
      //      }
    }

    checkFlattenLaws[F, Int, String]()
  }
}
