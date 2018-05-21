package example

import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.monoid._
import cats.{Functor, Monad, Monoid}
import example.CatsMonad.toCatsMonad
import org.scalatest.FlatSpec

class Chapter07_02_semimonadsSpec extends FlatSpec with FlattenableLawChecking with CatsLawChecking {

  behavior of "constructions of monads"

  it should "verify construction 1: constant functor" in {
    type C[Z, A] = Z

    // The functor is going to be C[Z, ?], for a fixed Z.

    implicit def functorC[Z]: Functor[C[Z, ?]] = new Functor[C[Z, ?]] {
      override def map[A, B](fa: C[Z, A])(f: A ⇒ B): C[Z, B] = fa
    }

    implicit def semimonadC[Z]: Semimonad[C[Z, ?]] = new Semimonad[C[Z, ?]] {
      override def flatMap[A, B](fa: C[Z, A])(f: A ⇒ C[Z, B]): C[Z, B] = fa
    }

    // Flatten is id. So, associativity is trivial.

    // Why is this not a full monad? Because the right identity law fails unless Z = 1.
    // We must have flatten (pure (x: C[Z, A])) = x. But pure: A ⇒ Z must give a fixed value of Z.
    // So pure(x) cannot depend on x, and we cannot restore x if we only know pure(x).
    // Therefore, flatten(pure(x)) cannot compute x.
    // However, if Z = 1 there is only one value of x, so there is no problem and pure(x) returns 1.
  }

  it should "verify semimonad construction 2" in {
    // The functor F[A] is defined as (A, G[A]).
    // In the syntax of the "kind projector", it is Lambda[X ⇒ (X, G[X])].

    implicit def functorAG[G[_] : Functor]: Functor[Lambda[X ⇒ (X, G[X])]] = new Functor[Lambda[X ⇒ (X, G[X])]] {
      override def map[A, B](fa: (A, G[A]))(f: A ⇒ B): (B, G[B]) = fa match {
        case (a, ga) ⇒ (f(a), ga.map(f))
      }
    }

    implicit def semimonadAG[G[_] : Functor]: Semimonad[Lambda[X ⇒ (X, G[X])]] = new Semimonad[Lambda[X ⇒ (X, G[X])]] {
      override def flatMap[A, B](fa: (A, G[A]))(f: A ⇒ (B, G[B])): (B, G[B]) = {
        // The first effect is in `fa`, the second effect is in the result of applying `f`.
        // We discard the second effect and extract just the value of `B` from that.
        val f1: A ⇒ B = f andThen (_._1)
        val (a, ga) = fa
        (f1(a), ga.map(f1))
      }
    }

    // If G = 1, we have F[A] = A; this is the Identity monad. flatmap = id, fmap = id, flatten = id, pure = id.
    // All monad laws hold trivially.

    // Now verify the associativity law for an arbitrary functor G.

    // Define type aliases for brevity.
    type F[G[_], A] = (A, G[A])
    type FF[G[_], A] = (F[G, A], G[F[G, A]])
    type FFF[G[_], A] = (FF[G, A], G[FF[G, A]])

    // Derive flatten from flatMap as flatten(ffa) = ffa.flatMap(id), where id is of type F[F[A]] ⇒ F[F[A]].
    def ftn[G[_] : Functor, A](ffa: FF[G, A]): (A, G[A]) = {
      /*
        val f1 = identity andThen (_._1) = _._1
        val (a, ga) = ffa
        (f1(a), ga.map(f1))
       */
      /* Substitute the definition of f1:
      val (fa, gfa) = ffa
      (fa._1, gfa.map(_._1))
      */
      // Simplify.
      (ffa._1._1, ffa._2.map(_._1))

    }

    // Implement fmap.
    def fmap[G[_] : Functor, A, B](f: A ⇒ B): ((A, G[A])) ⇒ (B, G[B]) = {
      case (a, ga) ⇒ (f(a), ga.map(f))
    }


    // Compute fmap(ftn) symbolically.
    def fmapFtn[G[_] : Functor, A](fffa: FFF[G, A]): FF[G, A] = {
      /*
      fffa match {
        case (ffa, gffa) ⇒ // fmap(ftn) (fffa)
          // (ftn[G, A](ffa), gffa.map(ftn[G, A]))
          ((ffa._1._1, ffa._2.map(_._1)), gffa.map(ftn[G, A]))
      }
      */
      // Simplify:
      ((fffa._1._1._1, fffa._1._2.map(_._1)), fffa._2.map(ftn[G, A]))
    }

    // Compute  fmap(ftn) ◦ ftn, that is, ftn(fmap(ftn)(fffa)), symbolically.
    def fmapFtnFtn[G[_] : Functor, A](fffa: FFF[G, A]): F[G, A] = {
      // ftn {
      //      ((fffa._1._1._1, fffa._1._2.map(_._1)), fffa._2.map(ftn[G, A]))
      //    }
      // Substitute the definition of ftn and simplify:
      //      (fffa._1._1._1, fffa._2.map(ftn[G, A]).map(_._1))
      // Combine the two `map()` calls and simplify:
      //      (fffa._1._1._1, fffa._2.map(ffa ⇒ ftn[G, A](ffa)._1))
      // Note that ftn(x)._1 = x._1._1 and simplify:
      (fffa._1._1._1, fffa._2.map(ffa ⇒ ffa._1._1))
    }

    // Compute ftn[F[A]] ◦ ftn[A], that is, ftn(ftn(fffa)) symbolically.
    def ftnFtn[G[_] : Functor, A](fffa: FFF[G, A]): F[G, A] = {
      // ftn {
      //      (fffa._1._1, fffa._2.map(_._1))
      // }
      /* Substitute the definition of ftn.
      (
        { (fffa._1._1, fffa._2.map(_._1)) }._1._1, 
        { (fffa._1._1, fffa._2.map(_._1)) }._2.map(_._1)
      */
      // Simplify:
      //      (fffa._1._1._1, fffa._2.map(_._1).map(_._1))
      // Simplify:
      (fffa._1._1._1, fffa._2.map(_._1._1))
    }

    // Observe now that ftn[F[A]] ◦ ftn[A] is the same expression as fmap(ftn) ◦ ftn.

    // Why is F not a monad? Because we can't have pure: A => (A, G[A]) for arbitrary functors G.
    // When G is itself a monad, we obtain a particular case of construction 3, see next example.
  }

  it should "verify monad construction 3" in {
    type F[G[_], H[_], A] = (G[A], H[A])
    type FF[G[_], H[_], A] = (G[(G[A], H[A])], H[(G[A], H[A])])
    type FFF[G[_], H[_], A] = (G[FF[G, H, A]], H[FF[G, H, A]])

    // We use `map(_._1)` and `map(_._2)` to discard H[A] inside G[(G[A], H[A])] and to discard G[A] inside H[(G[A], H[A])].
    def ftn[G[_] : Monad, H[_] : Monad, A](ffa: FF[G, H, A]): (G[A], H[A]) = {
      //      (ffa._1.map(_._1).flatten, ffa._2.map(_._2).flatten)
      // Simplify by substituting `flatMap`s already defined in the monads G and H:
      (ffa._1.flatMap(_._1), ffa._2.flatMap(_._2))
    }

    def pure[G[_] : Monad, H[_] : Monad, A](a: A): (G[A], H[A]) = (Monad[G].pure(a), Monad[H].pure(a))

    // The definition of `fmap` is standard for the product of two functors.
    def fmap[G[_] : Functor, H[_] : Functor, A, B](f: A ⇒ B)(fa: F[G, H, A]): F[G, H, B] = (fa._1.map(f), fa._2.map(f))

    // Verify the identity laws.

    // pure ◦ ftn = id
    def pureFtn[G[_] : Monad, H[_] : Monad, A](fa: F[G, H, A]): F[G, H, A] = {
      // ftn(pure(fa)) = ftn { (Monad[G].pure(fa), Monad[H].pure(fa)) }
      // Substitute the definition of ftn:
      //      (Monad[G].pure(fa).flatMap(_._1), Monad[H].pure(fa).flatMap(_._2))
      // Use the identity laws for monads G and H formulated in terms of flatMap:
      // Monad[G].pure(x).flatMap(f) = f(x), and similarly for Monad[H].
      (fa._1, fa._2)
      // This is identical to just `fa`.
    }

    // fmap(pure) ◦ ftn = id
    def fmapPureFtn[G[_] : Monad, H[_] : Monad, A](ffa: FF[G, H, A]): FF[G, H, A] = {
      // Compute ftn(fmap(pure)(ffa)).
      // ftn { (ffa._1.map(pure), ffa._2.map(pure) } 
      // = (ffa._1.map(pure).flatMap(_._1), ffa._2.map(pure).flatMap(_._2))
      // Use the naturality law to simplify `.map(f).flatMap(g) = .flatMap(f andThen g)`. 
      // Substitute the definition of `pure` and observe that e.g. `pure andThen (_._1)` is Monad[G].pure:
      // = (ffa._1.flatMap(Monad[G].pure), ffa._2.flatMap(Monad[H].pure))
      // Now use the identity law for monads G and H, e.g.
      // ga.flatMap(Monad[G].pure) = ga, similarly for H.
      (ffa._1, ffa._2)
      // This is identical to just `ffa`.
    }

    // Verify the associativity law for `ftn`.

    // Compute ftn[F[A]] ◦ ftn[A] symbolically.
    def ftnFtn[G[_] : Monad, H[_] : Monad, A](fffa: FFF[G, H, A]): F[G, H, A] = {
      // ftn { (fffa._1.flatMap(_._1), fffa._2.flatMap(_._2)) } =
      (fffa._1.flatMap(_._1).flatMap(_._1), fffa._2.flatMap(_._2).flatMap(_._2))
    }

    // Compute fmap(ftn) ◦ ftn symbolically.
    def fmapFtnFtn[G[_] : Monad, H[_] : Monad, A](fffa: FFF[G, H, A]): F[G, H, A] = {
      // ftn(fmap(ftn)(fffa))
      // = ftn { (fffa._1.map(ftn), fffa._2.map(ftn)) }
      // Substitute the definition of outer `ftn`:
      //       (fffa._1.map(ftn[G, H, A]).flatMap(_._1), fffa._2.map(ftn[G, H, A]).flatMap(_._2))
      // Use naturality to exchange `.map(x).flatMap(y) = .flatMap(x andThen y)`.
      //       (fffa._1.flatMap(ftn andThen (_._1)), fffa._2.flatMap(ftn andThen (_._2)))
      // Simplify `ftn andThen _._1` to `_._1.flatMap(_._1)`.
      //      (fffa._1.flatMap(_._1.flatMap(_._1)), fffa._2.flatMap(_._2.flatMap(_._2)))
      // Use the associativity law for the monads G and H to simplify `.flatMap(x andThen flatMap(y))` to `.flatMap(x).flatMap(y)`.
      (fffa._1.flatMap(_._1).flatMap(_._1), fffa._2.flatMap(_._2).flatMap(_._2))
    }

    // Observe now that ftn[F[A]] ◦ ftn[A] is the same expression as fmap(ftn) ◦ ftn.
  }

  it should "verify monad construction 5" in {
    type F[G[_], A] = Either[A, G[A]]
    type FF[G[_], A] = F[G, F[G, A]]
    type FFF[G[_], A] = F[G, FF[G, A]]

    // Auxiliary function that encapsulates an important computation:
    def merge[G[_] : Monad, A]: F[G, A] ⇒ G[A] = {
      case Left(a) ⇒ Monad[G].pure(a)
      case Right(ga) ⇒ ga
    }

    // The definition of flatten uses G's `pure` and `flatten`.
    def ftn[G[_] : Monad, A](ffa: FF[G, A]): F[G, A] = ffa match {
      // ffa has type (A + G[A]) + G[A + G[A]], and we need to return A + G[A].
      case Left(fa) ⇒ fa
      // We will use `merge`: (A + G[A]) ⇒ G[A] (this function is defined above). 
      // First, use `.map(merge)` to transform G[A + G[A]] into G[G[A]].
      // This gives us a G[G[A]]. Then we use G's `flatten` on that and obtain G[A].
      // Finally, we put that G[A] into the `Right` of A + G[A].
      case Right(gfa) ⇒ // Right(gfa.map(merge).flatten)
        // Use G's flatMap for brevity:
        Right(gfa.flatMap(merge))
    }

    // The definition of `pure` does not use any properties of `G`, but the laws won't hold unless G is a monad.
    def pure[G[_], A](a: A): F[G, A] = Left(a)

    // The definition of `fmap` is standard for the disjunction of two functors.
    def fmap[G[_] : Functor, A, B](f: A ⇒ B)(fa: F[G, A]): F[G, B] = fa match {
      case Left(a) ⇒ Left(f(a))
      case Right(ga) ⇒ Right(ga.map(f))
    }

    // Verify the identity laws.
    // pure ◦ ftn = id
    def pureFtn[G[_] : Monad, A](fa: F[G, A]): F[G, A] = {
      // ftn(pure(fa)) = ftn { Left(fa) }
      // Substitute the definition of ftn:
      fa
      // This is the identity function as required.
    }

    // fmap(pure) ◦ ftn = id
    def fmapPureFtn[G[_] : Monad, A](ffa: FF[G, A]): FF[G, A] = {
      // Compute ftn(fmap(pure)(ffa)).
      // ftn { ffa match {
      //        case Left(fa) ⇒ Left(pure(fa))
      //        case Right(gfa) ⇒ Right(gfa.map(pure))
      //     }
      // } 
      /* Substitute the definition of `ftn`:
      ffa match {
        case Left(fa) ⇒ pure(fa)
        case Right(gfa) ⇒ Right(gfa.map(pure).flatMap(merge))
      }
      */
      // Use the naturality law to simplify `.map(f).flatMap(g) = .flatMap(f andThen g)` where f = pure. 
      /* Substitute the definition of `pure` and observe that `pure andThen merge` is the same as Monad[G].pure:
      ffa match {
        case Left(fa) ⇒ Left(fa)
        case Right(gfa) ⇒ Right(gfa.flatMap(Monad[G].pure))
      }
      */
      // Now use the identity law for the monad G, that is, ga.flatMap(Monad[G].pure) = ga.
      ffa match {
        case Left(fa) ⇒ Left(fa)
        case Right(gfa) ⇒ Right(gfa)
      }
      // This is identical to just `ffa`.
    }

    // Verify the associativity law for `ftn`.

    // Compute ftn[F[A]] ◦ ftn[A] symbolically.
    def ftnFtn[G[_] : Monad, A](fffa: FFF[G, A]): F[G, A] = {
      // ftn { fffa match {
      //      case Left(ffa) ⇒ ffa
      //      case Right(gffa) ⇒ Right(gffa.flatMap(merge))
      //    }
      // }
      // Substitute the definition of ftn and simplify:
      fffa match {
        case Left(ffa) ⇒ ftn[G, A](ffa)
        case Right(gffa) ⇒ Right(gffa.flatMap(merge).flatMap(merge))
      }
    }

    // Compute fmap(ftn) ◦ ftn symbolically.
    def fmapFtnFtn[G[_] : Monad, A](fffa: FFF[G, A]): F[G, A] = {
      /*      ftn(fmap(ftn)(fffa)) = 
      ftn {
        fffa match {
          case Left(ffa) ⇒ Left(ftn(ffa))
          case Right(gffa) ⇒ Right(gffa.map(ftn))
        }
      }
      */
      // Substitute the definition of outer ftn and simplify:
      fffa match {
        case Left(ffa) ⇒ ftn[G, A](ffa)
        case Right(gffa) ⇒ Right(gffa.map(ftn[G, A]).flatMap(merge))
      }

      /* The `Left` case is already the same as in ftn[F[A]] ◦ ftn[A]. It remains to prove the `Right` case.
      We need to show that the following two expressions are the same,
      
      gffa.flatMap(merge).flatMap(merge)
      
      and
      
      gffa.map(ftn[G, A]).flatMap(merge)
      
      In the last expression, the `map` and `flatMap` are from G, so we can use naturality:
        .map(x).flatMap(merge) = .flatMap(x andThen merge)
      This gives
      
      gffa.flatMap { ftn[G, A] andThen merge }
      
      Substitute the definition of ftn:
      
      gffa.flatMap { ffa ⇒ ffa match {
              case Left(fa) ⇒ merge(fa)
              case Right(gfa) ⇒ merge(Right(gfa.flatMap(merge)))
           }
      }
      
      Substitute the definition of merge:
      
      gffa.flatMap { ffa ⇒ ffa match {
              case Left(fa) ⇒ merge(fa)
              case Right(gfa) ⇒ gfa.flatMap(merge))
           }
      }
      
      We now need to compare gffa.flatMap(...) with gffa.flatMap(merge).flatMap(merge).
      
      To make this comparison more direct, let us combine the two flatMap's by using G's associativity law:
        .flatMap(f).flatMap(y) = .flatMap(x ⇒ f(x).flatMap(y)).

      This gives, instead of gffa.flatMap(merge).flatMap(merge),
      
      gffa.flatMap { ffa ⇒ merge(ffa).flatMap(merge) }.
      
      It remains to compare two functions inside `flatMap`:
      
      The first function is
      
      ffa ⇒ ffa match {
              case Left(fa) ⇒ merge(fa)
              case Right(gfa) ⇒ gfa.flatMap(merge))
           }
      
      The second function is
      
      ffa ⇒ merge(ffa).flatMap(merge).
      
      Substitute the definition of merge:
      
      ffa ⇒ ffa match {
        case Left(fa) ⇒ Monad[G].pure(fa).flatMap(merge)
        case Right(gfa) ⇒ gfa.flatMap(merge)
      }
      
      The `Right` cases are identical. The `Left` cases become identical once we use G's identity law,
      Monad[G].pure(x).flatMap(f) = f(x).
      
      Q.E.D.
     */
    }
  }

  it should "verify monad construction 6" in {
    // Cut down the number of type parameters to be written each time.
    def construction6[G[_] : Monad, W: Monoid, Z](): Unit = {

      type P[A] = Either[Z, (W, A)]

      type F[A] = G[P[A]]

      // P[A] is a monad; exercise 8 is to show that its laws hold.

      def fmapP[A, B](f: A ⇒ B)(fa: P[A]): P[B] = fa match {
        case Left(z) ⇒ Left(z)
        case Right((w, a)) ⇒ Right((w, f(a)))
      }

      implicit val catsMonad: CatsMonad[P] = new CatsMonad[P] {
        override def flatMap[A, B](fa: P[A])(f: A ⇒ P[B]): P[B] = fa match {
          case Left(z1) ⇒ Left(z1)
          case Right((w1, a1)) ⇒ f(a1) match {
            case Left(z2) ⇒ Left(z2)
            case Right((w2, a2)) ⇒ Right((w1 |+| w2, a2))
          }
        }

        override def pure[A](a: A): P[A] = Right((Monoid[W].empty, a))
      }

      // We need this code explicitly. Convert Z + W × (Z + W × A) ⇒ Z + W × A.
      def ftnP[A](ppa: P[P[A]]): P[A] = ppa match {
        case Left(z1) ⇒ Left(z1)
        case Right((w1, pa)) ⇒ pa match {
          case Left(z2) ⇒ Left(z2)
          case Right((w2, a)) ⇒ Right((w1 |+| w2, a))
        }
      }

      def fmapG[A, B](f: A ⇒ B)(fa: G[A]): G[B] = Functor[G].fmap(fa)(f)

      def flmG[A, B](f: A ⇒ G[B])(fa: G[A]): G[B] = Monad[G].flatMap(fa)(f)

      def fmapF[A, B](f: A ⇒ B)(fa: F[A]): F[B] = Functor[G].map(fa)(pa ⇒ Functor[P].map(pa)(f))
      // Short notation: fmapF(f) = fmapG(fmapP(f)), or `fmapF = fmapP ◦ fmapG`

      implicit val functorF: Functor[F] = new Functor[F] {
        // Use the Functor instances for G and P, to define the standard Functor instance for the composition of functors.
        override def map[A, B](fa: F[A])(f: A ⇒ B): F[B] = fmapF(f)(fa)
      }

      def pure[A](a: A): F[A] = Monad[G].pure(Right((Monoid[W].empty, a)))

      // flatten for F transforms `G[P[G[P[A]]]]` first into `G[G[P[P[A]]]]` by 
      // using the "sequencing" function `seq: P[G[C]] ⇒ G[P[C]]`, which
      // we are able to define using some internal details of this _specific_ functor P:

      def seq[C](pgc: P[G[C]]): G[P[C]] = pgc match {
        case Left(z) ⇒ Monad[G].pure(Left(z))
        case Right((w, gc)) ⇒ gc.map(c ⇒ Right((w, c)))
      }
      // Since the code of `seq` only uses natural transformations and is fully generic,
      // `seq` is also a natural transformation -- between functors P[G[?]] and G[P[?]].
      // So `seq` automatically satisfies the naturality law:
      // `fmapP (fmapG f) ◦ seq = seq ◦ fmapG (fmapP f)`

      // Define `flatten` for F.
      def ftn[A](ffa: F[F[A]]): F[A] = Monad[G].flatMap(ffa) {
        // This inner function works like this:
        // P[G[P[A]]] ... `seq` ...> G[P[P[A]]] ... fmapG(ftnP) ...> G[P[A]]  
        // The type of this inner function is P[G[P[A]]] ⇒ G[P[A]],
        // so G.flatMap of it will be G[P[G[P[A]]]] ⇒ G[P[A]].
        seq[P[A]] _ andThen fmapG(ftnP)
      }

      // Short notation: ftnF = flmG(seq ◦ fmapG(ftnP))
      // = flmG(seq) ◦ fmapG(ftnP) -- using naturality for flmG.

      // Let us keep writing in the short notation and use the laws until we can't simplify any more.
      // This will minimize the amount of computation that needs to be done in explicit code.

      // Compute fmapF(ftnF) ◦ ftnF = fmapG(fmapP(ftnF)) ◦ ftnF  -- now substitute the definition of ftnF:
      // = fmapG(fmapP(flmG(seq) ◦ fmapG(ftnP)) ◦ flmG(seq ◦ fmapG(ftnP)) 
      // We want to pull fmapP(FmapG(...)) ◦ seq together. Use naturality fmapG(x) ◦ flmG(y) = flmG(x ◦ y):
      // = flmG { fmapP(flmG(seq) ◦ fmapG(ftnP)) ◦ seq ◦ fmapG(ftnP) }  -- now split off fmapP(x ◦ y) = fmapP(x) ◦ fmapP(y):
      // = flmG { fmapP(flmG(seq)) ◦ fmapP(fmapG(ftnP)) ◦ seq ◦ fmapG(ftnP) } -- now use naturality of seq:
      // = flmG { fmapP(flmG(seq)) ◦ seq ◦ fmapG(fmapP(ftnP)) ◦ fmapG(ftnP) } -- now pull fmapG() together:
      // = flmG { fmapP(flmG(seq)) ◦ seq ◦ fmapG(fmapP(ftnP) ◦ ftnP) } -- now we can use the associativity law for P:
      // = flmG { fmapP(flmG(seq)) ◦ seq ◦ fmapG(ftnP ◦ ftnP) } -- pull fmapG() apart and pull fmapG(ftnP) out of flmG():
      // = flmG { fmapP(flmG(seq)) ◦ seq ◦ fmapG(ftnP) } ◦ fmapG(ftnP) -- nothing else to simplify.

      // Compute ftnF ◦ ftnF = flmG(seq) ◦ fmapG(ftnP) ◦ flmG(seq) ◦ fmapG(ftnP)
      // -- let's try to make it similar to the code above, using associativity and naturality of flmG:
      // ftnF ◦ ftnF = flmG { seq ◦ fmapG(ftnP) ◦ flmG(seq) } ◦ fmapG(ftnP)

      // The difference is in the functions under flmG { }. The type of these functions is P[G[P[G[A]]]] ⇒ G[P[A]].
      // Checking that the types compile:
      def f1[A]: P[G[P[G[A]]]] ⇒ G[P[A]] = seq[P[G[A]]] _ andThen fmapG(ftnP) andThen flmG(seq[A])

      def f2[A]: P[G[P[G[A]]]] ⇒ G[P[A]] = fmapP(flmG(seq[A])) _ andThen seq andThen fmapG(ftnP)

      // It remains to evaluate f1 and f2 symbolically and to show that their expressions are equivalent.
      // Since the argument is a P[...], there are two cases: Left(z) and Right((W, G[P[G[A]]])).
      // We will now symbolically evaluate f1() and f2() in each of these two cases and show that the results are equal.

      // Case 1: x = Left(z). Note that seq(Left(z)) = Monad[G].pure(Left(z)), while ftnP(Left(z)) = Left(z).
      // Also, Monad[G].pure(Left(z)).flatMap(seq) = Monad[G].pure(Left(z)) because of G's identity law.
      // So we get f1(x) = Monad[G].pure(Left(z))
      // and f2(x) = Monad[G].pure(Left(z)) because fmapP on a Left() is id.

      // Case 2: x = Right((w, gpga)). Compute seq(Right((w, gpga))) = gpga.map(pga ⇒ Right((w, pga)))
      // So (seq andThen fmapG(ftnP)) (x) = gpgaa.map(pga ⇒ Right((w, pga))).map(ftnP) = gpga.map(pga ⇒ ftnP(Right((w, pga))))
      // f1(x) = gpga.map(pga ⇒ ftnP(Right((w, pga)))).flatMap(seq)
      // = gpga.flatMap(pga ⇒ seq(ftnP(Right((w, pga)))))

      // For f2(x), first compute fmapP(flmG(seq))(x) = Right((w, gpga.flatMap(seq)))
      // Then apply seq to that, obtain gpga.flatMap(seq).map(pa ⇒ Right((w, pa)))
      // Finally apply fmapG(ftnP) to that, obtain gpga.flatMap(seq).map(pa ⇒ Right((w, pa))).map(ftnP)
      // We would like to pull everything under gpga.flatMap, so we using a naturality law for flatMap:
      // f2(x) = gpga.flatMap(pga ⇒ seq(pga).map(pa ⇒ ftnP(Right((w, pa)))))

      // It remains to compare `seq(ftnP(Right((w, pga))))` and `seq(pga).map(pa ⇒ ftnP(Right((w, pa))))`.
      // Consider two cases for `pga: P[G[A]]`, Left(z) and Right((w2, ga)).

      // Case 1: pga = Left(z). For f1, we have ftnP(Right((w, Left(z)))) = Left(z), then seq(Left(z)) = pureG(Left(z)) as before.
      // For f2, we have pureG(Left(z)).map(...) = pureG( Left(z) ).

      // Case 2: pga = Right((w2, ga)). For f1, we have ftnP(Right((w, Right((w2, ga)))) = Right((w |+| w2, ga)).
      // Then we compute seq of that and get ga.map( a ⇒ Right((w |+| w2, a))).
      // For f2, we have seq(Right((w2, ga))) = ga.map(a ⇒ Right((w2, a))).
      // Then we compute `.map(pa ⇒ ftnP(Right((w, pa))))` of that and obtain
      // ga.map(a ⇒ Right((w |+| w2, a))).
    }
  }

  it should "verify monad construction 7" in {
    // Free monad over the functor G.

    def construction7[G[_] : Functor](): Unit = {

      case class F[A](eag: Either[A, G[F[A]]])

      def fmap[A, B](f: A ⇒ B): F[A] ⇒ F[B] = {
        case F(Left(a)) ⇒ F(Left(f(a)))
        case F(Right(gfa)) ⇒ F(Right(gfa.map(fmap(f))))
      }

      def ftn[A]: F[F[A]] ⇒ F[A] = {
        case F(Left(fa)) ⇒ F(fa.eag)
        case F(Right(gffa)) ⇒ F(Right(gffa.map(ftn[A])))
      }
      
      def pure[A]: A ⇒ F[A] = a ⇒ F(Left(a))
      
      // Identity laws.
      
    }
  }
}
