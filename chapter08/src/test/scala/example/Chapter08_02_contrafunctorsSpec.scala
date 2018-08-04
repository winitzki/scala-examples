package example

import cats.syntax.contravariant._
import cats.syntax.functor._
import cats.syntax.invariant._
import cats.syntax.profunctor
import cats.{Contravariant, Functor, Invariant}
import example.ContraWuZip._
import example.ProWuZip.ProWuZipSyntax
import javax.jws.soap.SOAPBinding.Use
import org.scalatest.{FlatSpec, Matchers}

class Chapter08_02_contrafunctorsSpec extends FlatSpec with Matchers {

  behavior of "applicative contrafunctor constructions"

  it should "define construction 3 for contrafunctors" in {
    // If G and H are applicative contrafunctors then G + H is also applicative.

    // Contrafunctor instance:
    implicit def contrafunctorSum[G[_] : Contravariant, H[_] : Contravariant]: Contravariant[Lambda[A ⇒ Either[G[A], H[A]]]] = new Contravariant[Lambda[A ⇒ Either[G[A], H[A]]]] {
      override def contramap[A, B](fa: Either[G[A], H[A]])(f: B ⇒ A): Either[G[B], H[B]] = fa match {
        case Left(ga) ⇒ Left(ga contramap f)
        case Right(ha) ⇒ Right(ha contramap f)
      }
    }

    // Applicative instance:
    implicit def contraaplicativeSum[G[_] : ContraWuZip : Contravariant, H[_] : ContraWuZip : Contravariant]: ContraWuZip[Lambda[A ⇒ Either[G[A], H[A]]]] = new ContraWuZip[Lambda[A ⇒ Either[G[A], H[A]]]] {
      override def wu: Either[G[Unit], H[Unit]] = Left(wU[G]) // We need to make an arbitrary choice between Left() and Right() here.

      override def zip[A, B](fa: Either[G[A], H[A]], fb: Either[G[B], H[B]]): Either[G[(A, B)], H[(A, B)]] = (fa, fb) match {
        case (Left(ga), Left(gb)) ⇒ Left(ga zip gb)
        case (Left(_), Right(hb)) ⇒ Right(hb contramap { case (x, y) ⇒ y }) // Since the wrapped unit is a `Left()`, we need to create a `Right()` in the product.
        case (Right(ha), Left(_)) ⇒ Right(ha contramap { case (x, y) ⇒ x })
        case (Right(ha), Right(hb)) ⇒ Right(ha zip hb)
      }
    }

    /* Check the laws:
    
    Associativity: Let's reformulate the computation of `zip` of 3 arguments in a way that is manifestly associative.
     
    (fa zip fb zip fc) is computed as G's `zip` or H's `zip` if all fa, fb, fc are on one side.
    This is associative.
    
    If some of (fa, fb, fc) are Left() while others are Right(), all the Left() ones are ignored,
    and the remaining Right() ones are converted to Right[H[(A, B, C)]] using contramap on the function
    such as { case (a, b, c) ⇒ zipH(a, b) } as required.
    This is also associative.
    
    Identity laws: Assuming that G's identity law works - and not using H's identity laws - we find: 
    
    (fa zip wu) = (fa zip Left(wU[G]) = (fa, Left(wU[G])) match {
      case (Left(ga), Left(gb)) ⇒ Left(ga zip gb) = Left(ga zip wU[G]) ≅ Left(ga)
      case (Right(ha), Left(gb)) ⇒ Right(ha contramap { case (x, ()) ⇒ x } ≅ Right(ha)
     }
     */
  }

  it should "define construction 4 for contrafunctors" in {
    // If H[A] is any functor and G[A] is a contrafunctor then H[A] ⇒ G[A] is applicative.

    // Contrafunctor instance:
    implicit def contrafunctor4[G[_] : Contravariant, H[_] : Functor]: Contravariant[Lambda[A ⇒ H[A] ⇒ G[A]]] = new Contravariant[Lambda[A ⇒ H[A] ⇒ G[A]]] {
      override def contramap[A, B](fa: H[A] ⇒ G[A])(f: B ⇒ A): H[B] ⇒ G[B] = { hb ⇒ fa(hb map f) contramap f }
    }

    // Applicative instance:
    implicit def contraaplicative4[G[_] : ContraWuZip : Contravariant, H[_] : Functor]: ContraWuZip[Lambda[A ⇒ H[A] ⇒ G[A]]] = new ContraWuZip[Lambda[A ⇒ H[A] ⇒ G[A]]] {
      override def wu: H[Unit] ⇒ G[Unit] = { _ ⇒ wU[G] }

      override def zip[A, B](fa: H[A] ⇒ G[A], fb: H[B] ⇒ G[B]): H[(A, B)] ⇒ G[(A, B)] = { hab ⇒
        val ha: H[A] = hab map { case (a, b) ⇒ a }
        val hb: H[B] = hab map { case (a, b) ⇒ b }
        fa(ha) zip fb(hb)
      }
    }

    /* Check the laws:
    
    Associativity:
    
    Consider `(fa zip fb) zip fc: H[((A, B), C)] ⇒ G[((A, B), C)]`:
    
    fa zip fb = { hab ⇒ fa(hab map(_._1)) zip fb(hab map(_._2) }
    (fa zip fb) zip fc
     = { habc: H[((A, B), C)] ⇒ (fa zip fb)(habc map(_._1) zip fc(habc map (_._2))
     = { habc: H[((A, B), C)] ⇒ (fa(habc map(_._1) map (_._1)) zip fb(habc map (_._1) map (_._2)) zip fc(habc map (_._2))
    
    To simplify the data structure, let's flatten the tuple type, from ((A, B), C) to (A, B, C):
    
    We will need to map h_abc: H[(A, B, C)] as h_abc map { case (a, b, c) ⇒ ((a, b), c) } and call that habc in the expression above.
    Note that we have expressions such as `habc map (_._1) map (_._1)`.
    These are simplified as h_abc map { case (a, b, c) ⇒ ((a, b), c)._1._1 } = h_abc map (_._1)
    and so on. Thus:
    
    ((fa zip fb) zip fc) contramap { case (a, b, c) ⇒ ((a, b), c) } = { habc: H[(A, B, C)] ⇒
      ((fa(h_abc map(_._1)) zip fb(h_abc map (_._2))) zip fc(h_abc map (_._3)) contramap { case (a, b, c) ⇒ ((a, b), c) }
    }
    
    The last step is G's zip, which is associative by assumption, followed by an isomorphism transformation. 
    
    Similarly, starting with `fa zip (fb zip fc): H[(A, (B, C))] ⇒ G[(A, (B, C))]` and applying `contramap { case (a, b, c) ⇒ (a, (b, c)) }`, we will find:
    
    (fa zip (fb zip fc)) contramap { case (a, b, c) ⇒ (a, (b, c)) } = { habc: H[(A, B, C)] ⇒
      (fa(h_abc map(_._1)) zip (fb(h_abc map (_._2)) zip fc(h_abc map (_._3))) contramap { case (a, b, c) ⇒ (a, (b, c)) }
    }
      
    The associativity of G's zip shows that this is the same expression after the isomorphism.
      
    Identity:
    
    Compute zip(fa, _ ⇒ wU[G]) = { hab ⇒ ... fa(ha) zip (_ ⇒ wU[G])(hb) }
    Clearly (_ ⇒ wU[G])(hb) = wU[G]. Hence we get fa(ha) zip wU[G].
    Use G's identity law for its `zip`, and find that fa(ha) is just mapped from G[A] into G[(A, Unit)].
    This is the isomorphism we expect.
    
    So the identity laws hold. 
     */
  }

  it should "define construction 5 for contrafunctors" in {
    // If G[A] is a functor and H[A] is a contrafunctor, both applicative, then G[H[A]] is an applicative contrafunctor.

    // Contrafunctor instance:
    implicit def contrafunctor5[G[_] : Functor, H[_] : Contravariant]: Contravariant[Lambda[A ⇒ G[H[A]]]] = new Contravariant[Lambda[A ⇒ G[H[A]]]] {
      override def contramap[A, B](gha: G[H[A]])(f: B ⇒ A): G[H[B]] = gha.map(_ contramap f)
    }

    // Applicative instance:
    implicit def contraaplicative5[G[_] : WuZip : Functor, H[_] : ContraWuZip : Contravariant]: ContraWuZip[Lambda[A ⇒ G[H[A]]]] = new ContraWuZip[Lambda[A ⇒ G[H[A]]]] {
      override def wu: G[H[Unit]] = WuZip[G].pure(wU[H])

      import WuZip.WuZipSyntax

      override def zip[A, B](gha: G[H[A]], ghb: G[H[B]]): G[H[(A, B)]] =
        (gha zip ghb).map { case (ha, hb) ⇒ ha zip hb }
    }

    /* Check the laws:
    
    Follow the same proof as in construction 8 for applicative functors.
    We use `map2` and `pureG` for G, but we never use `map2` or `pure` for `H` in that proof.
    We only use the laws of H's `zip` and `wu`. Therefore, the same proof goes through here.
    
     */
  }

  it should "define construction 4 for profunctors" in {
    // If H[A] is any profunctor then F[A] ≡ H[A] ⇒ A is an applicative profunctor.

    // For convenience, introduce `H[_]` as a type parameter up front.
    def construction4[H[_] : Invariant](): Unit = {

      // This is the new profunctor.
      type F[A] = H[A] ⇒ A

      // Profunctor instance:
      implicit def profunctor4: Invariant[F] = new Invariant[F] {
        override def imap[A, B](fa: H[A] ⇒ A)(f: A ⇒ B)(g: B ⇒ A): H[B] ⇒ B = { hb ⇒
          val ha: H[A] = hb.imap(g)(f)
          f(fa(ha))
        }
      }

      // Applicative instance:
      implicit def proaplicative4: ProWuZip[F] = new ProWuZip[F] {
        override def wu: H[Unit] ⇒ Unit = { _ ⇒ () }

        override def zip[A, B](fa: H[A] ⇒ A, fb: H[B] ⇒ B): H[(A, B)] ⇒ (A, B) = { hab ⇒
          // The plan: we can get `(A, B)` only if we somehow compute some values of types `H[A]` and `H[B]`.
          // The trick: First obtain a value of type A ⇒ (A, B).
          // This will allow us to map `hab` into a value of type `H[A]`.
          val aab: A ⇒ (A, B) = { a ⇒
            // Obtain a value of type H[B], then use `fb` on it to get a `B`.
            val hb: H[B] = hab.imap(_._2)(b ⇒ (a, b)) // Syntax: `b ⇒ (a, b)` is the same as `((a, _))`
          val b = fb(hb)
            (a, b)
          }
          val ha: H[A] = hab.imap(_._1)(aab)

          // Do the same with B instead of A:
          // Obtain a value of type B ⇒ (A, B) and get an `H[B]`.
          val bab: B ⇒ (A, B) = { b ⇒
            // Obtain a value of type H[A], then use `fa` on it to get an `A`.
            val ha: H[A] = hab.imap(_._1)(a ⇒ (a, b))
            val a = fa(ha)
            (a, b)
          }
          val hb: H[B] = hab.imap(_._2)(bab)

          (fa(ha), fb(hb))
        }
      }

      /* Check the laws:

      Associativity:
      
      First, refactor the implementation of the `zip` method as:
      */
      def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)] = { hab ⇒ (fa(get1(fb, hab)), fb(get2(fa, hab))) }

      // where we defined the auxiliary functions,

      def get1[A, B](fb: F[B], hab: H[(A, B)]): H[A] = hab.imap(_._1) { a ⇒ (a, fb(hab.imap(_._2)((a, _)))) }

      def get2[A, B](fa: F[A], hab: H[(A, B)]): H[B] = hab.imap(_._2) { b ⇒ (fa(hab.imap(_._1)((_, b))), b) }

      /*
      Note how the implementation uses ha and hb completely symmetrically.
      We expect this to be the correct implementation (as opposed to reusing `ha`
      when defining `bab`, say).
      
      It is not easy to reason about the result of `fa zip fb zip fc` directly.
      So we will need to perform a full symbolic computation.

      However, the code for `zip(fa, fb)` is completely symmetric w.r.t. (fa, fb).
      So, let us convert the result of  `(fa zip fb) zip fc : F[((A, B), C)]`
      to the "flattened tuple" type `F[(A, B, C)]`. If the resulting code is symmetric in fa, fb, fc,
      and does not show any dependence on computing zip(fa, fb) first,
      we will have proven associativity.

      The conversion is done by applying an `imap{ case ((a, b), c) ⇒ (a, b, c) }{ case (a, b, c) ⇒ ((a, b), c) }`:
      */
      def zipzip[A, B, C](fa: F[A], fb: F[B], fc: F[C]): F[(A, B, C)] = {
        val res1: F[(A, B, C)] = zip(zip(fa, fb), fc)
          .imap { case ((a, b), c) ⇒ (a, b, c) } { case (a, b, c) ⇒ ((a, b), c) }

        // The implementation of .imap()() is given above in the `Invariant` instance for `F`.
        // But we will do this at the end, since a lot of other simplification needs to occur first.

        // Substitute the definition of `zip`:
        val zipzip1: F[((A, B), C)] = zip({ hab ⇒ (fa(get1(fb, hab)), fb(get2(fa, hab))) }, fc)
        // Substitute again:
        val zipzip2: F[((A, B), C)] = { h_ab_c: H[((A, B), C)] ⇒
          (
            (fa(get1(fb, get1(fc, h_ab_c))), fb(get2(fa, get1(fc, h_ab_c)))),
            fc(get2({ hab: H[(A, B)] ⇒ (fa(get1(fb, hab)), fb(get2(fa, hab))) }, h_ab_c))
          )
        }
        // Here `habc` is of type `H[((A, B), C)]`, so mapping it with say _._1 would yield an `H[(A, B)]`.

        // Convert to a flattened tuple type: The result will be
        /*
        (
          fa(get1(fb, get1(fc, h_ab_c))),
          fb(get2(fa, get1(fc, h_ab_c))),
          fc(get2({ hab: H[(A, B)] ⇒ (fa(get1(fb, hab)), fb(get2(fa, hab))) }, h_ab_c))
        )
        Let's simplify the arguments of fa, fb, fc here. Denote these arguments by ha, hb, hc.
         */

        val zipzip2a: F[(A, B, C)] = { h_abc: H[(A, B, C)] ⇒
          val h_ab_c: H[((A, B), C)] = h_abc.imap { case (a, b, c) ⇒ ((a, b), c) } { case ((a, b), c) ⇒ (a, b, c) }
          val hab: H[(A, B)] = // get1(fc, h_ab_c)
          // h_ab_c.imap(_._1) { a ⇒ (a, fc(h_ab_c.imap(_._2)((a, _)))) }
          //   Use the profunctor composition law to simplify:
          // h_abc.imap { case (a, b, c) ⇒ ((a, b), c) } { case ((a, b), c) ⇒ (a, b, c) }
          //      .imap(_._1) { a ⇒ (a, fc(h_ab_c.imap(_._2)((a, _)))) }
            h_abc.imap { case (a, b, c) ⇒ (a, b) } { case (a, b) ⇒ (a, b, fc(h_abc.imap(_._3)((a, b, _)))) }
          val ha: H[A] = // get1(fb, hab)
          // hab.imap(_._1) { a ⇒ (a, fb(hab.imap(_._2)((a, _)))) }

          // h_abc.imap { case (a, b, c) ⇒ (a, b) } { case (a, b) ⇒ (a, b, fc(h_abc.imap(_._3)((a, b, _)))) }
          //      .imap(_._1) { a ⇒ (a, fb(h_abc.imap { case (a, b, c) ⇒ (a, b) } { case (a, b) ⇒ (a, b, fc(h_abc.imap(_._3)((a, b, _)))) }.imap(_._2)((a, _)))) }

          // h_abc.imap { case (a, b, c) ⇒ a }({ a ⇒
          //   val b0 = fb(h_abc.imap { case (a, b, c) ⇒ (a, b) } { case (a, b) ⇒ (a, b, fc(h_abc.imap(_._3)((a, b, _)))) }.imap(_._2)((a, _)))
          //   (a, b0)
          // } andThen { case (a, b) ⇒ (a, b, fc(h_abc.imap(_._3)((a, b, _)))) })

          // Simplify b0 = fb(h_abc.imap (_._2) { case b ⇒ (a, b, fc(h_abc.imap(_._3)((a, b, _)))) })
          // Hence ha =
            h_abc.imap(_._1) { a ⇒
              val b0 = fb(h_abc.imap(_._2) { b ⇒ (a, b, fc(h_abc.imap(_._3)((a, b, _)))) })
              (a, b0, fc(h_abc.imap(_._3)((a, b0, _))))
            }
          // Compute hb in the same way, just exchange a and b in ha above.
          val hb: H[B] = // get2(fa, hab)
            h_abc.imap(_._2) { b ⇒
              val a0 = fa(h_abc.imap(_._1) { a ⇒ (a, b, fc(h_abc.imap(_._3)((a, b, _)))) })
              (a0, b, fc(h_abc.imap(_._3)((a0, b, _))))
            }
          // The argument of fc is `get2({ hab: H[(A, B)] ⇒ (fa(get1(fb, hab)), fb(get2(fa, hab))) }, h_ab_c)`.
          val hc: H[C] = // h_ab_c.imap(_._2) { c ⇒ ((fa(get1(fb, h_ab_c.imap(_._1)((_, c)))), fb(get2(fa, h_ab_c.imap(_._1)((_, c))))), c) }
          //            h_abc.imap(_._3) { c ⇒ (fa(get1(fb, h_ab_c.imap(_._1)((_, c)))), fb(get2(fa, h_ab_c.imap(_._1)((_, c)))), c) }
          // Compute get1(fb, h_ab_c.imap(_._1)((_, c))))
          //  = h_ab_c.imap(_._1)((_, c))).imap(_._1) { a ⇒ (a, fb(h_ab_c.imap(_._1)((_, c))).imap(_._2)((a, _)) )) }
          //                                                            simplify here to:
          //                                                        h_abc.imap(_._2)((a, _, c))
          //          simplify here to:
          //  = h_abc.imap(_._1){ a ⇒ (a, fb(h_abc.imap(_._2)((a, _, c)) ), c) }
          // Similarly get2(fa, h_ab_c.imap(_._1)((_, c)))
          //  = h_abc.imap(_._2){ b ⇒ (fa(h_abc.imap(_._1)((_, b, c)) ), b, c) }
          // Hence hc =
            h_abc.imap(_._3) { c ⇒ (fa(h_abc.imap(_._1) { a ⇒ (a, fb(h_abc.imap(_._2)((a, _, c))), c) }), fb(h_abc.imap(_._2) { b ⇒ (fa(h_abc.imap(_._1)((_, b, c))), b, c) }), c) }

          // Exchange a and c in ha:
          val hc2: H[C] = h_abc.imap(_._3) { c ⇒
            val b0 = fb(h_abc.imap(_._2) { b ⇒ (fa(h_abc.imap(_._1)((_, b, c))), b, c) })
            (fa(h_abc.imap(_._1)((_, b0, c))), b0, c)
          }

          // Compare hc and hc2:
          /*
          (fa(h_abc.imap(_._1) { a ⇒ (a, fb(h_abc.imap(_._2){ b ⇒ (a, b, c)) }, c) }),
              fb(h_abc.imap(_._2) { b ⇒ (fa(h_abc.imap(_._1)((_, b, c))), b, c) }),
              c)
          (fa(h_abc.imap(_._1) { a ⇒ (a, fb(h_abc.imap(_._2) { b ⇒ (fa(h_abc.imap(_._1)((_, b, c))), b, c) }), c) }),
              fb(h_abc.imap(_._2) { b ⇒ (fa(h_abc.imap(_._1)((_, b, c))), b, c) }),
              c)
              
              They don't look the same!!!
           */
          ???
        }

        // Now we need to simplify the expressions that occur after `habc ⇒`.
        val zipzip3 = { habc: H[((A, B), C)] ⇒
          // Simplify the last line:
          val hc1: H[C] = get2({ hab: H[(A, B)] ⇒ (fa(get1(fb, hab)), fb(get2(fa, hab))) }, habc)

          // Compute the term `get1(fc, habc)`, which is of type `H[(A, B)]`:
          val xab1: H[(A, B)] = habc.imap(_._1) { ab ⇒ (ab, fc(habc.imap(_._2)((ab, _)))) }
          /*
      The mapping `habc.imap(_._2)(ab, _)` selects the `C` part of `H[((A, B), C)]`.
      So let us denote it for brevity by `habc2c`. (It is still a function of a local value `ab`.)
      Thus we can write `xab1` as
           */

          def habc2c(a: A, b: B): H[C] = habc.imap(_._2)(((a, b), _))

          val xab2: H[(A, B)] = habc.imap(_._1) { case (a, b) ⇒ ((a, b), fc(habc2c(a, b))) }

          // Define for convenience also some other functions for converting `habc`:

          def habc2a_bc(b: A ⇒ B, c: (A, B) ⇒ C): H[A] = habc.imap(_._1._1)(a ⇒ ((a, b(a)), c(a, b(a))))

          def habc2b_c(a: A, bc: B ⇒ C): H[B] = habc.imap(_._1._2)(b ⇒ ((a, b), bc(b)))

          // Now compute the argument of `fa()`, which is `get_a(fb, left(fc, habc)) = get_a(fb, xab2)`:

          val xa1: H[A] =
          // left(fb, xab2)
          // xab2.imap(_._1) { a ⇒ (a, fb(xab2.imap(_._2)((a, _)))) }

          // habc
          //   .imap(_._1) { case (a, b) ⇒ ((a, b), fc(habc2c(a, b))) }
          //   .imap(_._1) { a ⇒ (a, fb(habc.imap(_._1) { case (a, b) ⇒ ((a, b), fc(habc2c(a, b))) }.imap(_._2)((a, _)))) }

          // Use the profunctor composition law:
          //      p.imap(f)(m).imap(g)(n) = p.imap(f andThen g)(n andThen m)
          // to simplify the expressions with double .imap:
          // habc
          //   .imap(_._1) { case (a, b) ⇒ ((a, b), fc(habc2c(a, b))) }
          //   .imap(_._1) { a ⇒ (a, fb(habc.imap(_._1._2) { b ⇒ ((a, b), fc(habc2c(a, b))) })) }
          // Rewrite as
          // habc
          //   .imap(_._1) { case (a, b) ⇒ ((a, b), fc(habc2c(a, b))) }
          //   .imap(_._1) { a ⇒ (a, fb(habc2b_c(a, { b ⇒ fc(habc2c(a, b)) }))) }

          // habc
          //   .imap(_._1._1) {
          //     { a: A ⇒ (a, fb(habc2b_c(a, { b ⇒ fc(habc2c(a, b)) }))) } andThen { case (a, b) ⇒ ((a, b), fc(habc2c(a, b))) }
          // }

          // Rewrite more concisely as
            habc2a_bc(a ⇒ fb(habc2b_c(a, { b ⇒ fc(habc2c(a, b)) })), (a, b) ⇒ fc(habc2c(a, b)))


          // Compute the argument of `fb()`, which is `right(fa, left(fc, habc))`.
          // This is just xa1 except we swap `a` and `b` everywhere:
          val xb1: H[B] = ???

        }


        ???
      }

      /*    
            
            
            left(fc, habc) 
            
            Compute the argument of `fc()`:
             
            right({ hab ⇒ (fa(left(fb, hab)), fb(right(fa, hab))) }, habc) : H[C] =
              habc.imap(_._2){ c ⇒ ((fa(left(fb, habc.imap(_._1)(_, c)), fb(right(fa, habc.imap(_._1)(_, c)))), c) }
            
            First simplify the arguments of `fa()` and `fb()`.
            
            left(fb, habc.imap(_._1)(_, c)) : H[A] = habc.imap(_._1)(_, c).imap(_._1){ a ⇒ (a, fb(habc.imap(_._1)(_, c).imap(_._2)(a, _))) }
            
            Use the profunctor composition law to simplify: 
            
            habc.imap(_._1)(_, c).imap(_._2)(a, _) = habc.imap{ case ((a, b), c) ⇒ b }{ b ⇒ ((a, b), c) }
            
            This mapping selects the middle `B` out of `H[((A, B), C)]` to obtain an `H[B]`. Denote the result by `habc2b`.
            
            So
            
            left(fb, habc.imap(_._1)(_, c)) = habc.imap{ case ((a, b), c) ⇒ a }{ a ⇒ ((a, fb(habc2b)), c) }
            
            Similarly compute:
            
            right(fa, habc.imap(_._1)(_, c)): H[B] = habc.imap(_._1)(_, c).imap(_._2){ b ⇒ (fa(habc.imap(_._1)(_, c).imap(_._1)(_, b)), b) }
            
            Simplify using the profunctor composition law:
            
            right(fa, habc.imap(_._1)(_, c)) = habc.imap{ case ((a, b), c) ⇒ b }{ b ⇒ (fa(habc2a), b) }
            
            where habc2a = habc.imap(_._1)(_, c).imap(_._1)(_, b) = habc.imap{ case ((a, b), c) ⇒ a }{ a ⇒ ((a, b), c) } 
            
            Now substitute the simplified arguments of `fa()` and `fb()`:
            
            right({ hab ⇒ (fa(left(fb, hab)), fb(right(fa, hab))) }, habc) : H[C] =
              habc.imap(_._2){ c ⇒ ((
                fa(habc.imap{ case ((a, b), c) ⇒ a }{ a ⇒ ((a, fb(habc2b)), c) }),
                fb(habc.imap{ case ((a, b), c) ⇒ b }{ b ⇒ (fa(habc2a), b) })
               ), c) }
            
            So finally we have zip(zip(fa, fb), fc) : H[((A, B), C)] ⇒ ((A, B), C) =
              { habc ⇒ (
                    habc.imap(_._1){ ab ⇒ (ab, fc(habc2c)) },
                    habc.imap(_._2){ c ⇒ ((
                       fa(habc.imap{ case ((a, b), c) ⇒ a }{ a ⇒ ((a, fb(habc2b)), c) }),
                       fb(habc.imap{ case ((a, b), c) ⇒ b }{ b ⇒ (fa(habc2a), b) })
                      ), c) }
                  )
              }
            
            To find out whether this is associative, let us apply the required isomorphisms
            in order to transform `zip(zip(fa, fb), fc)` into a value
            of a "flattened tuple" type `H[(A, B, C)] ⇒ (A, B, C)`.
            This is done by applying an `imap{ case ((a, b), c) ⇒ (a, b, c) }{ case (a, b, c) ⇒ ((a, b), c) }`:
            
            fabc.imap{ case ((a, b), c) ⇒ (a, b, c) }{ case (a, b, c) ⇒ ((a, b), c) } =
            { (habcFlat: H[(A, B, C)]) ⇒
              val habc: H[((A, B), C)] = habcFlat.imap{ case (a, b, c) ⇒ ((a, b), c) }{ case ((a, b), c) ⇒ (a, b, c) }
              fa(habc) match { case ((a, b), c) ⇒ (a, b, c) }
            } 
            
            Substitute the body of the function into fa(habc):
            
            zip(zip(fa, fb), fc).imap{ case ((a, b), c) ⇒ (a, b, c) }{ case (a, b, c) ⇒ ((a, b), c) } =
            { (habcFlat: H[(A, B, C)]) ⇒
              val habc: H[((A, B), C)] = habcFlat.imap{ case (a, b, c) ⇒ ((a, b), c) }{ case ((a, b), c) ⇒ (a, b, c) }
              
              (
                habc.imap(_._1){ ab ⇒ (ab, fc(habc2c)) },
                habc.imap(_._2){ c ⇒ ((
                   fa(habc.imap{ case ((a, b), c) ⇒ a }{ a ⇒ ((a, fb(habc2b)), c) }),
                   fb(habc.imap{ case ((a, b), c) ⇒ b }{ b ⇒ (fa(habc2a), b) })
                  ), c) }
              ) match { case ((a, b), c) ⇒ (a, b, c) }
            }
            
            Now we can simplify by using the profunctor composition law:
            
            habc.imap(_._1){ ab ⇒ (ab, fc(habc2c)) } =
              habcFlat.imap{ case (a, b, c) ⇒ ((a, b), c) }{ case ((a, b), c) ⇒ (a, b, c) }.imap(_._1){ ab ⇒ (ab, fc(habc2c)) } =
              habcFlat.imap{ case (a, b, c) ⇒ (a, b) }{ case (a, b) ⇒ (a, b, fc(habc2c)) }
      
            
              
            Identity:
            
            zip(fa, wuF) = zip(fa, {_ ⇒ 1 }) = { ha1 ⇒ (fa(left(wuF, ha1)), wuF(...)) }
             = { ha1 ⇒ (fa(left(wuF, ha1)), 1) }
             
            Compute left(wuF, ha1) = ha1.imap(_._1){ a ⇒ (a, wuF(...)) } = ha1.imap(_._1)(_, 1)
            This is an equivalence transformation in the sense of `≅`, since we are just adding a unit to the type.
            Hence zip(fa, wuF) ≅ { ha1 ⇒ fa(ha1) } up to equivalence. So zip(fa, wuF) ≅ fa.
            
            Since the code of `zip` is symmetric, it's sufficient to check just one identity law.
             */
    }
  }

}
