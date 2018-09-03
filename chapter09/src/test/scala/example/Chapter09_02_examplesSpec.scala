package example

import cats.{Applicative, Bifunctor, Bitraverse, Functor}
import org.scalatest.{FlatSpec, Matchers}
import cats.syntax.functor._
import cats.syntax.bifunctor._
import cats.syntax.bitraverse._
import WuZip.WuZipSyntax
import Trav.TravSyntax

class Chapter09_02_examplesSpec extends FlatSpec with Matchers {

  behavior of "traversable functor constructions"

  it should "define traversable for constant functor" in {
    def withParam[Z](): Unit = {
      type L[A] = Z

      // Constant functor: L[A] = Z; L[F[A]] = Z; F[L[A]] = F[Z];
      // need Z ⇒ F[Z], which is F.pure.
      def seq[F[_] : WuZip : Functor, A](lfa: L[F[A]]): F[L[A]] = WuZip[F].pure(lfa)

      // Check laws:

      // Identity law: fmap_L(F.pure) ◦ seq = F.pure.
      // Since fmap_L is the identity function, and seq = F.pure, the law is satisfied.

      // Composition law: seq[F] ◦ fmap_F(seq[G]) = seq[FG] where we denote FG[A] = F[G[A]].
      // Since seq[F] = F.pure, we need F.pure andThen fmap_F(G.pure) = (FG).pure
      // But the definition of pure for FG is exactly F.pure andThen fmap_F(G.pure).
      // So the composition law holds.
    }
  }

  it should "define traversable for identity functor" in {
    type L[A] = A

    // Identity functor: Id[F[A]] ⇒ F[Id[A]] is the identity function.
    def seq[F[_] : WuZip : Functor, A](lfa: L[F[A]]): F[L[A]] = lfa

    // Check laws:

    // Identity law: fmap_L(F.pure) ◦ seq = F.pure.
    // Since fmap_L is the identity function, and seq = id, the law is satisfied.

    // Composition law: seq[F] ◦ fmap_F(seq[G]) = seq[FG]
    // Since seq[anything] = id, the law is about composition of identity functions being identity.
    // So the composition law holds.
  }

  it should "derive traversable for functor product" in {
    def withParam[L1[_] : Trav : Functor, L2[_] : Trav : Functor](): Unit = {
      type L[A] = (L1[A], L2[A])

      implicit val functorL: Functor[L] = new Functor[L] {
        override def map[A, B](fa: (L1[A], L2[A]))(f: A ⇒ B): (L1[B], L2[B]) = (fa._1 map f, fa._2 map f)
      }

      implicit val travL: Trav[L] = new Trav[L] {
        override def seq[F[_] : WuZip : Functor, A](lfa: (L1[F[A]], L2[F[A]])): F[(L1[A], L2[A])] = {
          Trav[L1].seq[F, A](lfa._1) zip Trav[L2].seq[F, A](lfa._2)
          //  lfa._1.seq zip lfa._2.seq
        }
      }

      // Check laws:

      // Identity law: fmap_L(F.pure[A]) ◦ seq = F.pure[L[A]]
      /*
        Substitute the code of fmap_L:
        fmap_L(F.pure)((l1a, l2a)) = (fmap_L(F.pure)(l1a), fmap_L(F.pure)(l2a))  
        Then apply seq to this:
        
        fmap_L1(F.pure)(l1a).seq zip fmap_L2(F.pure)(l2a).seq
        
        Since the law holds for L1[_] and L2[_], we have
          fmap_L1(F.pure)(l1a).seq = F.pure(l1a)
        and similarly for L2. Therefore we have F.pure(l1a) zip F.pure(l2a).
        The identity laws of applicative now say that this equals F.pure( (l1a, l2a) ).
        This is the same as F.pure[L[A]]( (l1a, l2a) ).
       */

      // Composition law: seq_L[F] ◦ fmap_F(seq_L[G]) = seq_L[FG]
      /*
        Apply both sides to some (l1fga, l2fga): L[F[G[A]]].
        After applying seq_L[F], this becomes
        
          l1fga.seq[F] zip_F l2fga.seq[F] : F[ (L1[G[A]], L2[G[A]]) ].
        
        Now we need to apply fmap_F(seq[G]) to that.
        
        seq_L[G] acts on some (l1ga, l2ga) and gives l1ga.seq zip_G l2ga.seq
        
        However, we need to lift this to act on F[L[G[A]]]. We use the definition of zip_FG:
        
        fmap_F( (gx, gy) ⇒ gx zip_G gy ) (fgx zip_F fgy) = fgx zip_FG fgy  
        
        then use naturality of zip to transform gx and gy using some functions p and q:
        
        fmap_F( (gx, gy) ⇒ p(gx) zip_G q(gy)) ) (fgx zip_F fgy) = fgx.map(p) zip_FG fgy.map(q)  
         
        and finally obtain
        
        fmap_F(seq_L[G]) ( l1fga.seq[F] zip_F l2fga.seq[F] ) = l1fga.seq[F].map(_.seq[G]) zip_FG l2fga.seq[F].map(_.seq[G])
        
        This is the left-hand side of the composition law. The right-hand side is
        
        l1fga.seq[FG] zip_FG l2fga.seq[FG]
        
        The definition of seq[FG] on L1 is the code of seq applied to the applicative functor FG.
        We assume that the composition law already holds for L1: l1fga.seq[F].map(_.seq[G]) = l1fga.seq[FG].
        Similarly for L2.
        
        Therefore the composition law holds for L.
       */
    }
  }

  it should "derive traversable for functor disjunction" in {
    def withParam[L1[_] : Trav : Functor, L2[_] : Trav : Functor](): Unit = {
      type L[A] = Either[L1[A], L2[A]]

      implicit val functorL: Functor[L] = new Functor[L] {
        override def map[A, B](fa: L[A])(f: A ⇒ B): L[B] = fa match {
          case Left(l1a) ⇒ Left(l1a map f)
          case Right(l2a) ⇒ Right(l2a map f)
        }
      }

      implicit val travL: Trav[L] = new Trav[L] {
        override def seq[F[_] : WuZip : Functor, A](lfa: L[F[A]]): F[L[A]] = lfa match {
          case Left(l1fa) ⇒ l1fa.seq map Left.apply
          case Right(l2fa) ⇒ l2fa.seq map Right.apply
        }
      }
    }

    // Check laws:

    // Identity law: fmap_L(F.pure[A]) ◦ seq = F.pure[L[A]]
    /*
      Substitute the code of fmap_L and apply to some Left(l1a) : L[A]
       -- it is enough to consider Left() since the code for Right(l2a) is symmetrically similar.
       
      fmap_L(F.pure)(Left(l1a)) = Left(l1a map F.pure): L[F[A]]
      
      Then apply seq to this and get
      
      seq_L1(l1a map F.pure).map(Left.apply)
      
      Since the law holds for L1[_] , we have seq_L1(l1a map F.pure) = F.pure(l1a)
      Therefore we have F.pure(l1a).map(Left.apply) = F.pure[L[A]](Left(l1a)).
      
      This is the same as the right-hand side of the identity law.
     */

    // Composition law: seq_L[F] ◦ fmap_F(seq_L[G]) = seq_L[FG]
    /*
      Apply both sides to some Left(l1fga): L[F[G[A]]].
      After applying seq_L[F], this becomes
      
        seq_L1(l1fga) map Left.apply: F[L[G[A]]]
      
      Now we need to apply fmap_F(seq_L[G]) to that; we can combine the two .map_F() calls:
      
        seq_L1(l1fga).map_F(l1ga ⇒ seq_L[G](Left(l1ga)))
      
      seq_L[G] acts on Left(l1ga) and gives l1ga.seq map_G Left.apply
      
      So we get
        seq_L1(l1fga).map_F(l1ga ⇒ l1ga.seq map_G Left.apply)
      
      We assume that the composition law already holds for L1, so
      
        seq_L1[F] ◦ fmap_F(seq_L1[G]) = seq_L1[FG]
      
      in other words
      
        seq_L1[F](l1fga) map_F (l1ga ⇒ l1ga.seq) = seq_L1[FG](l1fga)
      
      However, we need to lift this to act on F[L[G[A]]]. We use naturality of seq_L and
      apply fmap_FG(Left.apply) at the right-hand side, to get seq_L[FG](Left(l1fga)).
      By definition of fmap_FG, it is fmap_F(fmap_G(Left.apply)); so, put this into the left-hand side:
      
        seq_L1[F](l1fga) map_F (l1ga ⇒ l1ga.seq) map_F(gl1a ⇒ gl1a.map_G(Left.apply))
        = seq_L1[F](l1fga) map_F (l1ga ⇒ l1ga.seq map_G Left.apply)
      
      This is exactly our left-hand side of the composition law computed previously.
      The code is symmetric with respect to L1 or L2.
      Therefore the composition law holds for L.
     */
  }

  it should "derive traversable for recursive functor" in {
    def withParam[S[_, _] : Bifunctor : Bitraverse](): Unit = {
      final case class L[A](s: S[A, L[A]])

      implicit val functorL: Functor[L] = new Functor[L] {
        override def map[A, B](fa: L[A])(f: A ⇒ B): L[B] =
          L(fa.s.bimap(f, map(_)(f)))
      }

      implicit val travL: Trav[L] = new Trav[L] {
        override def seq[F[_] : WuZip : Functor, A](lfa: L[F[A]]): F[L[A]] = {
          // Adapt to cats.Applicative, so that we can use `bisequence`.
          implicit val applicativeF: Applicative[F] = WuZip.toCatsApplicative[F]
          // First, convert S[F[A], L[F[A]]] into S[F[A], F[L[A]]].
          val f: S[F[A], F[L[A]]] = lfa.s.bimap(identity[F[A]], { xlfa: L[F[A]] ⇒ seq[F, A](xlfa) })
          val g: F[S[A, L[A]]] = f.bisequence
          // Convert to F[L[A]] by wrapping in the case class L.
          g.map(L.apply)
        }
      }
    }

    // Check laws:

    // Identity law: fmap_L(F.pure[A]) ◦ seq = F.pure[L[A]]
    /*
      Substitute the code of fmap_L and apply to some s: S[A, L[A]].
       
      fmap_L(F.pure)(s) = s.bimap(F.pure, fmap_L(F.pure)): L[F[A]]
      
      Then apply seq to this and get (omitting the wrapping and unwrapping in the case class L)
      
      s.bimap(F.pure, fmap_L(F.pure)).bimap(id, seq)
        = s.bimap(F.pure, fmap_L(F.pure) andThen seq).biseq
        
      By the inductive assumption we already have map_L(F.pure) andThen seq = F.pure in the second argument of bimap.
      So we have s.bimap(F.pure, F.pure).biseq and we assume that
      the identity law holds for S[_, _], so this becomes F.pure(s),
      i.e. the same as the right-hand side of the identity law.
     */

    // Composition law: seq_L[F] ◦ fmap_F(seq_L[G]) = seq_L[FG]
    /*
      Apply both sides to some s: S[F[G[A]], L[F[G[A]]]]. We get
            
      sfga.bimap(id, seq_L[F]).biseq[F].map_F(sga ⇒ sga.bimap(id, seq_L[G]).biseq[G])) ?=? sfga.bimap(id, seq_L[FG]).biseq[FG]   (*)
      
      We assume that the composition law already holds for the bitraversable bifunctor S,
      so with any sfgxy : S[F[G[X]], F[G[Y]]],
      
        sfgxy.biseq[F].map_F(biseq[G]) = sfgxy.biseq[FG]
      
      To get a value of the type S[F[G[X]], F[G[Y]]] out of s: S[F[G[A]], L[F[G[A]]]], we need
      to use some bimap and also use seq_L recursively with respect to the second type argument of S.  
      
      So, substitute sfgxy := sfga.bimap(id, lfga ⇒ lfga.seq_L[F].map_F(seq_L[G])) with X = A and Y = L[A], and get
            
      sfga.bimap(id, lfga ⇒ lfga.seq_L[F].map_F(seq_L[G])).biseq[F].map_F(biseq[G]) = sfga.bimap(id, lfga ⇒ lfga.seq_L[F].map_F(seq_L[G])).biseq[FG]
      
      We may assume, by induction, that the composition law for seq_L already holds
      with respect to the second type argument of S. So we can rewrite the r.h.s. of the above equation,

      sfga.bimap(id, lfga ⇒ lfga.seq_L[F].map_F(seq_L[G])).biseq[F].map_F(biseq[G]) = sfga.bimap(id, seq_L[FG]).biseq[FG]
      
      The r.h.s. is the same as the r.h.s. of eq. (*) above.
      So, to demontrate eq.(*), it remains to show that
      
      sfga.bimap(id, seq_L[F]).biseq[F].map_F(sga ⇒ sga.bimap(id, seq_L[G]).biseq[G]) ?=? sfga.bimap(id, lfga ⇒ lfga.seq_L[F].map_F(seq_L[G])).biseq[F].map_F(biseq[G])
      
      We can omit the trailing .map_F(biseq[G]) from both sides of the equation:

      sfga.bimap(id, seq_L[F]).biseq[F].map_F(bimap(id, seq_L[G])) ?=? sfga.bimap(id, seq_L[F] ◦ fmap_F(seq_L[G])).biseq[F]
      
      We now need to interchange the order of bimap and biseq; for this,
      we need to use the naturality of biseq: S[F[X], F[Y]] ⇒ F[S[X, Y]] as
      
      sfxy.biseq.map_F(bimap(f, g)) = sfxy.bimap(fmap_F(f), fmap_F(g)).biseq
      
      This gives
      
      sfga.bimap(id, seq_L[F]).biseq[F].map_F(bimap(id, seq_L[G]))
        = sfga.bimap(id, seq_L[F]).bimap(id, fmap_F(seq_L[G])).biseq[F]
        = sfga.bimap(id, seq_L[F] ◦ fmap_F(seq_L[G])).biseq[F]
      
      Now both parts of the equation are the same.
      
      Therefore the composition law holds for L.
     */
  }

}
