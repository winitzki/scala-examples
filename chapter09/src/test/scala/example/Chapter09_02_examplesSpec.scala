package example

import cats.Functor
import org.scalatest.{FlatSpec, Matchers}
import cats.syntax.functor._
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

      // Identity law: fmapL(F.pure) ◦ seq = F.pure.
      // Since fmapL is the identity function, and seq = F.pure, the law is satisfied.

      // Composition law: seq[F] ◦ fmapF(seq[G]) = seq[FG] where we denote FG[A] = F[G[A]].
      // Since seq[F] = F.pure, we need F.pure andThen fmapF(G.pure) = (FG).pure
      // But the definition of pure for FG is exactly F.pure andThen fmapF(G.pure).
      // So the composition law holds.
    }
  }

  it should "define traversable for identity functor" in {
    type L[A] = A

    // Identity functor: Id[F[A]] ⇒ F[Id[A]] is the identity function.
    def seq[F[_] : WuZip : Functor, A](lfa: L[F[A]]): F[L[A]] = lfa

    // Check laws:

    // Identity law: fmapL(F.pure) ◦ seq = F.pure.
    // Since fmapL is the identity function, and seq = id, the law is satisfied.

    // Composition law: seq[F] ◦ fmapF(seq[G]) = seq[FG]
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

      // Identity law: fmapL(F.pure[A]) ◦ seq = F.pure[L[A]]
      /*
        Substitute the code of fmapL:
        fmapL(F.pure)((l1a, l2a)) = (fmapL(F.pure)(l1a), fmapL(F.pure)(l2a))  
        Then apply seq to this:
        
        fmapL(F.pure)(l1a).seq zip fmapL(F.pure)(l2a).seq
        
        Since the law holds for L1[_] and L2[_], we have fmapL(F.pure)(l1a).seq = F.pure[L1[A]]
        and similarly for L2. Therefore we have F.pure(l1a) zip F.pure(l2a).
        The identity laws of applicative now say that this equals F.pure( (l1a, l2a) ).
        This is the same as F.pure[L[A]].       
        
       */

      // Composition law: seq_L[F] ◦ fmapF(seq_L[G]) = seq_L[FG]
      /*
        Apply both sides to some (l1fga, l2fga): L[F[G[A]]].
        After applying seq_L[F], this becomes
        
          l1fga.seq[F] zip_F l2fga.seq[F] : F[ (L1[G[A]], L2[G[A]]) ].
        
        Now we need to apply fmapF(seq[G]) to that.
        
        seq_L[G] acts on some (l1ga, l2ga) and gives l1ga.seq zip_G l2ga.seq
        
        However, we need to lift this to act on F[L[G[A]]]. We use the definition of of zip_FG,
        
        fmap_F( (gx, gy) ⇒ gx zip_G gy ) (fgx zip_F fgy) = fgx zip_FG fgy  
        
        then use naturality of zip to transform gx and gy,
        
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

}
