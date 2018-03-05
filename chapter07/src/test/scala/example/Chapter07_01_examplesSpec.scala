package example

import cats.kernel.Semigroup
import cats.{Functor, Monoid, derive}
import cats.syntax.functor._
import cats.syntax.semigroup._
import io.chymyst.ch._
import org.scalatest.FlatSpec
import org.scalacheck.ScalacheckShapeless._
import org.scalactic.Equality

import scala.collection.immutable

class Chapter07_01_examplesSpec extends FlatSpec with FlattenableLawChecking with CatsLawChecking {

  behavior of "worked examples: list-like monads"

  it should "1. Compute all permutations of a sequence of 3" in {
    val xs = Seq("a", "b", "c")
    val permutations = for {
      x ← xs
      remain1 = xs diff Seq(x)
      y ← remain1
      remain2 = remain1 diff Seq(y)
      z ← remain2
    } yield {
      Seq(x, y, z)
    }
    val expected = xs.permutations.toSeq
    permutations shouldEqual expected
  }

  it should "2. Compute all subsets of a set of 3" in {
    val xs = Set("a", "b", "c")
    val expected = xs.subsets.toSet

    // Organize the selection by hand.
    val subsets: Set[Set[String]] = for {
      xa ← Set(Set[String](), Set("a"))
      xb ← Set(Set[String](), Set("b"))
      xc ← Set(Set[String](), Set("c"))
    } yield {
      xa ++ xb ++ xc
    }
    subsets shouldEqual expected
  }

  it should "3. Compute all subsequences of length 3 out of given sequence" in {
    val givenSequence = 1 to 5
    val expected = Seq(Seq(1, 2, 3), Seq(1, 2, 4), Seq(1, 2, 5), Seq(1, 3, 4), Seq(1, 3, 5), Seq(1, 4, 5), Seq(2, 3, 4), Seq(2, 3, 5), Seq(2, 4, 5), Seq(3, 4, 5))

    val subsequences = for {
      xs ← givenSequence.tails // 1 to 5, 2 to 5, 3 to 5, 4 to 5, 5 to 5, Nil
      if xs.nonEmpty
      x = xs.head // xs is non-empty here
      remain1 = xs.tail
      ys ← remain1.tails
      if ys.nonEmpty
      y = ys.head
      remain2 = ys.tail
      zs ← remain2.tails
      if zs.nonEmpty
      z = zs.head
    } yield {
      Seq(x, y, z)
    }
    subsequences.toSeq shouldEqual expected
  }

  it should "4a. Generalize example 1" in {
    def permutations[A](xs: Seq[A]): Seq[Seq[A]] = if (xs.isEmpty) Seq(Seq()) else for {
      x ← xs
      remain1 = xs diff Seq(x)
      ys ← permutations(remain1)
    } yield {
      Seq(x) ++ ys
    }

    val xs = Seq("a", "b", "c", "d", "e")

    val expected = xs.permutations.toSeq
    permutations(xs) shouldEqual expected
  }

  it should "4b. Generalize example 2" in {

    // Organize the selection by hand.
    def subsets[A](xs: Set[A]): Set[Set[A]] = xs.headOption match {
      case None ⇒ Set(Set())
      case Some(x) ⇒ for {
        xa ← Set(Set[A](), Set(x))
        remain = xs - x
        yas ← subsets(remain)
      } yield {
        xa ++ yas
      }
    }

    val xs = Set("a", "b", "c", "d", "e")
    val expected = xs.subsets.toSet
    subsets(xs) shouldEqual expected
  }

  it should "4c. Generalize example 3" in {

    def subsequences[A](xs: Seq[A], n: Int): Seq[Seq[A]] = {
      if (n == 0)
        Seq(Seq())
      else if (xs.isEmpty)
        Seq()
      else
        (for {
          xs ← xs.tails // 1 to n, 2 to n, etc.
          if xs.nonEmpty
          x = xs.head // xs is non-empty here
          remain = xs.tail
          yys ← subsequences(remain, n - 1)
        } yield {
          Seq(x) ++ yys
        }).toSeq
    }

    val xs = 1 to 5
    val expected = Seq(Seq(1, 2, 3), Seq(1, 2, 4), Seq(1, 2, 5), Seq(1, 3, 4), Seq(1, 3, 5), Seq(1, 4, 5), Seq(2, 3, 4), Seq(2, 3, 5), Seq(2, 4, 5), Seq(3, 4, 5))
    subsequences(xs, 3) shouldEqual expected
  }

  // Helper function for the n-queens problem.
  def noThreat(prev: Int*): Int ⇒ Boolean = { otherX ⇒
    val otherY = prev.length
    prev.zipWithIndex.forall { case (x, y) ⇒
      x != otherX && x - y != otherX - otherY && x + y != otherX + otherY
    }
  }

  it should "5. Find all solutions to the 8 queens problem" in {
    val row = 0 until 8

    val solutions: Seq[Seq[Int]] = for {
      x1 ← row
      x2 ← row.filter(noThreat(x1))
      x3 ← row.filter(noThreat(x1, x2))
      x4 ← row.filter(noThreat(x1, x2, x3))
      x5 ← row.filter(noThreat(x1, x2, x3, x4))
      x6 ← row.filter(noThreat(x1, x2, x3, x4, x5))
      x7 ← row.filter(noThreat(x1, x2, x3, x4, x5, x6))
      x8 ← row.filter(noThreat(x1, x2, x3, x4, x5, x6, x7))
    } yield Seq(x1, x2, x3, x4, x5, x6, x7, x8)

    solutions.length shouldEqual 92
  }

  it should "6. Find all solutions to the n-queens problem" in {

    def nQueens(n: Int): Seq[Seq[Int]] = {
      val row = 0 until n

      def nQueensPartial(m: Int, prev: Seq[Int]): Seq[Seq[Int]] = if (m == 0) Seq(Seq()) else for {
        x ← row.filter(noThreat(prev: _*))
        newQueens = prev :+ x
        rest ← nQueensPartial(m - 1, newQueens)
      } yield Seq(x) ++ rest

      nQueensPartial(n, Seq())
    }

    // See https://math.stackexchange.com/questions/1872444/how-many-solutions-are-there-to-an-n-by-n-queens-problem
    Seq(8, 9, 10, 11).map(nQueens(_).length) shouldEqual Seq(92, 352, 724, 2680)
  }

  it should "7. Transform Boolean formulas between CNF and DNF" in {
    /**
      * A Boolean formula in CNF is represented by a set of sets of an arbitrary type `T`.
      * Values of `T` represent elementary Boolean variables or terms.
      * For instance, the CNF Boolean formula (a || b) && (c || d || e) is represented by
      * {{{ Set( Set(a, b), Set(c, d, e) ) }}}
      *
      * The value True is represented by the empty conjunction, `Set()`. The value False is represented by the empty disjunction, `Set(Set())`.
      *
      * A Boolean formula in DNF is represented by a set of sets of an arbitrary type `T`.
      * Values of `T` represent elementary Boolean variables or terms.
      * For instance, the DNF Boolean formula (a && b) || (c && d && e) is represented by
      * {{{ Set( Set(a, b), Set(c, d, e) ) }}}
      *
      * The value True is represented by the empty conjunction, `Set(Set())`. The value False is represented by the empty disjunction, `Set()`.
      *
      * Any Boolean formula can be converted to CNF or to DNF as desired. These functions will convert from DNF to CNF.
      */

    final case class CNF[A](v: Set[Set[A]])
    final case class DNF[A](v: Set[Set[A]])

    implicit def equalityCNF[A]: Equality[CNF[A]] = { (x: CNF[A], y: Any) ⇒ x == y }

    def cnfTrue[A] = CNF[A](Set())

    def cnfFalse[A] = CNF[A](Set(Set()))

    def dnfTrue[A] = DNF[A](Set(Set()))

    def dnfFalse[A] = DNF[A](Set())

    def dnf2cnf[A](dnf: DNF[A]): CNF[A] = dnf.v.headOption match {
      case None ⇒ CNF(Set(Set())) // False
      case Some(firstClause) ⇒ // firstClause is a && b
        cnfSimplify(CNF(
          for {
            x ← firstClause // x goes over terms in the first clause.
            ys ← dnf2cnf(DNF(dnf.v.tail)).v // ys are all other terms converted to CNF.
            // Now we have x || ( (c || d || e) && ... the rest of the CNF )
          } yield Set(x) ++ ys
        ))
    }

    // Simplify clauses: (a || b) && (a || b || c || ...) is the same as (a || b).
    def cnfSimplify[A](cnf: CNF[A]): CNF[A] = CNF(
      cnf.v.toList.sortBy(_.size).foldLeft(Set[Set[A]]()) { case (prev, clause) ⇒
        if (prev.exists(_ subsetOf clause)) prev else prev + clause
      }
    )

    // Tests.
    dnf2cnf(dnfTrue) shouldEqual cnfTrue
    dnf2cnf(dnfFalse) shouldEqual cnfFalse

    cnfSimplify(CNF(Set(Set('a, 'b), Set('a, 'b, 'c, 'd)))) shouldEqual CNF(Set(Set('a, 'b)))

    val dnf1 = DNF(Set(Set('a, 'b), Set('c, 'd, 'e)))
    val cnf1 = CNF(Set(Set('a, 'c), Set('a, 'd), Set('a, 'e), Set('b, 'c), Set('b, 'd), Set('b, 'e)))
    dnf2cnf(dnf1) shouldEqual cnf1

    // dnf2cnf is equal to its own inverse.
    dnf2cnf(DNF(cnf1.v)) shouldEqual CNF(dnf1.v)
  }

  /*
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
  */
}
