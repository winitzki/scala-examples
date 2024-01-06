package example

import cats.implicits.catsSyntaxSemigroup
import cats.{Bifunctor, Monoid}
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Try

class Chapter10_Church_encoding_Spec extends FlatSpec with Matchers {

  trait Foldable2[F[_, _]] {
    def reduce[M: Monoid, A](f: A => M)(fam: F[A, M]): M
  }

  val intMonoidMax: Monoid[Int] = new Monoid[Int] {

    override def empty: Int = 0

    override def combine(x: Int, y: Int): Int = math.max(x, y)
  }

  // A rank-1 Church encoding for type constructors: `Ch1[F, A]` is the fixpoint `µR. F[A, R]`.
  abstract class Ch1[F[_, _] : Bifunctor : Foldable2, A] {
    final def rmap[B, C](f: B => C): F[A, B] => F[A, C] = implicitly[Bifunctor[F]].rightFunctor.map(_)(f)

    // Standard catamorphism.
    def cata[R](alg: F[A, R] => R): R

    final def unfix: F[A, Ch1[F, A]] = cata[F[A, Ch1[F, A]]] {
      rmap(Ch1.fix[F, A])
    }

    final def depth: Int = cata[Int] { far => 1 + implicitly[Foldable2[F]].reduce[Int, A](_ => 0)(far)(intMonoidMax) }

    // Standard paramorphism.
    final def para[R](palg: F[A, (Ch1[F, A], R)] => R): R = cata[(Ch1[F, A], R)] { facr =>
      val c: Ch1[F, A] = Ch1.fix(rmap[(Ch1[F, A], R), Ch1[F, A]](_._1)(facr))
      val r: R = palg(facr)
      (c, r)
    }._2

    // Twisted paramorphism, basic version.
    final def twistedPara0[P, R](palg: F[A, P] => P, pralg: P => F[A, R] => R): R = {
      val baseP: P = cata[P](palg)
      val falg: F[A, R] => R = pralg(baseP)
      cata[R](falg)
    }

    // Twisted paramorphism, basic version, with extra P value.
    final def twistedPara0p[P, R](palg: F[A, P] => P, pralg: P => F[A, R] => R, baseP: P): R = {
      val falg: F[A, R] => R = pralg(baseP)
      cata[R](falg)
    }

    // Twisted paramorphism with 18 different choices of P parameterized by the number 0 <= choice < 18.
    // The only correct value is 11.
    final def twistedPara1[P, R](palg: F[A, P] => P, pralg: P => F[A, R] => R, baseP: P, choice: Int = 0): R = {
      val choice1: Boolean = choice % 2 == 0 // false
      val choice2: Boolean = (choice / 2) % 3 == 0 // false
      val choice3: Boolean = (choice / 2) % 3 == 1 // false
      val choice4: Boolean = (choice / 6) % 3 == 0 // false
      val choice5: Boolean = (choice / 6) % 3 == 1 // true
      //      val baseP: P = cata[P](palg) // If this is used instead of a given baseP, no choices ever work for zip.
      val falgpr: F[A, P => R] => P => R = { fapr =>
        oldP =>
          val choice1P: P = if (choice1) baseP else oldP
          val fap: F[A, P] = rmap[P => R, P](_ => choice1P)(fapr)
          val newP = palg(fap)
          val choice2P: P = if (choice2) baseP else if (choice3) oldP else newP
          val far: F[A, R] = rmap[P => R, R](f => f(choice2P))(fapr)
          val choice3P: P = if (choice4) baseP else if (choice5) oldP else newP
          pralg(choice3P)(far)
      }
      val pr: P => R = cata[P => R](falgpr)
      val r: R = pr(baseP)
      r
    }
  }

  object Ch1 {
    def fix[F[_, _] : Bifunctor : Foldable2, A](fa: F[A, Ch1[F, A]]): Ch1[F, A] = new Ch1[F, A] {
      override def cata[R](alg: F[A, R] => R): R = {
        val c2r: Ch1[F, A] => R = _.cata(alg)
        val fc2r: F[A, Ch1[F, A]] => F[A, R] = fac => implicitly[Bifunctor[F]].rightFunctor.map(fac)(c2r)
        alg(fc2r(fa))
      }
    }
  }

  type F[A, R] = Option[(A, R)]

  val bifunctorF: Bifunctor[F] = new Bifunctor[F] {
    override def bimap[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): F[C, D] = fab match {
      case Some((a, b)) => Some((f(a), g(b)))
      case None => None
    }
  }

  val foldable2: Foldable2[F] = new Foldable2[F] {
    override def reduce[M: Monoid, A](f: A => M)(fam: F[A, M]): M = fam match {
      case Some((a, m)) => f(a) |+| m
      case None => Monoid[M].empty
    }
  }

  trait Lst[A] {
    def cata[R](alg: F[A, R] => R): R

    override def toString: String = "[" + cata[String] {
      case None => ""
      case Some((a, prev)) => a.toString + (if (prev.nonEmpty) ", " + prev else prev)
    } + "]"

    def fold[R](init: R)(update: (A, R) => R): R = cata[R] {
      case None => init
      case Some((a, prev)) => update(a, prev)
    }

    def unfix: F[A, Lst[A]] = fold[F[A, Lst[A]]](None) { (a, falsta: F[A, Lst[A]]) => Some((a, Lst.fix(falsta))) }

    def headOption: Option[A] = unfix match {
      case Some((a, lsta)) => Some(a)
      case None => None
    }

    def safeTail: Lst[A] = unfix match {
      case Some((a, lsta)) => lsta
      case None => this
    }

    // Paramorphism: the function `palg` takes arguments that are the current value A, the current tail of the list, ad the current accumulator.
    def para[R](palg: F[A, (Lst[A], R)] => R): R = cata[(Lst[A], R)] { falstar: F[A, (Lst[A], R)] =>
      val c: Lst[A] = falstar match {
        case Some((a, (lsta, r))) => Lst.cons(a, lsta)
        case None => Lst.nil
      }
      val r: R = palg(falstar)
      (c, r)
    }._2

    def twistedPara[P, R](pr: P => R, pap: (P, A) => P, parr: (P, A, R) => R): P => R = cata[P => R] {
      case None => pr
      case Some((a, pr)) => p => parr(p, a, pr(pap(p, a)))
    }

    def concat(other: Lst[A]): Lst[A] = cata[Lst[A]] {
      case None => other
      case Some((a, lsta)) => Lst.cons(a, lsta)
    }

    def concat1(other: Lst[A]): Lst[A] = twistedPara[Lst[A], Lst[A]](
      identity,
      (p, _) => p,
      (_, a, r) => Lst.cons(a, r),
    )(other)

    def zip1[B](other: Lst[B]): Lst[(A, B)] = twistedPara[Lst[B], Lst[(A, B)]](
      _ => Lst.nil,
      (ys, _) => ys.safeTail,
      (ys, a, t) => ys.headOption match {
        case Some(b) => Lst.cons((a, b), t)
        case None => Lst.nil
      },
    )(other)
  }

  object Lst {
    def nil[A]: Lst[A] = new Lst[A] {
      override def cata[R](alg: F[A, R] => R): R = alg(None)
    }

    def cons[A](a: A, tail: Lst[A]): Lst[A] = new Lst[A] {
      override def cata[R](alg: F[A, R] => R): R = alg(Some((a, tail.cata(alg))))
    }

    def fix[A]: F[A, Lst[A]] => Lst[A] = { // "fix" or "build" is equivalent to the pair (nil, cons)
      case None => Lst.nil
      case Some((a, lsta)) => Lst.cons(a, lsta)
    }
  }

  it should "use Church-encoded list via Lst" in {
    val list123: Lst[Int] = Lst.cons(1, Lst.cons(2, Lst.cons(3, Lst.nil)))
    val nilInt: Lst[Int] = Lst.nil

    list123.toString shouldEqual "[1, 2, 3]"
    nilInt.toString shouldEqual "[]"

    list123.unfix.toString shouldEqual "Some((1,[2, 3]))"
    nilInt.unfix shouldEqual None

    Lst.fix(list123.unfix).toString shouldEqual list123.toString

    list123.headOption shouldEqual Some(1)
    nilInt.headOption shouldEqual None

    list123.safeTail.toString shouldEqual "[2, 3]"
    nilInt.safeTail.toString shouldEqual "[]"

    list123.concat(list123).toString shouldEqual "[1, 2, 3, 1, 2, 3]"
    list123.concat(nilInt).toString shouldEqual list123.toString
    nilInt.concat(nilInt).toString shouldEqual "[]"
    nilInt.concat(list123).toString shouldEqual list123.toString

    list123.concat1(list123).toString shouldEqual "[1, 2, 3, 1, 2, 3]"
    list123.concat1(nilInt).toString shouldEqual list123.toString
    nilInt.concat1(nilInt).toString shouldEqual "[]"
    nilInt.concat1(list123).toString shouldEqual list123.toString

    list123.zip1(list123).toString shouldEqual "[(1,1), (2,2), (3,3)]"
    list123.zip1(list123.safeTail).toString shouldEqual "[(1,2), (2,3)]"
    list123.safeTail.zip1(list123).toString shouldEqual "[(2,1), (3,2)]"

    list123.para[Seq[(Int, String)]] {
      case None => Seq()
      case Some((i, (b, r))) => (i, b.toString) +: r
    }.toString shouldEqual "List((1,[2, 3]), (2,[3]), (3,[]))"
  }

  object Lst1 {
    type Lst1[A] = Ch1[F, A]
    implicit val i1 = bifunctorF
    implicit val i2 = foldable2

    def nil[A]: Lst1[A] = new Lst1[A] {
      override def cata[R](alg: F[A, R] => R): R = alg(None)
    }

    def cons[A](a: A, tail: Lst1[A]): Lst1[A] = new Lst1[A] {
      override def cata[R](alg: F[A, R] => R): R = alg(Some((a, tail.cata(alg))))
    }

    implicit class Ops[A](lst1: Lst1[A]) {
      def toList: List[A] = lst1.cata[List[A]] {
        case None => Nil
        case Some((a, tail)) => a +: tail
      }

      def headOption: Option[A] = lst1.unfix match {
        case Some((head, tail)) => Some(head)
        case None => None
      }

      def safeTail: Lst1[A] = lst1.unfix match {
        case Some((head, tail)) => tail
        case None => lst1
      }

      def concat(other: Lst1[A]): Lst1[A] = lst1.cata[Lst1[A]] {
        case None => other
        case Some((a, lsta)) => Lst1.cons(a, lsta)
      }

      def concat0(other: Lst1[A]): Lst1[A] = lst1.twistedPara0[Lst1[A], Lst1[A]](
        {
          case None => other
          case Some((_, p)) => p
        },
        p => {
          case None => p
          case Some((a, r)) => Lst1.cons(a, r)
        }
      )

      def concat1(other: Lst1[A], choice: Int): Lst1[A] = lst1.twistedPara1[Lst1[A], Lst1[A]](
        {
          case None => other
          case Some((_, p)) => p
        },
        p => {
          case None => p
          case Some((a, r)) => Lst1.cons(a, r)
        },
        other,
        choice,
      )

      def zip0[B](other: Lst1[B]): Lst1[(A, B)] = lst1.twistedPara0[Lst1[B], Lst1[(A, B)]](
        {
          case None => other
          case Some((_, ys)) => ys.safeTail
        },
        p => {
          case None => Lst1.nil
          case Some((a, t)) => p.headOption match {
            case Some(b) => Lst1.cons((a, b), t)
            case None => Lst1.nil
          }
        }
      )

      def zip0p[B](other: Lst1[B]): Lst1[(A, B)] = lst1.twistedPara0p[Lst1[B], Lst1[(A, B)]](
        {
          case None => other
          case Some((_, ys)) => ys.safeTail
        },
        p => {
          case None => Lst1.nil
          case Some((a, t)) => p.headOption match {
            case Some(b) => Lst1.cons((a, b), t)
            case None => Lst1.nil
          }
        },
        other
      )

      def zip1[B](other: Lst1[B], choice: Int): Lst1[(A, B)] = lst1.twistedPara1[Lst1[B], Lst1[(A, B)]](
        {
          case None => other
          case Some((_, ys)) => ys.safeTail
        },
        p => {
          case None => Lst1.nil
          case Some((a, t)) => p.headOption match {
            case Some(b) => Lst1.cons((a, b), t)
            case None => Lst1.nil
          }
        },
        other,
        choice
      )

    }
  }

  it should "perform operations with Church-encoded list via Ch1" in {
    import Lst1._
    val list123: Lst1[Int] = cons(1, cons(2, cons(3, nil)))
    val nilInt: Lst1[Int] = nil

    list123.toList shouldEqual List(1, 2, 3)
    nilInt.toList shouldEqual Nil

    bifunctorF.rightFunctor.map(list123.unfix)(_.toList) shouldEqual Some((1, List(2, 3)))
    nilInt.unfix shouldEqual None

    Ch1.fix(list123.unfix).toList shouldEqual list123.toList

    list123.headOption shouldEqual Some(1)
    nilInt.headOption shouldEqual None

    list123.safeTail.toList shouldEqual List(2, 3)
    nilInt.safeTail.toList shouldEqual Nil

    list123.concat(list123).toList shouldEqual List(1, 2, 3, 1, 2, 3)
    list123.concat(nilInt).toList shouldEqual list123.toList
    nilInt.concat(nilInt).toList shouldEqual Nil
    nilInt.concat(list123).toList shouldEqual list123.toList

    list123.concat0(list123).toList shouldEqual List(1, 2, 3, 1, 2, 3)
    list123.concat0(nilInt).toList shouldEqual list123.toList
    nilInt.concat0(nilInt).toList shouldEqual Nil
    nilInt.concat0(list123).toList shouldEqual list123.toList
    val results = (0 to 17).map { i =>
      Try {
        list123.concat1(list123, i).toList shouldEqual List(1, 2, 3, 1, 2, 3)
        list123.concat1(nilInt, i).toList shouldEqual list123.toList
        nilInt.concat1(nilInt, i).toList shouldEqual Nil
        nilInt.concat1(list123, i).toList shouldEqual list123.toList
        s"parameter=$i"
      }
    }
    println(s"concat1 has ${results.count(_.isSuccess)} successes:\n" + results) // Success with any choice of parameter.

    list123.zip0(list123).toList shouldEqual Nil
    list123.zip0(list123.safeTail).toList shouldEqual Nil
    list123.safeTail.zip0(list123).toList shouldEqual List((2, 3), (3, 3))
    nilInt.zip0(list123).toList shouldEqual Nil
    list123.zip0(nilInt).toList shouldEqual Nil

    list123.zip0p(list123).toList shouldEqual List((1,1), (2,1), (3,1))
    list123.zip0p(list123.safeTail).toList shouldEqual List((1,2), (2,2), (3,2))
    list123.safeTail.zip0p(list123).toList shouldEqual List((2,1), (3,1))
    nilInt.zip0p(list123).toList shouldEqual Nil
    list123.zip0p(nilInt).toList shouldEqual Nil

    val results2 = (0 to 17).map { i =>
      Try {
        list123.zip1(list123, i).toList shouldEqual List((1, 1), (2, 2), (3, 3))
        list123.zip1(list123.safeTail, i).toList shouldEqual List((1, 2), (2, 3))
        list123.safeTail.zip1(list123, i).toList shouldEqual List((2, 1), (3, 2))
        nilInt.zip1(list123, i).toList shouldEqual Nil
        list123.zip1(nilInt, i).toList shouldEqual Nil
        s"parameter=$i"
      }
    }
    println(s"zip1 has ${results2.count(_.isSuccess)} successes:\n" + results2) // Success only with parameter = 11.

  }

}