package example

import cats.kernel.Semigroup
import cats.{FlatMap, Functor, Monoid, derive}
import cats.syntax.functor._
import cats.syntax.semigroup._
import io.chymyst.ch._
import org.scalatest.{FlatSpec, Matchers}
import org.scalacheck.ScalacheckShapeless._
import org.scalactic.Equality
import Utils.time
import cats.syntax.flatMap

import scala.collection.immutable
import scala.concurrent.duration.{Duration, DurationConversions}
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Try}

class Chapter07_01_examplesSpec extends FlatSpec with Matchers {

  behavior of "initial examples"

  it should "compute some sequences with functor block" in {
    val result: Seq[String] = for {
      i ← 1 to 5
      j ← i to 5
    } yield {
      val product = i * j
      s"$i * $j = $product"
    }

    result shouldEqual multTable
  }

  lazy val multTable = Seq(
    "1 * 1 = 1", "1 * 2 = 2", "1 * 3 = 3", "1 * 4 = 4", "1 * 5 = 5",
    "2 * 2 = 4", "2 * 3 = 6", "2 * 4 = 8", "2 * 5 = 10",
    "3 * 3 = 9", "3 * 4 = 12", "3 * 5 = 15",
    "4 * 4 = 16", "4 * 5 = 20",
    "5 * 5 = 25"
  )

  it should "compute sequences with functor block using filter" in {
    val result: Seq[String] = for {
      i ← 1 to 5
      j ← 1 to 5
      if j >= i
      product = i * j
    } yield s"$i * $j = $product"

    result shouldEqual multTable
  }

  it should "generate empty sequence when one of the containers is empty" in {
    val result: Seq[Int] = for {
      x ← 1 to 100
      y ← Seq()
    } yield x

    result shouldEqual Seq()
  }

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

    val expected = xs.permutations.toSeq // Standard library function.
    permutations shouldEqual expected
  }

  it should "2. Compute all subsets of a set of 3" in {

    // Organize the selection by hand.
    val subsets: Set[Set[String]] = for {
      xa ← Set(Set[String](), Set("a"))
      xb ← Set(Set[String](), Set("b"))
      xc ← Set(Set[String](), Set("c"))
    } yield {
      xa ++ xb ++ xc
    }

    val xs = Set("a", "b", "c")
    val expected = xs.subsets.toSet // Standard library function.
    subsets shouldEqual expected
  }

  it should "3. Compute all subsequences of length 3 out of given sequence" in {
    val givenSequence = 1 to 5

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

    val expected = Seq(Seq(1, 2, 3), Seq(1, 2, 4), Seq(1, 2, 5), Seq(1, 3, 4), Seq(1, 3, 5), Seq(1, 4, 5), Seq(2, 3, 4), Seq(2, 3, 5), Seq(2, 4, 5), Seq(3, 4, 5))
    subsequences.toSeq shouldEqual expected
  }

  it should "4a. Generalize example 1" in {

    def permutations[A](xs: Seq[A]): Seq[Seq[A]] = {
      if (xs.isEmpty)
        Seq(Seq())
      else for {
        x ← xs
        remain1 = xs diff Seq(x)
        ys ← permutations(remain1) // Recursive call.
      } yield {
        Seq(x) ++ ys
      }
    }

    val xs = Seq("a", "b", "c", "d", "e")

    val expected = xs.permutations.toSeq
    permutations(xs) shouldEqual expected
  }

  it should "4b. Generalize example 2" in {

    // Choose some element x from the set. Compute all subsets that contain x, and then compute all subsets that do not.
    def subsets[A](xs: Set[A]): Set[Set[A]] = xs.headOption match {
      case None ⇒ Set(Set())
      case Some(x) ⇒ for {
        xa ← Set(Set[A](), Set(x))
        remain = xs - x
        yas ← subsets(remain) // Recursive call.
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
        Seq(Seq()) // Check this first because n == 0 means "we are done" even if xs is empty.
      else if (xs.isEmpty)
        Seq()
      else
        (for {
          xs ← xs.tails // 1 to n, 2 to n, etc.
          if xs.nonEmpty // The last element of `.tails` is an empty list.
          x = xs.head // xs is non-empty here
          remain = xs.tail
          yys ← subsequences(remain, n - 1) // Recursive call.
        } yield {
          Seq(x) ++ yys
        }).toSeq
    }

    val xs = 1 to 5
    val expected = Seq(Seq(1, 2, 3), Seq(1, 2, 4), Seq(1, 2, 5), Seq(1, 3, 4), Seq(1, 3, 5), Seq(1, 4, 5), Seq(2, 3, 4), Seq(2, 3, 5), Seq(2, 4, 5), Seq(3, 4, 5))
    subsequences(xs, 3) shouldEqual expected
  }

  // Helper function for the n-queens problem.
  def noThreat(prev: Int*)(otherX: Int): Boolean = {
    val otherY = prev.length
    prev.zipWithIndex.forall { case (x, y) ⇒
      x != otherX && x - y != otherX - otherY && x + y != otherX + otherY
    }
  }

  it should "5. Find all solutions to the 8 queens problem" in {
    val row = 0 until 8

    val solutions: Seq[Seq[Int]] = for {
      x1 ← row
      x2 ← row
      if noThreat(x1)(x2) // queen 2 does not threaten queen 1
      x3 ← row
      if noThreat(x1, x2)(x3) // queen 3 does not threaten previous queens
      x4 ← row
      if noThreat(x1, x2, x3)(x4)
      x5 ← row
      if noThreat(x1, x2, x3, x4)(x5)
      x6 ← row
      if noThreat(x1, x2, x3, x4, x5)(x6)
      x7 ← row
      if noThreat(x1, x2, x3, x4, x5, x6)(x7)
      x8 ← row
      if noThreat(x1, x2, x3, x4, x5, x6, x7)(x8)
    } yield
      Seq(x1, x2, x3, x4, x5, x6, x7, x8)

    solutions.length shouldEqual 92
  }

  it should "6. Find all solutions to the n-queens problem" in {

    def nQueens(n: Int): Seq[Seq[Int]] = {
      val row = 0 until n

      def nQueensPartial(m: Int, prev: Seq[Int]): Seq[Seq[Int]] = if (m == 0) Seq(Seq()) else for {
        x ← row
        if noThreat(prev: _*)(x)
        newQueens = prev :+ x
        rest ← nQueensPartial(m - 1, newQueens) // Recursive call.
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

    /*
    Before writing a full implementation, let us try implementing the dnf2cnf conversion for an example.
    (a && b) || (c && d)  ---->   (a || c) && (a || d) && (b || c) && (b || d)

    The code would be like this:
    for {
       x ← Set('a, 'b) // first clause
       y ← Set('c, 'd) // second clause
    } yield Set(x) ++ Set(y)
     */

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

  it should "demonstrate linear algebra manipulations" in {

    // Compute matrix transpose.
    def transpose[A](xss: Seq[Seq[A]]): Seq[Seq[A]] = for {
      i ← xss.head.indices
    } yield {
      for {
        xs ← xss
      } yield xs(i)
    }

    transpose(Seq(
      Seq(1, 2),
      Seq(3, 4),
      Seq(5, 6)
    )) shouldEqual Seq(
      Seq(1, 3, 5),
      Seq(2, 4, 6)
    )

    import scala.math.Numeric.Implicits._

    // Compute scalar product of two vectors.
    def scalprod[N: Numeric](xs: Seq[N], ys: Seq[N]) = {
      (for {
        (x, y) ← xs zip ys
      } yield x * y
        ).sum
    }

    scalprod(Seq(1, 2, 3), Seq(3, 2, 1)) shouldEqual 10

    // Compute matrix product.
    def matmul[N: Numeric](xss: Seq[Seq[N]], yss: Seq[Seq[N]]) = {
      for (xs ← xss) yield
        for (yst ← transpose(yss)) yield
          scalprod(xs, yst)
    }

    val mat = Seq(
      Seq(1, 1),
      Seq(-1, 1)
    )
    matmul(mat, mat) shouldEqual Seq(
      Seq(0, 2),
      Seq(-2, 0)
    )
  }

  behavior of "worked examples: pass/fail monads"

  it should "1. Read values of Java properties, checking that they all exist" in {
    System.setProperty("client 1", "corporation 1")
    System.setProperty("client 2", "corporation 2")
    System.setProperty("corporation 1", "orders 1")
    System.setProperty("corporation 2", "orders 2")
    System.setProperty("orders 1", "123")
    System.setProperty("orders 2", "456")

    def getOrders(client: String): Option[Int] = for {
      corporation ← Option(System.getProperty(client))
      orders ← Option(System.getProperty(corporation))
      stringValue ← Option(System.getProperty(orders))
      intValue ← Try(stringValue.toInt).toOption
    } yield intValue

    getOrders("client 1") shouldEqual Some(123)
    getOrders("client 2") shouldEqual Some(456)
    getOrders("client 3") shouldEqual None
  }

  it should "2. Obtain values from Future computations in sequence" in {

    val n = 2000
    import scala.concurrent.ExecutionContext.Implicits.global

    def longComputation(c: Double): Future[Double] = Future {
      (for {
        i ← 1 to n
        j ← 1 to n
        x = math.cos(i * math.cos(i + math.cos(j * math.cos(j + c)))) // whatever
      } yield x).sum
    }

    // Compute several of these in sequence.
    val (r1, t1) = time {
      val result1 = for {
        x ← longComputation(1.0)
        y ← longComputation(x)
        z ← longComputation(y)
      } yield y
      Await.result(result1, Duration.Inf)
    }

    println(s"Computing $n iterations with sequential futures yields $r1 in $t1 seconds")

    // Similar computation where we can start all the futures in parallel.
    val (r2, t2) = time {
      val c1 = longComputation(1.0)
      val c2 = longComputation(2.0)
      val c3 = longComputation(3.0)

      val result2 = for {
        x ← c1
        y ← c2
        z ← c3
      } yield y
      Await.result(result2, Duration.Inf)
    }

    println(s"Computing $n iterations with parallel futures yields $r2 in $t2 seconds")

    /*
Computing 2000 iterations with sequential futures yields 1483.4944693546663 in 6.207532281 seconds
Computing 2000 iterations with parallel futures yields 2910.7779073064853 in 2.774459626 seconds
     */
  }

  it should "3. Make arithmetic safe by returning error messages in Either" in {
    type SafeDouble = Either[String, Double]

    implicit def safeOp(x: Double): SafeDouble = Right(x)

    def safeDivide(x: Double, y: Double): SafeDouble = if (y == 0.0) Left(s"Error: dividing $x by 0") else Right(x / y)

    safeDivide(1.0, 0.0) shouldEqual Left(s"Error: dividing 1.0 by 0")

    val result: SafeDouble = for {
      x ← 1.0 // automatic conversion
      y ← safeDivide(x, 1.0)
      z ← x + y // automatic conversion
    } yield z

    result shouldEqual Right(2.0)

    val badResult: SafeDouble = for {
      x ← 1.0
      y ← safeDivide(x, 0.0)
      z ← x + y // automatic conversion
    } yield z

    badResult shouldEqual Left(s"Error: dividing 1.0 by 0")
  }

  it should "4. Fail less: chain computations that may throw an exception" in {
    // Computations f1, f2, ... may throw exceptions.
    def f1(x: Int): Int = 2 / x

    def f2(x: Int): Int = 2 - x

    def f3(x: Int): Int = 1 / x

    // The ordinary for/yield chain will require all of them to pass.
    val result = for {
      x ← Try(f1(1))
      y ← Try(f2(x))
      z ← Try(f3(y))
    } yield z

    result match {
      case Failure(t) ⇒ t should have message "/ by zero"
    }
  }

  behavior of "worked examples: tree-like monads"

  it should "1. Implement a tree of String properties with arbitrary branching" in {
    // Need to introduce a type parameter.

    sealed trait PropTree[A] {
      // For simplicity, implement map and flatMap directly in the trait.
      def map[B](f: A ⇒ B): PropTree[B]

      def flatMap[B](f: A ⇒ PropTree[B]): PropTree[B]
    }

    final case class Leaf[A](value: A) extends PropTree[A] {
      override def map[B](f: A ⇒ B): PropTree[B] = Leaf(f(value))

      override def flatMap[B](f: A ⇒ PropTree[B]): PropTree[B] = f(value)
    }

    final case class Fork[A](name: String, trees: Seq[PropTree[A]]) extends PropTree[A] {
      override def map[B](f: A ⇒ B): PropTree[B] = Fork(name, trees.map(_.map(f)))

      override def flatMap[B](f: A ⇒ PropTree[B]): PropTree[B] = Fork(name, trees.map(_.flatMap(f)))
    }

    val tree: PropTree[Int] = Fork("a1", Seq(Leaf(1), Leaf(2), Fork("a2", Seq(Leaf(3)))))

    tree.map(_ + 10) shouldEqual Fork("a1", Seq(
      Leaf(11), Leaf(12), Fork("a2", Seq(
        Leaf(13)
      ))
    ))

    val result = for {
      x ← tree // `x` goes over leaf values
      y ← if (x > 1) Leaf(x) else Fork("small", Seq(Leaf(x)))
    } yield y

    val expectedResult = Fork("a1", Seq(
      Fork("small", Seq(
        Leaf(1)
      )), Leaf(2), Fork("a2", Seq(
        Leaf(3)
      ))
    ))

    result shouldEqual expectedResult

    // Same computation using `flatMap` explicitly.
    tree.flatMap { x ⇒ if (x > 1) Leaf(x) else Fork("small", Seq(Leaf(x))) } shouldEqual expectedResult
  }

  // Language supports constants, variables, and multiplication.
  // The type parameter A describes variables (values are `Int`), so flatMap performs variable substitution.
  // Known scalac issue: Cannot define trait/case class hierarchy within a function scope. See https://issues.scala-lang.org/browse/SI-5252
  sealed trait Term[A] {
    def map[B](f: A ⇒ B): Term[B]

    def flatMap[B](f: A ⇒ Term[B]): Term[B]

    def *(y: Term[A]): Term[A] = Mult(this, y)
  }

  final case class Const[A](value: Int) extends Term[A] {
    override def map[B](f: A ⇒ B): Term[B] = Const(value)

    override def flatMap[B](f: A ⇒ Term[B]): Term[B] = Const(value)
  }

  final case class Var[A](name: A) extends Term[A] {
    override def map[B](f: A ⇒ B): Term[B] = Var(f(name))

    override def flatMap[B](f: A ⇒ Term[B]): Term[B] = f(name)
  }

  final case class Mult[A](x: Term[A], y: Term[A]) extends Term[A] {
    override def map[B](f: A ⇒ B): Term[B] = Mult(x map f, y map f)

    override def flatMap[B](f: A ⇒ Term[B]): Term[B] = Mult(x flatMap f, y flatMap f)
  }

  it should "2. Implement variable substitution for a simple arithmetic language" in {

    val expr: Term[String] = Const(123) * Var("a") * Const(456) * Var("b")

    // Modify variable names.
    expr.map(_ + "x") shouldEqual Const(123) * Var("ax") * Const(456) * Var("bx")

    // Substitute variables.
    val aSubst: Term[String] = Const(10)
    val bSubst: Term[String] = Const(20) * Var("c")

    val result = for {
      x ← expr // `x` goes over variable names in the expression
      y ← if (x == "a") aSubst else if (x == "b") bSubst else throw new Exception(s"invalid variable name $x")
    } yield y

    result shouldEqual Const(123) * aSubst * Const(456) * bSubst

    // Same computation using `flatMap` explicitly.
    expr.flatMap { x ⇒ if (x == "a") aSubst else bSubst } shouldEqual result
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
