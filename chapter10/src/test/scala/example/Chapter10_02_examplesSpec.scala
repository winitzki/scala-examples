package example

import org.scalatest.{FlatSpec, Matchers}

class Chapter10_02_examplesSpec extends FlatSpec with Matchers {

  behavior of "Church encoding constructions"

  it should "define non-recursive Church encoding of List[Int]" in {
    // Recursive encoding of P = List[Int].
    /*
    sealed trait ListInt
    case class End() extends ListInt 
    case class Link(x: Int, next: ListInt)
    */
    // Can't write `type CList = ∀A. (1 + Int × A ⇒ A) ⇒ A` in Scala.
    // Encode this as `∀A. CP[A] ⇒ A` where CP[A] = (1 + Int × A ⇒ A)
    // = (1 ⇒ A) × (Int × A ⇒ A) = A × (Int ⇒ A ⇒ A).
    final case class CP[A](end: A, link: Int ⇒ A ⇒ A)

    trait CList {
      def run[A]: CP[A] ⇒ A // Note the similarity with the type signature of `fold`.
    }

    // To define values of type CList, use helper functions.

    // Create an empty list.
    val empty: CList = new CList {
      def run[A]: CP[A] ⇒ A = {
        case CP(end, _) ⇒ end
      }
    }

    // Create a list with one element.
    def pure(x: Int): CList = new CList {
      def run[A]: CP[A] ⇒ A = {
        case CP(end, link) ⇒ link(x)(end)
      }
    }

    // Append an element to a list.
    def append(x: Int, cList: CList): CList = new CList {
      def run[A]: CP[A] ⇒ A = {
        case cp@CP(_, link) ⇒ link(x)(cList.run(cp))
      }
    }

    // Fold a list. The implementation is non-recursive.
    def fold[A](init: A)(update: Int ⇒ A ⇒ A)(cList: CList): A =
      cList.run(CP(end = init, link = update))

    // Convert to ordinary List[Int].
    def toListInt(cList: CList): List[Int] = fold(Nil: List[Int])(x ⇒ l ⇒ x :: l)(cList)

    // Some other operations that do not require pattern-matching can be implemented non-recursively.
    def map(cList: CList)(f: Int ⇒ Int): CList = new CList {
      override def run[A]: CP[A] ⇒ A = {
        case CP(end, link) ⇒ cList.run(CP(end, x ⇒ l ⇒ link(f(x))(l)))
      }
    }

    def headOption(cList: CList): Option[Int] = cList.run[Option[Int]](CP(end = None, link = x ⇒ _ ⇒ Some(x)))

    // Due to lack of pattern-matching, `tail` cannot be implemented efficiently. 

    // Testing.
    def sum(cList: CList): Int = fold[Int](0)(x ⇒ x + _)(cList)

    val list10 = pure(10)

    val list15 = append(5, pure(10))

    sum(empty) shouldEqual 0
    sum(list10) shouldEqual 10
    sum(append(10, empty)) shouldEqual 10
    sum(list15) shouldEqual 15

    headOption(empty) shouldEqual None
    headOption(list10) shouldEqual Some(10)

    toListInt(list15) shouldEqual List(5, 10)

    val list15a = map(list15)(_ + 1)
    sum(list15a) shouldEqual 17
    toListInt(list15a) shouldEqual List(6, 11)

    // Create a list of `length` elements, all equal to x. This is stack-safe.
    def fill(length: Int)(x: Int): CList = new CList {
      override def run[A]: CP[A] ⇒ A = {
        case CP(end, link) ⇒ // (link(x) compose link(x) compose ... compose link(x) ) (end)
          val l: A ⇒ A = link(x)
          (1 to length).foldLeft(end) { (b, _) ⇒ l(b) }
      }
    }

    val n = 1000000
    val bigList = fill(n)(1)
    sum(bigList) shouldEqual n

    // Appending many elements is not stack-safe.
    val badList = (1 to n).foldLeft(empty) { (x, _) ⇒ append(1, x) }
    the[StackOverflowError] thrownBy sum(badList) should have message null

  }

  it should "define Church encoding of Option[A]" in {

    // Direct polynomial encoding:
    // Option[A] = 1 + A
    // None or Some(x)

    // Since Option[_] is a type constructor, its Church encoding is
    // ∀P[_]. (∀X. Option[X] ⇒ P[X]) ⇒ P[A]  or equivalently
    // ∀P[_]. (∀X. 1 + X ⇒ P[X]) ⇒ P[A]

    // First, encode the universally quantified type `∀X. 1 + X ⇒ P[X]` or equivalently `∀X. P[X] × (X ⇒ P[X])`.
    trait ExOption[P[_]] {
      def none[X]: P[X]

      def some[X]: X ⇒ P[X]
    }

    // It is now easy to Church-encode Option[_].
    trait COption[A] {
      def run[P[_]]: ExOption[P] ⇒ P[A]
    }

    // Helper functions to create COption values.

    def some[A](x: A): COption[A] = new COption[A] {
      override def run[P[_]]: ExOption[P] ⇒ P[A] = _.some(x)
    }

    def none[A]: COption[A] = new COption[A] {
      override def run[P[_]]: ExOption[P] ⇒ P[A] = _.none
    }

    // Convert between Option and COption.
    def wrap[A](optA: Option[A]): COption[A] = new COption[A] {
      override def run[P[_]]: ExOption[P] ⇒ P[A] = optA match {
        case Some(x) ⇒ _.some(x)
        case None ⇒ _.none
      }
    }

    def unwrap[A](cOption: COption[A]): Option[A] = cOption.run(new ExOption[Option] {
      override def none[X]: Option[X] = None

      override def some[X]: X ⇒ Option[X] = Some.apply
    })

    val noneInt = none[Int]
    val someString = some("abc")

    // A function for pattern-matching on COption[A] does not work if implemented directly.
    /*    
        def inCase[A, R](cOption: COption[A])(default: R)(transform: A ⇒ R): R = {
          type C[X] = R
          cOption.run(new ExOption[C] {
            override def none[X]: C[X] = default
    
            override def some[X]: X ⇒ C[X] = transform // Type error: `transform` must have type X ⇒ R. 
          })
        }
    */

    // Natural transformations, e.g. Option ~> List, work:
    def toList[A](cOption: COption[A]): List[A] = cOption.run(new ExOption[List] {
      override def none[X]: List[X] = Nil

      override def some[X]: X ⇒ List[X] = x ⇒ List(x)
    })

    toList(noneInt) shouldEqual Nil
    toList(someString) shouldEqual List("abc")
  }

  it should "define non-recursive Church encoding of List[A]" in {
    // ∀P[_]. (∀B. (1 + B × P[B] ⇒ P[B]) ⇒ P[A]  =  ∀P[_]. (∀B. (P[B] × (B × P[B] ⇒ P[B])) ⇒ P[A]

    // First, encode `∀B. (P[B] × (B × P[B] ⇒ P[B])` as a product of `∀B. P[B]` and `∀B. B ⇒ P[B] ⇒ P[B]`.
    trait CL[P[_]] {
      def end[X]: P[X]

      def link[X](x: X): P[X] ⇒ P[X]
    }

    // Now, Church-encode List[A] as `∀P[_]. CL[P] ⇒ P[A]`.
    trait CList[A] {
      def run[P[_]](cl: CL[P]): P[A]
    }

    // Helper functions for creating values of CList[A].

    def empty[A]: CList[A] = new CList[A] {
      override def run[P[_]](cl: CL[P]): P[A] = cl.end
    }

    def pure[A](x: A): CList[A] = new CList[A] {
      override def run[P[_]](cl: CL[P]): P[A] = cl.link(x)(cl.end)
    }

    def append[A](x: A)(cList: CList[A]): CList[A] = new CList[A] {
      override def run[P[_]](cl: CL[P]): P[A] = cl.link(x)(cList.run(cl))
    }

    // Fold is non-recursive.

    def fold[A, B](init: B)(update: A ⇒ B ⇒ B)(cList: CList[A]): B = {
      // Helper type: constant functor.
      type C[X] = B

      cList.run(new CL[C] {
        override def end[X]: C[X] = init

        // We are cheating here! We know that this will be called only with values x of type A.
        override def link[X](x: X): C[X] ⇒ C[X] = update(x.asInstanceOf[A])
      })
    }

    def toList[A](cList: CList[A]): List[A] = fold[A, List[A]](Nil)(x ⇒ l ⇒ x :: l)(cList)

    def sum(cList: CList[Int]): Int = fold[Int, Int](0)(x ⇒ x + _)(cList)

    // Working with lists.

    val x0 = empty[Int]
    val x1 = pure(20)
    val x1a = append(20)(x0)
    val x2 = append(10)(x1)

    sum(x0) shouldEqual 0
    sum(x1) shouldEqual 20
    sum(x1a) shouldEqual 20
    sum(x2) shouldEqual 30

    toList(x0) shouldEqual Nil
    toList(x1) shouldEqual List(20)
    toList(x1a) shouldEqual List(20)
    toList(x2) shouldEqual List(10, 20)
  }

  it should "define Church encoding of free typeclass for any inductive typeclass" in {

  }

  it should "define tree encoding of free typeclass for any inductive typeclass" in {

  }

}
