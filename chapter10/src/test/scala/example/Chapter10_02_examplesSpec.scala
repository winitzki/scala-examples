package example

import cats.Functor
import cats.syntax.functor._
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
        case CP(end, link) ⇒ link(x)(end) // Link(x, Nil)
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
    def toListInt(cList: CList): List[Int] =
      fold(Nil: List[Int])(x ⇒ l ⇒ x :: l)(cList)

    // Some other operations that do not require pattern-matching can be implemented non-recursively.
    def map(cList: CList)(f: Int ⇒ Int): CList = new CList {
      def run[A]: CP[A] ⇒ A = {
        case CP(end, link) ⇒ cList.run(CP(end, x ⇒ l ⇒ link(f(x))(l)))
      }
    }

    def headOption(cList: CList): Option[Int] =
      cList.run[Option[Int]](CP(end = None, link = x ⇒ _ ⇒ Some(x)))

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
      def run[A]: CP[A] ⇒ A = {
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
      def run[P[_]]: ExOption[P] ⇒ P[A] = _.some(x)
    }

    def none[A]: COption[A] = new COption[A] {
      def run[P[_]]: ExOption[P] ⇒ P[A] = _.none
    }

    // Convert between Option and COption.
    def wrap[A](optA: Option[A]): COption[A] = new COption[A] {
      def run[P[_]]: ExOption[P] ⇒ P[A] = optA match {
        case Some(x) ⇒ _.some(x)
        case None ⇒ _.none
      }
    }

    def unwrap[A](cOption: COption[A]): Option[A] = cOption.run(new ExOption[Option] {
      def none[X]: Option[X] = None

      def some[X]: X ⇒ Option[X] = Some.apply
    })

    val noneInt = none[Int]
    val someString = some("abc")

    // A function for pattern-matching on COption[A] does not work if implemented directly.
    /*    
        def inCase[A, R](cOption: COption[A])(default: R)(transform: A ⇒ R): R = {
          type C[X] = R
          cOption.run(new ExOption[C] {
             def none[X]: C[X] = default
    
             def some[X]: X ⇒ C[X] = transform // Type error: `transform` must have type X ⇒ R. 
          })
        }
    */

    // Natural transformations, e.g. COption ~> List, work without trouble:
    def toList[A](cOption: COption[A]): List[A] = cOption.run(new ExOption[List] {
      def none[X]: List[X] = Nil

      def some[X]: X ⇒ List[X] = x ⇒ List(x)
    })

    toList(noneInt) shouldEqual Nil
    toList(someString) shouldEqual List("abc")
  }

  it should "define non-recursive Church encoding of List[A]" in {
    // ∀P[_]. (∀B. (1 + B × P[B] ⇒ P[B]) ⇒ P[A]
    // =  ∀P[_]. (∀B. (P[B] × (B × P[B] ⇒ P[B])) ⇒ P[A]

    // First, encode `∀B. (P[B] × (B × P[B] ⇒ P[B])` as a product of `∀B. P[B]` and `∀B. B ⇒ P[B] ⇒ P[B]`.
    trait CL[P[_]] {
      def end[B]: P[B]

      def link[B](x: B): P[B] ⇒ P[B]
    }

    // Now, Church-encode List[A] as `∀P[_]. CL[P] ⇒ P[A]`.
    trait CList[A] {
      def run[P[_]](cl: CL[P]): P[A]
    }

    // Helper functions for creating values of CList[A].

    def empty[A]: CList[A] = new CList[A] {
      def run[P[_]](cl: CL[P]): P[A] = cl.end
    }

    def pure[A](x: A): CList[A] = new CList[A] {
      def run[P[_]](cl: CL[P]): P[A] = cl.link(x)(cl.end)
    }

    def append[A](x: A)(cList: CList[A]): CList[A] = new CList[A] {
      def run[P[_]](cl: CL[P]): P[A] = cl.link(x)(cList.run(cl))
    }

    // Fold is non-recursive.

    def fold[A, B](init: B)(update: A ⇒ B ⇒ B)(cList: CList[A]): B = {
      // Helper type: constant functor.
      type C[X] = B

      cList.run(new CL[C] {
        def end[X]: C[X] = init

        // We are cheating here! We know that this will be called only with values x of type A.
        def link[X](x: X): C[X] ⇒ C[X] = update(x.asInstanceOf[A])
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

  it should "define the tree encoding of free instance for any inductive typeclass" in {
    // Consider an inductive typeclass C with methods (operations) C[X] ⇒ X.
    // Given the method functor C[_], we construct the free instance Free[C[_], Z] generated by type Z.
    // ("free instance of C over Z")

    // The type definition of tree-encoded Free[C, Z] is recursive:
    // Free[C, Z] ≡ Z + C[ Free[C, Z] ]
    sealed trait Free[C[_], Z]
    case class Wrap[C[_], Z](z: Z) extends Free[C, Z]
    case class Ops[C[_], Z](cf: C[Free[C, Z]]) extends Free[C, Z]

    // Prove that Free[C, Z] is an instance of typeclass C generated by Z.
    // This means we have a value of type C[X] ⇒ X for X = Free[C, Z] (methods of the typeclass)
    // and that we can inject a value of Z into Free[C, Z].
    def ops[C[_], Z](cf: C[Free[C, Z]]): Free[C, Z] = Ops(cf)

    def wrap[C[_], Z]: Z ⇒ Free[C, Z] = Wrap.apply

    // Interpret a Free[C, Z] into an instance P : C.
    def run[C[_] : Functor, P, Z](ex: Z ⇒ P)(implicit pMethodsC: C[P] ⇒ P): Free[C, Z] ⇒ P = {
      case Wrap(z) ⇒ ex(z)
      case Ops(cf) ⇒ pMethodsC(cf.map(run[C, P, Z](ex)))
    }

    // Example: semigroup; the single method is P × P ⇒ P. This is represented as SemiG[P] ⇒ P,
    // where SemiG is the methods functor for the semigroup:
    type SemiG[P] = (P, P)

    // Functor instance for SemiG.
    implicit val functorSemiG: Functor[SemiG] = cats.derive.functor[SemiG]

    // Free semigroup generated by String.
    type FreeSemigroupOverString = Free[SemiG, String]

    // Create a value of this type and perform some operations on it, using `wrap` and `ops` as helpers.
    val s1 = wrap[SemiG, String]("abc")
    val s2 = wrap[SemiG, String]("xyz")
    val s3 = ops[SemiG, String]((s1, s2))

    // Interpret `s3` into the standard semigroup of strings.
    implicit val semigString: SemiG[String] ⇒ String = {
      case (x, y) ⇒ x + y
    }

    val r3 = run[SemiG, String, String](identity).apply(s3)
    r3 shouldEqual "abcxyz"
  }

  it should "define the Church/tree encoding of free instance for any inductive typeclass" in {
    // Consider an inductive typeclass C with methods (operations) C[X] ⇒ X.
    // The Church encoding of the free typeclass instance C over a type Z is
    // ∀X.􏰂 ( Z + C[X] ⇒ X ) 􏰃⇒ X  or equivalently  ∀X.􏰂 (Z ⇒ X) × (C[X] ⇒ X) 􏰃⇒ X
    trait CFreeP[C[_], Z, X] { // This could be a case class instead of a `trait`, since there are no quantified types inside.
      def wrap: Z ⇒ X

      def ops: C[X] ⇒ X
    }

    // The type definition is not recursive but has a universally quantified type. So it must be a `trait`.
    trait CFree[C[_], Z] {
      def run[X]: CFreeP[C, Z, X] ⇒ X
    }

    // Prove that Free[C, Z] is an instance of typeclass C generated by Z.
    // This means we have a value of type C[X] ⇒ X for X = CFree[C, Z] (methods of the typeclass)
    // and that we can inject a value of Z into CFree[C, Z].

    // How to derive C[ ∀X.􏰂 ( Z + C[X] ⇒ X ) 􏰃⇒ X ] ⇒ ∀Y.􏰂 ( Z + C[Y] ⇒ Y ) 􏰃⇒ Y ?
    // The only way would be to get a C[Y] out of C [ ∀X.􏰂 ( Z + C[X] ⇒ X ) 􏰃⇒ X ] somehow,
    // and then substitute into C[Y] ⇒ Y.
    // For a given Y, we may set X = Y in ∀X.􏰂 ( Z + C[X] ⇒ X ) 􏰃⇒ X and then map the value under the functor C,
    // substituting the available value Z + C[Y] ⇒ Y.

    def ops[C[_] : Functor, Z]: C[CFree[C, Z]] ⇒ CFree[C, Z] = { cf: C[CFree[C, Z]] ⇒
      new CFree[C, Z] {
        def run[Y]: CFreeP[C, Z, Y] ⇒ Y = { cfp: CFreeP[C, Z, Y] ⇒
          // We are required to produce a value of Y.
          // 1. Map under C[ ∀X.􏰂 CFreeP[C, Z, X] 􏰃⇒ X ] setting X = Y and substituting `cfp`, to get a C[Y].
          val cy: C[Y] = cf.map[Y] { cfcz: CFree[C, Z] ⇒ cfcz.run[Y](cfp) }
          // 2. Substitute this C[Y] into cfp's `ops` method, and get a Y. 
          val y: Y = cfp.ops(cy)
          y
        }
      }
    }

    def wrap[C[_], Z]: Z ⇒ CFree[C, Z] = z ⇒ new CFree[C, Z] {
      def run[X]: CFreeP[C, Z, X] ⇒ X = cfp ⇒ cfp.wrap(z)
    }

    // Interpret a Free[C, Z] into an instance P : C.
    def run[C[_] : Functor, P, Z](ex: Z ⇒ P)(implicit pMethodsC: C[P] ⇒ P): CFree[C, Z] ⇒ P = { cf ⇒
      cf.run[P](new CFreeP[C, Z, P] {
        def ops: C[P] ⇒ P = pMethodsC

        def wrap: Z ⇒ P = ex
      })
    }

    // Example: semigroup; the single method is P × P ⇒ P. This is represented as SemiG[P] ⇒ P,
    // where SemiG is the methods functor for the semigroup:
    type SemiG[P] = (P, P)

    // Functor instance for SemiG.
    implicit val functorSemiG: Functor[SemiG] = cats.derive.functor[SemiG]

    // Free semigroup generated by String.
    type FreeSemigroupOverString = CFree[SemiG, String]

    // Create a value of this type and perform some operations on it, using `wrap` and `ops` as helpers.
    val s1 = wrap[SemiG, String]("abc")
    val s2 = wrap[SemiG, String]("xyz")
    val s3 = ops[SemiG, String].apply((s1, s2)) // Need `apply` because of implicit functor evidence for SemiG.

    // Interpret `s3` into the standard semigroup of strings.
    implicit val semigString: SemiG[String] ⇒ String = {
      case (x, y) ⇒ x + y
    }

    val r3 = run[SemiG, String, String](identity).apply(s3)
    r3 shouldEqual "abcxyz"
  }

}
