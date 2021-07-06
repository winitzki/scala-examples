package example

import cats.data.State
import cats.syntax.apply._
import cats.syntax.functor._
import cats.syntax.monoid._
import cats.{Applicative, Monoid}
import org.scalatest.{Assertion, FlatSpec, Matchers}

class Chapter09_nonstandard_traversals extends FlatSpec with Matchers {

  behavior of "nonstandard traversals"

  sealed trait T2[A]

  final case class Leaf[A](a: A) extends T2[A]

  final case class Branch[A](l: T2[A], r: T2[A]) extends T2[A]

  def foldMap[A, M: Monoid](f: A => M)(t: T2[A]): M = t match {
    case Leaf(a) => f(a)
    case Branch(t1, t2) => foldMap(f)(t1) |+| foldMap(f)(t2)
  }

  implicit def monoidList[A]: Monoid[List[A]] = new Monoid[List[A]] {
    override def empty: List[A] = List()

    override def combine(x: List[A], y: List[A]): List[A] = x ++ y
  }

  def toList[A]: T2[A] => List[A] = foldMap[A, List[A]](List(_)) // Need a Monoid instance for List[A].

  def toListDFS[A]: T2[A] ⇒ List[A] = {
    case Leaf(a) ⇒ List(a)
    case Branch(l, r) ⇒ toListDFS(l) ++ toListDFS(r)
  }

  // This is not the same as map2(l, r)(_ ++ _) because zip will truncate the longer list to the length of the
  // shorter list, but we should not do that here.
  def listMerge[A](l: List[List[A]], r: List[List[A]]): List[List[A]] = (l, r) match {
    case (Nil, r) ⇒ r
    case (l, Nil) ⇒ l
    case (lh :: lt, rh :: rt) ⇒ (lh ++ rh) :: listMerge(lt, rt)
  }

  def toList2[A]: T2[A] ⇒ List[List[A]] = {
    case Leaf(a) ⇒ List(List(a))
    case Branch(l, r) ⇒ listMerge(Nil :: toList2(l), Nil :: toList2(r))
  }

  def toListBFS[A]: T2[A] ⇒ List[A] = toList2 andThen (_.flatten)

  it should "implement listMerge" in {
    listMerge(List(Nil, List(1), List(3), List(6)), List(List(2), Nil, List(4, 5))) shouldEqual List(List(2), List(1), List(3, 4, 5), List(6))
    listMerge(List(List(1), Nil, List(4)), List(List(2), List(3))) shouldEqual List(List(1, 2), List(3), List(4))
  }

  val t2 = Branch(Leaf(1), Branch(Branch(Leaf(3), Leaf(4)), Leaf(2)))
  /* Visualize t2:
       *
      / \
     1  *
       / \
      *  2
     / \
    3  4

   */

  val t3 = Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Branch(Branch(Leaf(5), Branch(Leaf(6), Leaf(7))), Leaf(4)))
  /* Visualize t3:
        *
       / \
      /   \
     /     \
    /       \
   /         \
  *           *
 / \         / \
1  *        *  4
  / \      / \
 2  3     5  *
            / \
           6  7
  */

  def testEvalTo[A](x: A)(y: A): Assertion = x shouldEqual y

  it should "implement toList for binary tree" in {
    toList(t2) shouldEqual List(1, 3, 4, 2)

    {
      val t2 = Branch(Leaf(8), Branch(Branch(Leaf(3), Leaf(5)), Leaf(4)))
      testEvalTo[List[Int]] {
        toList(t2)
      } {
        List(8, 3, 5, 4)
      }
      testEvalTo[List[List[Int]]] {
        toList2(t2)
      } {
        List(List(), List(8), List(4), List(3, 5))
      }
      testEvalTo[List[Int]] {
        toListBFS(t2)
      } {
        List(8, 4, 3, 5)
      }
    }
  }

  it should "implement breadth-first traversal for binary tree" in {

    toListDFS(t2) shouldEqual List(1, 3, 4, 2)
    toList2(t2) shouldEqual List(Nil, List(1), List(2), List(3, 4))
    toListBFS(t2) shouldEqual List(1, 2, 3, 4)

    toListDFS(t3) shouldEqual List(1, 2, 3, 5, 6, 7, 4)
    toListBFS(t3) shouldEqual List(1, 4, 2, 3, 5, 6, 7)
    toList2(t3) shouldEqual List(Nil, Nil, List(1, 4), List(2, 3, 5), List(6, 7))
  }

  object ListXExample {
    sealed trait ListX[A]

    final case class One[A](a: A) extends ListX[A]

    final case class Two[A](a: A, tail: ListX[Option[A]]) extends ListX[A]

    val lx: ListX[Int] = Two(1, Two(Some(1), Two(Some(Some(2)), One(Some(Some(Some(3)))))))
  }

  /* This data structure is a "breadth-first tree descriptor".

  For example, the tree t2:
       *
      / \
     1  *
       / \
      *  2
     / \
    3  4

  corresponds to the descriptor List(
                                     List(None),
                                     List(Left(Some(1)), Right(None)),
                                     List(Right(Left(None)), Right(Right(Some(2)))),
                                     List(Right(Left(Left(Some(3)))), Right(Left(Right(Some(4)))))
                                    )

  Can we simplify this to:        List(
                                      List(),
                                      List(Left(1)),
                                      List(Right(Right(2))),
                                      List(Right(Left(Left(3))), Right(Left(Right(4))))
                                     )
  ?
   */
  sealed trait TD[A]

  final case class Last[A](a: List[A]) extends TD[A]

  final case class More[A](a: List[A], tail: TD[Either[A, A]]) extends TD[A]

  def t2ToTD[A]: T2[A] => TD[A] = {
    case Leaf(a) => Last(List(a))
    case Branch(l, r) => More(List(), tdMerge(addLeft(t2ToTD(l)), addRight(t2ToTD(r))))
  }

  def addLeft[A]: TD[A] ⇒ TD[Either[A, A]] = {
    case Last(a) ⇒ Last(a.map(Left.apply))
    case More(a, tail) ⇒ More(a.map(Left.apply), addLeft[Either[A, A]](tail))
  }

  def addRight[A]: TD[A] ⇒ TD[Either[A, A]] = {
    case Last(a) ⇒ Last(a.map(Right.apply))
    case More(a, tail) ⇒ More(a.map(Right.apply), addRight[Either[A, A]](tail))
  }

  /* Note: here we need wrap: [A] => A => Either[A, A], or else the code does not compile!
  def addLayer[A](wrap: A => Either[A, A]): TD[A] => TD[Either[A, A]] = {
    case Last(a)         => Last(a.map(wrap))
    case More(a, tail)   => More(a.map(wrap), addLayer[Either[A, A]](wrap)(tail))
  }
*/
  def getLeft[A]: List[Either[A, A]] ⇒ List[A] = _.collect { case Left(x) ⇒ x }

  def getRight[A]: List[Either[A, A]] ⇒ List[A] = _.collect { case Right(x) ⇒ x }

  def filterLeft[A]: TD[Either[A, A]] ⇒ TD[A] = {
    case Last(la) ⇒ Last(getLeft(la))
    case More(la, tail) ⇒ More(getLeft(la), filterLeft(tail))
  }

  def filterRight[A]: TD[Either[A, A]] ⇒ TD[A] = {
    case Last(la) ⇒ Last(getRight(la))
    case More(la, tail) ⇒ More(getRight(la), filterRight(tail))
  }

  def tdMerge[A](l: TD[A], r: TD[A]): TD[A] = (l, r) match {
    case (Last(la), Last(lb)) ⇒ Last(la ++ lb)
    case (Last(la), More(lb, tail)) ⇒ More(la ++ lb, tail)
    case (More(la, tail), Last(lb)) ⇒ More(la ++ lb, tail)
    case (More(la, tailA), More(lb, tailB)) ⇒ More(la ++ lb, tdMerge(tailA, tailB))
  }

  def tdToT2[A]: TD[A] ⇒ T2[A] = { // The argument is assumed to be a top-level tree.
    case Last(List(a)) ⇒ Leaf(a) // We may disallow all other cases here.
    case More(List(a), tail) ⇒ // Here `tail` must consist of empty lists.
      Leaf(a)
    case More(List(), tail) ⇒
      val left = filterLeft(tail)
      val right = filterRight(tail)
      Branch(tdToT2(left), tdToT2(right))
  }

  it should "convert trees to BFS tree descriptors and back without loss of information" in {
    val td2: TD[Int] = t2ToTD(t2)
    td2 shouldEqual
      More[Int](
        List(), More(
          List(Left(1)), More(
            List(Right(Right(2))), Last(
              List(Right(Left(Left(3))), Right(Left(Right(4))))
            ))))

    val t2new: T2[Int] = tdToT2(td2)
    t2new shouldEqual t2

    val td3: TD[Int] = t2ToTD(t3)
    val t3new: T2[Int] = tdToT2(td3)
    t3 shouldEqual t3new
  }

  def travList[A, B, F[_] : Applicative](f: A ⇒ F[B])(la: List[A]): F[List[B]] = la match {
    case Nil ⇒ Applicative[F].pure(Nil)
    case head :: tail ⇒ f(head).map2(travList(f)(tail)) { (x, y) ⇒ x :: y }
  }

  def travBFS[A, B, F[_] : Applicative](f: A ⇒ F[B])(t2: T2[A]): F[T2[B]] = travTD(f)(t2ToTD(t2)).map(tdToT2)

  def travEither[A, B, F[_] : Applicative](f: A ⇒ F[B])(e: Either[A, A]): F[Either[B, B]] = e match {
    case Left(a) ⇒ f(a).map(Left.apply)
    case Right(a) ⇒ f(a).map(Right.apply)
  }

  def travTD[A, B, F[_] : Applicative](f: A ⇒ F[B])(td: TD[A]): F[TD[B]] = td match {
    case Last(a) ⇒ travList(f)(a).map(Last.apply)
    case More(a, tail) ⇒
      val head: F[List[B]] = travList(f)(a)
      val t: F[TD[Either[B, B]]] = travTD { x: Either[A, A] ⇒ travEither(f)(x) }(tail)
      head.map2(t) { (lb, tb) ⇒ More(lb, tb) }
  }

  type S[X] = State[Int, X]
  val makeLabel: S[Int] = for {
    s ← State.get
    _ ← State.set(s + 1)
  } yield s

  def zipWithIndexBFS[A](t: T2[A]): T2[(A, Int)] = {
    val tDecorated = travBFS { leaf: A ⇒ makeLabel.map(x ⇒ (leaf, x)) }(t)
    tDecorated.run(0).value._2
  }

  it should "decorate a tree using a BFS traversal with the State monad" in {
    // Use a state monad as the applicative effect.

    val t2 = Branch(Leaf(8), Branch(Branch(Leaf(3), Leaf(5)), Leaf(4)))
    zipWithIndexBFS(t2) shouldEqual Branch(Leaf((8, 0)), Branch(Branch(Leaf((3, 2)), Leaf((5, 3))), Leaf((4, 1))))

    val t2decorated = travBFS { leaf: Int ⇒ makeLabel.map(x ⇒ (leaf, s"order = $x")) }(t2)
    val result: T2[(Int, String)] = t2decorated.run(0).value._2
    result shouldEqual Branch(Leaf((8, "order = 0")), Branch(Branch(Leaf((3, "order = 2")), Leaf((5, "order = 3"))), Leaf((4, "order = 1"))))
  }

  def zipWithDepth[A](initial: Int = 0): T2[A] ⇒ T2[(A, Int)] = {
    case Leaf(a) ⇒ Leaf((a, initial))
    case Branch(l, r) ⇒ Branch(zipWithDepth(initial + 1)(l), zipWithDepth(initial + 1)(r))
  }

  it should "implement zipWithDepth" in {
    val t2 = Branch(Leaf(8), Branch(Branch(Leaf(3), Leaf(5)), Leaf(4)))
    zipWithDepth()(t2) shouldEqual Branch(Leaf((8, 1)), Branch(Branch(Leaf((3, 3)), Leaf((5, 3))), Leaf((4, 2))))
  }

  it should "implement printLaTeX" in {
    def printLaTeX[A](t: T2[A])(toString: A => String): String = {

      def printLaTeXSubtree: T2[A] => String = {
        case Leaf(a)        => toString(a)
        case Branch(l, r)   => "[ " + printLaTeXSubtree(l) + " " + printLaTeXSubtree(r) + " ]"
      }

      "\\Tree" + printLaTeXSubtree(t)
    }

    val t2: T2[Int] = Branch(Leaf(8), Branch(Branch(Leaf(3), Leaf(5)), Leaf(4)))

    printLaTeX(t2)(_.toString) shouldEqual "\\Tree[ 8 [ [ 3 5 ] 4 ] ]"
  }
}
