package example

import cats.data.{IndexedStateT, State}
import org.scalatest.{Assertion, FlatSpec, Matchers}
import cats.{Applicative, Eval, Monoid}
import cats.syntax.monoid._
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.functor._

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

  final case class L[A](a: List[A]) extends TD[A]

  final case class Two[A](a: List[A], tail: TD[Either[A, A]]) extends TD[A]

  def toTreeBFS[A]: T2[A] ⇒ TD[A] = {
    case Leaf(a) ⇒ L(List(a))
    case Branch(l, r) ⇒ tdMerge(pushDownLeft(toTreeBFS(l)), pushDownRight(toTreeBFS(r)))
  }

  def pushDownLeft[A]: TD[A] ⇒ TD[A] = td ⇒ Two(List(), mapLeft(td))

  def pushDownRight[A]: TD[A] ⇒ TD[A] = td ⇒ Two(List(), mapRight(td))

  // Special methods that wrap with Left and Right outside rather than inside.
  def mapLeft[A]: TD[A] ⇒ TD[Either[A, A]] = {
    case L(a) ⇒ L(a.map(Left.apply))
    case Two(a, tail) ⇒ Two(a.map(Left.apply), mapLeft(tail))
  }

  def mapRight[A]: TD[A] ⇒ TD[Either[A, A]] = {
    case L(a) ⇒ L(a.map(Right.apply))
    case Two(a, tail) ⇒ Two(a.map(Right.apply), mapRight(tail))
  }

  def getLeft[A]: List[Either[A, A]] ⇒ List[A] = _.collect { case Left(x) ⇒ x }

  def getRight[A]: List[Either[A, A]] ⇒ List[A] = _.collect { case Right(x) ⇒ x }

  def filterLeft[A]: TD[Either[A, A]] ⇒ TD[A] = {
    case L(la) ⇒ L(getLeft(la))
    case Two(la, tail) ⇒ Two(getLeft(la), filterLeft(tail))
  }

  def filterRight[A]: TD[Either[A, A]] ⇒ TD[A] = {
    case L(la) ⇒ L(getRight(la))
    case Two(la, tail) ⇒ Two(getRight(la), filterRight(tail))
  }

  def tdMerge[A](l: TD[A], r: TD[A]): TD[A] = (l, r) match {
    case (L(la), L(lb)) ⇒ L(la ++ lb)
    case (L(la), Two(lb, tail)) ⇒ Two(la ++ lb, tail)
    case (Two(la, tail), L(lb)) ⇒ Two(la ++ lb, tail)
    case (Two(la, tailA), Two(lb, tailB)) ⇒ Two(la ++ lb, tdMerge(tailA, tailB))
  }

  def tdToTree[A]: TD[A] ⇒ T2[A] = { // The argument is assumed to be a top-level tree.
    case L(List(a)) ⇒ Leaf(a) // We may disallow all other cases here.
    case Two(List(a), tail) ⇒ // Here `tail` must consist of empty lists.
      Leaf(a)
    case Two(List(), tail) ⇒
      val left = filterLeft(tail)
      val right = filterRight(tail)
      Branch(tdToTree(left), tdToTree(right))
  }

  it should "convert trees to BFS tree descriptors and back without loss of information" in {
    val td2: TD[Int] = toTreeBFS(t2)
    td2 shouldEqual
      Two[Int](
        List(), Two(
          List(Left(1)), Two(
            List(Right(Right(2))), L(
              List(Right(Left(Left(3))), Right(Left(Right(4))))
            ))))

    val t2new: T2[Int] = tdToTree(td2)
    t2new shouldEqual t2

    val td3: TD[Int] = toTreeBFS(t3)
    val t3new: T2[Int] = tdToTree(td3)
    t3 shouldEqual t3new
  }

  def travList[A, B, F[_] : Applicative](f: A ⇒ F[B])(la: List[A]): F[List[B]] = la match {
    case Nil ⇒ Applicative[F].pure(Nil)
    case head :: tail ⇒ f(head).map2(travList(f)(tail)) { (x, y) ⇒ x :: y }
  }

  def travBFS[A, B, F[_] : Applicative](f: A ⇒ F[B])(t2: T2[A]): F[T2[B]] = travTD(f)(toTreeBFS(t2)).map(tdToTree)

  def mapEither[A, B, F[_] : Applicative](f: A ⇒ F[B])(e: Either[A, A]): F[Either[B, B]] = e match {
    case Left(a) ⇒ f(a).map(Left.apply)
    case Right(a) ⇒ f(a).map(Right.apply)
  }

  def travTD[A, B, F[_] : Applicative](f: A ⇒ F[B])(td: TD[A]): F[TD[B]] = td match {
    case L(a) ⇒ travList(f)(a).map(L.apply)
    case Two(a, tail) ⇒
      val head: F[List[B]] = travList(f)(a)
      val t: F[TD[Either[B, B]]] = travTD { x: Either[A, A] ⇒ mapEither(f)(x) }(tail)
      head.map2(t) { (lb, tb) ⇒ Two(lb, tb) }
  }

  it should "decorate a tree using a BFS traversal with the State monad" in {
    // Use a state monad as the applicative effect.
    type S[X] = State[Int, X]
    val makeLabel: S[Int] = for {
      s ← State.get
      _ ← State.set(s + 1)
    } yield s
    val t2 = Branch(Leaf(8), Branch(Branch(Leaf(3), Leaf(5)), Leaf(4)))
    val t2decorated = travBFS { leaf: Int ⇒ makeLabel.map(x ⇒ (leaf, s"order = $x")) }(t2)
    val result: T2[(Int, String)] = t2decorated.run(0).value._2
    result shouldEqual Branch(Leaf((8, "order = 0")), Branch(Branch(Leaf((3, "order = 2")), Leaf((5, "order = 3"))), Leaf((4, "order = 1"))))
  }
}
