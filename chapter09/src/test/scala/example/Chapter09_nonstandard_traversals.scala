package example

import org.scalatest.{FlatSpec, Matchers}

class Chapter09_nonstandard_traversals extends FlatSpec with Matchers {

  behavior of "nonstandard traversals"

  sealed trait T2[A]

  final case class Leaf[A](a: A) extends T2[A]

  final case class Branch[A](l: T2[A], r: T2[A]) extends T2[A]

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

  def toListBFS2[A]: T2[A] ⇒ List[List[A]] = {
    case Leaf(a) ⇒ List(List(a))
    case Branch(l, r) ⇒ listMerge(Nil :: toListBFS2(l), Nil :: toListBFS2(r))
  }

  def toListBFS[A]: T2[A] ⇒ List[A] = toListBFS2 andThen (_.flatten)

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

  it should "implement breadth-first traversal for binary tree" in {

    toListDFS(t2) shouldEqual List(1, 3, 4, 2)
    toListBFS2(t2) shouldEqual List(Nil, List(1), List(2), List(3, 4))
    toListBFS(t2) shouldEqual List(1, 2, 3, 4)

    toListDFS(t3) shouldEqual List(1, 2, 3, 5, 6, 7, 4)
    toListBFS(t3) shouldEqual List(1, 4, 2, 3, 5, 6, 7)
    toListBFS2(t3) shouldEqual List(Nil, Nil, List(1, 4), List(2, 3, 5), List(6, 7))
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

   */
  sealed trait TD[A]

  final case class L[A](a: List[A]) extends TD[A]

  final case class Two[A](a: List[A], tail: TD[Either[A, A]]) extends TD[A]

  def toTreeBFS[A]: T2[A] ⇒ TD[Option[A]] = {
    case Leaf(a) ⇒
    case Branch(l, r) ⇒
  }
}
