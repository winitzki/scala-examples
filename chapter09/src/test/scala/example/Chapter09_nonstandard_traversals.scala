package example

import cats.data.State
import cats.syntax.apply._
import cats.syntax.functor._
import cats.syntax.monoid._
import cats.{Applicative, Functor, Monoid}
import io.chymyst.ch.implement
import org.scalatest.{Assertion, FlatSpec, Matchers}

class Chapter09_nonstandard_traversals extends FlatSpec with Matchers {

  behavior of "nonstandard traversals"

  sealed trait T2[A]

  final case class Leaf[A](a: A) extends T2[A]

  final case class Branch[A](l: T2[A], r: T2[A]) extends T2[A]

  implicit class FunctorT2[A](t2: T2[A]) {
    def map[B](f: A ⇒ B): T2[B] = t2 match {
      case Leaf(a) ⇒ Leaf(f(a))
      case Branch(l, r) ⇒ Branch(l map f, r map f)
    }
  }

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
  def removeLeft[A]: List[Either[A, A]] ⇒ List[A] = _.collect { case Left(x) ⇒ x }

  def removeRight[A]: List[Either[A, A]] ⇒ List[A] = _.collect { case Right(x) ⇒ x }

  def filterLeft[A]: TD[Either[A, A]] ⇒ TD[A] = {
    case Last(la) ⇒ Last(removeLeft(la))
    case More(la, tail) ⇒ More(removeLeft(la), filterLeft(tail))
  }

  def filterRight[A]: TD[Either[A, A]] ⇒ TD[A] = {
    case Last(la) ⇒ Last(removeRight(la))
    case More(la, tail) ⇒ More(removeRight(la), filterRight(tail))
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

  type St[X] = State[Int, X]
  val makeLabel: St[Int] = for {
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
        case Leaf(a) => toString(a)
        case Branch(l, r) => "[ " + printLaTeXSubtree(l) + " " + printLaTeXSubtree(r) + " ]"
      }

      "\\Tree" + printLaTeXSubtree(t)
    }

    val t2: T2[Int] = Branch(Leaf(8), Branch(Branch(Leaf(3), Leaf(5)), Leaf(4)))

    printLaTeX(t2)(_.toString) shouldEqual "\\Tree[ 8 [ [ 3 5 ] 4 ] ]"
  }

  it should "implement printLaTeX via recursion schemes" in {
    type S[A, R] = Either[A, (R, R)]
    final case class T2[A](run: S[A, T2[A]])

    def fmapR[A, R, T](f: R => T): S[A, R] => S[A, T] = _.map { case (r1, r2) => (f(r1), f(r2)) }

    def foldT2[A, Z](f: S[A, Z] => Z)(tree: T2[A]): Z = f(fmapR(foldT2(f))(tree.run))

    def toLaTeX[A]: S[A, String] => String = {
      case Left(a) => a.toString
      case Right((l, r)) => s"[ $l $r ]"
    }

    def printLaTeX[A](tree: T2[A]): String = "\\Tree" + foldT2[A, String](toLaTeX)(tree)

    val t2a: T2[Int] = T2(Right((T2(Right((T2(Left(8)), T2(Right((T2(Left(3)), T2(Left(5)))))))), T2(Left(4)))))

    printLaTeX(t2a) shouldEqual "\\Tree[ [ 8 [ 3 5 ] ] 4 ]"

    val t2: T2[Int] = T2(Right((T2(Left(8)), T2(Right((T2(Right((T2(Left(3)), T2(Left(5))))), T2(Left(4))))))))

    printLaTeX(t2) shouldEqual "\\Tree[ 8 [ [ 3 5 ] 4 ] ]"

  }

  sealed trait NEL[A] {
    def mkString(suffix: String): String

    def map[B](f: A ⇒ B): NEL[B]

    def length: Int

    def fold(update: (A, A) ⇒ A): A
  }

  final case class One[A](a: A) extends NEL[A] {
    override def mkString(suffix: String): String = a.toString

    override def map[B](f: A ⇒ B): NEL[B] = One(f(a))

    override def length: Int = 1

    override def fold(update: (A, A) ⇒ A): A = a
  }

  final case class Two[A](head: A, tail: NEL[A]) extends NEL[A] {
    override def mkString(suffix: String): String = head.toString + suffix + tail.mkString(suffix)

    override def map[B](f: A ⇒ B): NEL[B] = Two(f(head), tail map f)

    override def length: Int = 1 + tail.length

    override def fold(update: (A, A) ⇒ A): A = update(head, tail.fold(update))
  }

  object NEL {
    def apply[A](head: A, tail: A*): NEL[A] = tail.toList match {
      case Nil ⇒ One(head)
      case hd :: tl ⇒ Two(head, NEL.apply(hd, tl: _*))
    }

    def max[A](nel: NEL[A])(implicit ord: Ordering[A]): A = nel.fold(ord.max)
  }

  implicit class NelOps[A](nel: NEL[A]) {
    def max(implicit ord: Ordering[A]): A = NEL.max(nel)
  }

  sealed trait TreeN[A]

  object TreeN {
    final case class Leaf[A](a: A) extends TreeN[A]

    final case class Branch[A](ts: NEL[TreeN[A]]) extends TreeN[A]
  }

  final case class Fix[S[_, _], A](unfix: S[A, Fix[S, A]])

  it should "implement several printLaTeX functions via recursion schemes" in {
    type S1[A, R] = Option[(A, R)] // For List.
    type S2[A, R] = Either[A, (A, R)] // For NEL.
    type S3[A, R] = Either[A, NEL[R]] // For TreeN.

    // Define some values of these types.
    // Equivalent to List(1, 2, 3)
    val x1 = Fix[S1, Int](Some((1, Fix[S1, Int](Some((2, Fix[S1, Int](Some((3, Fix[S1, Int](None))))))))))

    // Equivalent to NEL(1, 2, 3)
    val x2 = Fix[S2, Int](Right((1, Fix[S2, Int](Right((2, Fix[S2, Int](Left(3))))))))

    // Equivalent to the tree t2 used earlier.
    val x3 = Fix[S3, Int](Right(NEL(Fix[S3, Int](Right(NEL(Fix[S3, Int](Left(8)), Fix[S3, Int](Right(NEL(Fix[S3, Int](Left(3)), Fix[S3, Int](Left(5)))))))), Fix[S3, Int](Left(4)))))

    implicit def functorS1[X]: Functor[S1[X, *]] = new Functor[S1[X, *]] {
      override def map[A, B](fa: S1[X, A])(f: A ⇒ B): S1[X, B] = implement
    }

    implicit def functorS2[X]: Functor[S2[X, *]] = new Functor[S2[X, *]] {
      override def map[A, B](fa: S2[X, A])(f: A ⇒ B): S2[X, B] = implement
    }

    implicit def functorS3[X]: Functor[S3[X, *]] = new Functor[S3[X, *]] {
      override def map[A, B](fa: S3[X, A])(f: A ⇒ B): S3[X, B] = fa match {
        case Left(x) ⇒ Left(x)
        case Right(nel) ⇒ Right(nel.map(f))
      }
    }

    import cats.syntax.functor._
    def fold[A, Z, S[_, _]](f: S[A, Z] => Z)(t: Fix[S, A])(implicit fs: Functor[S[A, *]]): Z =
      f(t.unfix.map(fold(f)))

    def toLaTeX1[A]: S1[A, String] => String = {
      case None => "Nil"
      case Some((head, tail)) => head.toString + ", " + tail
    }

    def toLaTeX2[A]: S2[A, String] => String = {
      case Left(a) => a.toString
      case Right((head, tail)) => head.toString + ", " + tail
    }

    def toLaTeX3[A]: S3[A, String] => String = {
      case Left(a) => a.toString
      case Right(nel) => "[ " + nel.mkString(" ") + " ]" // Assume mkString() is defined for NEL.
    }

    def listToLaTeX[A](t: Fix[S1, A]): String = "[ " + fold[A, String, S1](toLaTeX1)(t) + " ]"

    def nelToLaTeX[A](t: Fix[S2, A]): String = "[ " + fold[A, String, S2](toLaTeX2)(t) + " ]"

    def treeNToLaTeX[A](t: Fix[S3, A]): String = "\\Tree" + fold[A, String, S3](toLaTeX3)(t)

    listToLaTeX(x1) shouldEqual "[ 1, 2, 3, Nil ]"

    nelToLaTeX(x2) shouldEqual "[ 1, 2, 3 ]"

    treeNToLaTeX(x3) shouldEqual "\\Tree[ [ 8 [ 3 5 ] ] 4 ]"
  }

  it should "define and test foldTreeN" in {
    type S3[A, R] = Either[A, NEL[R]] // For TreeN.

    def foldT2[A, Z](f: Either[A, (Z, Z)] => Z): T2[A] => Z = {
      case Leaf(a) => f(Left(a))
      case Branch(l, r) => f((Right(foldT2(f)(l), foldT2(f)(r))))
    }

    import TreeN.{Leaf ⇒ LeafN, Branch ⇒ BranchN}

    def foldTreeN[A, Z](f: Either[A, NEL[Z]] => Z): TreeN[A] => Z = {
      case LeafN(a) => f(Left(a))
      case BranchN(ts) => f(Right(ts.map(foldTreeN(f))))
    }

    def toLaTeX3[A]: S3[A, String] => String = {
      case Left(a) => a.toString
      case Right(nel) => "[ " + nel.mkString(" ") + " ]" // Assume mkString() is defined for NEL.
    }

    def printLaTeX[A](tree: TreeN[A]): String = "\\Tree" + foldTreeN[A, String](toLaTeX3)(tree)

    val x3a: TreeN[Int] = BranchN(NEL(LeafN(8), BranchN(NEL(BranchN(NEL(LeafN(3), LeafN(5))), LeafN(4)))))
    printLaTeX(x3a) shouldEqual "\\Tree[ 8 [ [ 3 5 ] 4 ] ]"
    val x3b: TreeN[Int] = BranchN(NEL(BranchN(NEL(LeafN(8), BranchN(NEL(LeafN(3), LeafN(5))))), LeafN(4)))
    printLaTeX(x3b) shouldEqual "\\Tree[ [ 8 [ 3 5 ] ] 4 ]"

    def maxBranching[A]: TreeN[A] => Int = foldTreeN[A, Int] {
      case Left(_) => 0
      case Right(nel) => math.max(nel.max, nel.length) // The `max` and `length` methods must be defined.
    }

    maxBranching(x3a) shouldEqual 2
    maxBranching(x3b) shouldEqual 2
  }

  it should "define and test unfoldList" in {

    type S[A, R] = Option[(A, R)]

    def unfoldList[A, Z](f: Z => S[A, Z])(init: Z): List[A] = f(init) match {
      case None => Nil
      case Some((a, z)) => a :: unfoldList(f)(z)
    }

    def f(n: Long): Long => Option[(Long, Long)] = { z =>
      if (z >= n) None else Some((z, z * 2))
    }

    def powersOf2UpTo(n: Long): List[Long] = unfoldList(f(n))(1)

    powersOf2UpTo(1000) shouldEqual List(1, 2, 4, 8, 16, 32, 64, 128, 256, 512)
  }

  it should "define and test unfoldT2" in {
    type S[A, R] = Either[A, (R, R)]

    def unfoldT2[A, Z](f: Z => S[A, Z])(init: Z): T2[A] = f(init) match {
      case Left(a) => Leaf(a)
      case Right((z1, z2)) => Branch(unfoldT2(f)(z1), unfoldT2(f)(z2))
    }

    def fullBinaryTree(n: Int): T2[Int] = {
      type Z = (Int, Int)
      val init: Z = (0, 1 << n)
      val f: Z => Either[Int, (Z, Z)] = {
        case (k, m) if m == 1 => Left(k)
        case (k, m) => Right(((k, m / 2), (k + m / 2, m / 2)))
      }
      unfoldT2[Int, Z](f)(init)
    }

    fullBinaryTree(2) shouldEqual Branch(Branch(Leaf(0), Leaf(1)), Branch(Leaf(2), Leaf(3)))
  }

  it should "define and test evenOdd for T2" in {
    type S[A, R] = Either[A, (R, R)]

    def unfoldT2[A, Z](f: Z => S[A, Z])(init: Z): T2[A] = f(init) match {
      case Left(a) => Leaf(a)
      case Right((z1, z2)) => Branch(unfoldT2(f)(z1), unfoldT2(f)(z2))
    }

    def evenOdd(n: Int): T2[Int] = {
      final case class Z(startAt: Int, makeLeaf: Boolean)
      val init = Z(startAt = n, makeLeaf = false)
      val f: Z => Either[Int, (Z, Z)] = {
        case Z(n, false) if n > 0 && n % 2 == 0 => Right((Z(n - 1, false), Z(n, true)))
        case Z(n, false) if n > 0 && n % 2 == 1 => Right((Z(n, true), Z(n - 1, false)))
        case Z(n, _) => Left(n) // Make a leaf when n == 0 or makeLeaf == true.
      }
      unfoldT2[Int, Z](f)(init)
    }

    evenOdd(3) shouldEqual Branch(Leaf(3), Branch(Branch(Leaf(1), Leaf(0)), Leaf(2)))
    evenOdd(4) shouldEqual Branch(Branch(Leaf(3), Branch(Branch(Leaf(1), Leaf(0)), Leaf(2))), Leaf(4))
  }

  it should "use unfold to generate an unbounded tree" in {
    type S[A, R] = Either[A, Unit => (R, R)]
    sealed trait UT[A]
    final case class ULeaf[A](a: A) extends UT[A]
    final case class UBranch[A](run: Unit ⇒ (UT[A], UT[A])) extends UT[A]

    def unfoldUT[A, Z](f: Z ⇒ S[A, Z])(init: Z): UT[A] = f(init) match {
      case Left(a) ⇒ ULeaf(a)
      case Right(func) ⇒ UBranch { _ ⇒ // Important to delay the evaluation of func().
        val (z1, z2) = func(())
        (unfoldUT(f)(z1), unfoldUT(f)(z2))
      }
    }

    val tree1toInf = unfoldUT[Int, (Int, Boolean)] { case (z, makeLeaf) ⇒
      if (makeLeaf) Left(z) else Right(_ ⇒ ((z + 1, true), (z + 1, false)))
    }((0, false))

    def toT2[A](default: A, maxDepth: Int = 0): UT[A] ⇒ T2[A] = {
      case ULeaf(a) ⇒ Leaf(a)
      case UBranch(func) ⇒ if (maxDepth > 0) {
        val (z1, z2) = func(())
        Branch(toT2(default, maxDepth - 1)(z1), toT2(default, maxDepth - 1)(z2))
      } else Leaf(default)
    }

    toT2(-1, 3)(tree1toInf) shouldEqual Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(-1))))

  }

  it should "implement zipWithDepth as a traversal with recursion scheme and cats.state" in {
    type S[A, R] = Either[A, (R, R)] // Recursion scheme for T2.

    def travS[A, B, F[_]](f: S[A, F[T2[B]]] => F[T2[B]]): T2[A] => F[T2[B]] = {
      case Leaf(a) => f(Left(a))
      case Branch(l, r) => f(Right((travS(f)(l), travS(f)(r))))
    }

    travS[Int, (Int, Int), St] {
      case Left(a) ⇒ makeLabel.map(s ⇒ Leaf((a, s)))
      case Right((l, r)) ⇒ for {
        _ ← makeLabel // Increment depth by 1.
        s ← State.get
        x ← l // Start both left and right subtrees at the same depth.
        _ ← State.set(s)
        y ← r
      } yield Branch(x, y)
    }(t2).run(0).value._2 shouldEqual Branch(Leaf((1, 1)), Branch(Branch(Leaf((3, 3)), Leaf((4, 3))), Leaf((2, 2))))

  }

  it should "implement zipWithDepth as a traversal with recursion scheme and simple State monad" in {
    type S[A, R] = Either[A, (R, R)] // Recursion scheme for T2.

    def travS[A, B, F[_]](f: S[A, F[T2[B]]] => F[T2[B]]): T2[A] => F[T2[B]] = {
      case Leaf(a) => f(Left(a))
      case Branch(l, r) => f(Right((travS(f)(l), travS(f)(r))))
    }

    final case class St[A](run: Int => (A, Int)) { // A State monad with internal state of type Int.

      import io.chymyst.ch.implement

      def flatMap[B](f: A => St[B]): St[B] = implement

      def map[B](f: A => B): St[B] = implement
    }

    def incrementAndGet: St[Int] = St(s => (s + 1, s + 1)) // Increment the current state value.

    def get: St[Int] = St(s => (s, s)) // Fetch the current state value.

    def set(s: Int): St[Unit] = St(_ => ((), s)) // Set the state, ignore previous state value.

    def zipWithDepth[A](tree: T2[A]): T2[(A, Int)] = travS[A, (A, Int), St] {
      case Left(a)         => for { s <- get } yield Leaf((a, s)) // Put the current depth into the Leaf value.
      case Right((l, r))   => for {
        s <- incrementAndGet // Read the current depth after incrementing it.
        x <- l // Traverse the left branch starting from the new current depth.
        _ <- set(s) // Set the same depth before traversing the right branch.
        y <- r        // Traverse the right branch.
      } yield Branch(x, y)
    }(tree).run(0)._1
    val t2: T2[Int] = Branch(Leaf(8), Branch(Branch(Leaf(3), Leaf(5)), Leaf(4)))
    zipWithDepth(t2) shouldEqual Branch(Leaf((8, 1)), Branch(Branch(Leaf((3, 3)), Leaf((5, 3))), Leaf((4, 2))))
  }
}
