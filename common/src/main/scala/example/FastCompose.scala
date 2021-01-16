package example

import scala.annotation.tailrec

/** Stack-safe and fast function composition. Reimplement SafeCompose with more sophisticated optimizations.
 *
 * Usage:
 *
 * val f: A => B = ...
 * val g: B => C = ...
 * val h = f before g
 * val x: A = ...
 * h(x) // Interface should be the same as for functions, but h is a FastCompose value.
 *
 * All functions are implicitly converted to FastCompose whenever `before` or `after` are used.
 */
object FastCompose {

  // Define the specific collection type here?
  //
  @inline def of[A, B, Coll[_] : CollectionAPI](f: A ⇒ B): FastCompose[A, B, Coll] =
    FastCompose[A, B, Coll](CollectionAPI[Coll].pure(CountCompose(f.asInstanceOf[Any ⇒ Any])))

  // Convenience method.
  @inline def of[A, B, Coll[_] : CollectionAPI](f: FastCompose[A, B, Coll]): FastCompose[A, B, Coll] = f

  //
  //  @inline def apply[A, B, Coll[_]](countCompose: CountCompose[A, B])(implicit collAPI: CollectionAPI[Coll]): FastCompose[A, B, Coll] =
  //    FastCompose[A, B, Coll](collAPI.pure(countCompose.asInstanceOf[CountCompose[Any, Any]]))

  @inline implicit class FastComposeOps[A, B, Coll[_]](val f: A ⇒ B)(implicit collAPI: CollectionAPI[Coll]) {
    @inline def before[C](g: B ⇒ C): FastCompose[A, C, Coll] = f match {
      case p@FastCompose(_) ⇒ p.before(g).asInstanceOf[FastCompose[A, C, Coll]]
      case _ ⇒ FastCompose(collAPI.pure(CountCompose((f andThen g).asInstanceOf[Any ⇒ Any], 2)))
    }

    @inline def after[C](g: C ⇒ A): FastCompose[C, A, Coll] = f match {
      case p@FastCompose(_) ⇒ p.after(g).asInstanceOf[FastCompose[C, A, Coll]]
      case _ ⇒ FastCompose(collAPI.pure(CountCompose((f compose g).asInstanceOf[Any ⇒ Any], 2)))
    }

  }

  def andThen[A, B, C, Coll[_]](f: FastCompose[A, B, Coll], g: FastCompose[B, C, Coll])(implicit collAPI: CollectionAPI[Coll]): FastCompose[A, C, Coll] = {
    val chain1 = f.chain
    val chain2 = g.chain
    if (collAPI.isLengthOne(chain1)) {
      val head1 = collAPI.head(chain1)
      val head2 = collAPI.head(chain2)
      if (head1.composedCount + head2.composedCount <= collAPI.directCompositionLimit)
        FastCompose(collAPI.replaceHead(chain2, head1 andThen head2))
      else // Cannot optimize at the end of chain:
        FastCompose(collAPI.prepend(head1, chain2))
    } else {
      FastCompose(collAPI.concat(chain1, chain2))
    }
  }
}

// Wrap a function A => B and count the number of function compositions inside it.
// Function composition will compose functions and add the counts.
private[example] final case class CountCompose[
  @specialized(scala.Int, scala.Long, scala.Float, scala.Double) -A,
  @specialized(scala.Unit, scala.Boolean, scala.Int, scala.Float, scala.Long, scala.Double) +B
](f: A ⇒ B, composedCount: Int = 1) extends (A ⇒ B) {

  @inline override def apply(a: A): B = f(a)

  @inline override def andThen[C](other: B ⇒ C): CountCompose[A, C] = other match {
    case CountCompose(g, c) ⇒ CountCompose(f andThen g, composedCount + c)
    case _ ⇒ CountCompose(f andThen other, composedCount + 1)
  }

  @inline override def compose[C](other: C ⇒ A): CountCompose[C, B] = other match {
    case CountCompose(g, c) ⇒ CountCompose(f compose g, composedCount + c)
    case _ ⇒ CountCompose(f compose other, composedCount + 1)
  }
}


// The `chain` contains a sequence of counted-composed functions.
// The *first* function in the chain must have less than `directCompositionLimit` compositions.
private[example] final case class FastCompose[-A, +B, Coll[_]] private(private val chain: Coll[CountCompose[Any, Any]])(implicit collAPI: CollectionAPI[Coll]) extends (A ⇒ B) {
  @inline override def apply(a: A): B = collAPI.foldLeft(chain)(a: Any)((x, f) ⇒ f(x)).asInstanceOf[B]

  @inline override def andThen[C](g: B ⇒ C): A ⇒ C = before(g)

  @inline override def compose[C](g: C ⇒ A): C ⇒ B = after(g)

  private[example] def debugInfo: Coll[Int] = collAPI.fmap[CountCompose[Any, Any], Int](_.composedCount)(chain)

  // Composing this FastCompose value before `other` can be optimized only if this value has length 1.
  @inline def before[C](other: B ⇒ C): FastCompose[A, C, Coll] = FastCompose.andThen(this, FastCompose.of(other))

  // In this case, we may optimize if the first element of the chain has less than max compositions.
  @inline def after[C](other: C ⇒ A): FastCompose[C, B, Coll] = FastCompose.andThen(FastCompose.of(other), this)
}

trait CollectionAPI[Coll[_]] {
  def foldLeft[A, R](coll: Coll[A])(init: R)(update: (R, A) ⇒ R): R

  def head[A](c: Coll[A]): A

  def replaceHead[A](c: Coll[A], a: A): Coll[A]

  def pure[A](a: A): Coll[A]

  def concat[A](c1: Coll[A], c2: Coll[A]): Coll[A]

  def append[A](c: Coll[A], a: A): Coll[A]

  def prepend[A](a: A, c: Coll[A]): Coll[A]

  def fmap[A, B](f: A ⇒ B): Coll[A] ⇒ Coll[B]

  def isLengthOne[A](c: Coll[A]): Boolean

  def directCompositionLimit: Int
}

object CollectionAPI {
  def apply[Coll[_]](implicit collAPI: CollectionAPI[Coll]): CollectionAPI[Coll] = collAPI

  // Define various instances here.
  def collList(limit: Int): CollectionAPI[List] = new CollectionAPI[List] {
    override def foldLeft[A, R](coll: List[A])(init: R)(update: (R, A) ⇒ R): R = coll.foldLeft(init)(update)

    override def head[A](c: List[A]): A = c.head

    override def replaceHead[A](c: List[A], a: A): List[A] = a :: c.tail

    override def pure[A](a: A): List[A] = List(a)

    override def concat[A](c1: List[A], c2: List[A]): List[A] = c1 ++ c2

    override def append[A](c: List[A], a: A): List[A] = c :+ a

    override def prepend[A](a: A, c: List[A]): List[A] = a +: c

    override def fmap[A, B](f: A ⇒ B): List[A] ⇒ List[B] = _.map(f)

    override def isLengthOne[A](c: List[A]): Boolean = c.lengthCompare(1) == 0

    override def directCompositionLimit: Int = limit
  }

  //  implicit val collApiList: CollectionAPI[List] = collList(100)
  implicit val collApi: CollectionAPI[Vector] = collVector(100)

  def collVector(limit: Int): CollectionAPI[Vector] = new CollectionAPI[Vector] {
    override def foldLeft[A, R](coll: Vector[A])(init: R)(update: (R, A) ⇒ R): R = coll.foldLeft(init)(update)

    override def head[A](c: Vector[A]): A = c.head

    override def replaceHead[A](c: Vector[A], a: A): Vector[A] = a +: c.tail

    override def pure[A](a: A): Vector[A] = Vector(a)

    override def concat[A](c1: Vector[A], c2: Vector[A]): Vector[A] = c1 ++ c2

    override def append[A](c: Vector[A], a: A): Vector[A] = c :+ a

    override def prepend[A](a: A, c: Vector[A]): Vector[A] = a +: c

    override def fmap[A, B](f: A ⇒ B): Vector[A] ⇒ Vector[B] = _.map(f)

    override def isLengthOne[A](c: Vector[A]): Boolean = c.lengthCompare(1) == 0

    override def directCompositionLimit: Int = limit
  }

}

