package example

/** Stack-safe and fast function composition.
 *
 * Usage:
 *
 * val f: A => B = ...
 * val g: B => C = ...
 * val h = f before g
 * val x: A = ...
 * h(x) // Interface should be the same as for functions, but h is a FastCompose object.
 */
object FastCompose {
  // Define the specific collection type here?

  @inline def apply[A, B, Coll[_] : CollectionAPI](f: A ⇒ B): FastCompose[A, B, Coll] =
    new FastCompose[A, B, Coll](CollectionAPI[Coll].pure(Function1CountingComposed(f.asInstanceOf[Any ⇒ Any])))

  @inline def apply[A, B, Coll[_]](function1CountingComposed: Function1CountingComposed[A, B])(implicit collAPI: CollectionAPI[Coll]): FastCompose[A, B, Coll] =
    new FastCompose[A, B, Coll](collAPI.pure(function1CountingComposed.asInstanceOf[Function1CountingComposed[Any, Any]]))

  @inline implicit class FastComposeOps[A, B, Coll[_]](val f: A ⇒ B)(implicit collAPI: CollectionAPI[Coll]) {
    @inline def before[C](g: B ⇒ C): FastCompose[A, C, Coll] = new FastCompose(collAPI.pure(Function1CountingComposed((f andThen g).asInstanceOf[Any ⇒ Any])))

    @inline def after[C](g: C ⇒ A): FastCompose[C, A, Coll] = new FastCompose(collAPI.pure(Function1CountingComposed((f compose g).asInstanceOf[Any ⇒ Any])))
  }

}

// Wrap a function A => B and count the number of function compositions inside it.
private final case class Function1CountingComposed[
  @specialized(scala.Int, scala.Long, scala.Float, scala.Double) -A,
  @specialized(scala.Unit, scala.Boolean, scala.Int, scala.Float, scala.Long, scala.Double) +B
](f: A ⇒ B, composedCount: Int = 1) extends (A ⇒ B) {

  @inline override def apply(a: A): B = f(a)

  @inline override def andThen[C](other: B ⇒ C): Function1CountingComposed[A, C] = other match {
    case Function1CountingComposed(g, c) ⇒ Function1CountingComposed(f andThen g, composedCount + c)
    case _ ⇒ Function1CountingComposed(f andThen other, composedCount + 1)
  }

  @inline override def compose[C](other: C ⇒ A): Function1CountingComposed[C, B] = other match {
    case Function1CountingComposed(g, c) ⇒ Function1CountingComposed(f compose g, composedCount + c)
    case _ ⇒ Function1CountingComposed(f compose other, composedCount + 1)
  }
}


// The `chain` contains a sequence of counted-composed functions.
// The *first* function in the chain must have less than `directCompositionLimit` compositions.
final class FastCompose[-A, +B, Coll[_]] private(private val chain: Coll[Function1CountingComposed[Any, Any]])(implicit collAPI: CollectionAPI[Coll]) extends (A ⇒ B) {
  override def apply(a: A): B = collAPI.foldLeft(chain)(a: Any)((x, f) ⇒ f(x)).asInstanceOf[B]

  override def andThen[C](g: B ⇒ C): A ⇒ C = before(g)

  override def compose[C](g: C ⇒ A): C ⇒ B = after(g)

  private[example] def debugInfo: Coll[Int] = collAPI.fmap[Function1CountingComposed[Any, Any], Int](_.composedCount)(chain)

  // Composing this FastCompose value before `other` can be optimized only if this value has length 1.
  def before[C](other: B ⇒ C): FastCompose[A, C, Coll] = other match {
    case g: FastCompose[B, C, Coll] ⇒ new FastCompose(collAPI.concat(chain, g.chain))
    case f@Function1CountingComposed(g, c) ⇒ new FastCompose(collAPI.append(chain, f.asInstanceOf[Function1CountingComposed[Any, Any]]))
    case _ ⇒ new FastCompose(collAPI.append(chain, Function1CountingComposed(other.asInstanceOf[Any ⇒ Any])))
  }

  // In this case, we may optimize if the first element of the chain has less than max compositions.
  def after[C](other: C ⇒ A): FastCompose[C, B, Coll] = other match {
    case g: FastCompose[C, A, Coll] ⇒ new FastCompose(collAPI.concat(g.chain, chain))
    case f: Function1CountingComposed[C, A] ⇒
      val h = collAPI.head(chain)
      if (h.composedCount + f.composedCount <= collAPI.directCompositionLimit)
        new FastCompose(collAPI.replaceHead(chain, h compose f.asInstanceOf[Function1CountingComposed[Any, Any]]))
      else
        new FastCompose(collAPI.prepend(f.asInstanceOf[Function1CountingComposed[Any, Any]], chain))
    case _ ⇒
      val h = collAPI.head(chain)
      if (h.composedCount + 1 <= collAPI.directCompositionLimit)
        new FastCompose(collAPI.replaceHead(chain, h compose other.asInstanceOf[Any ⇒ Any]))
      else
        new FastCompose(collAPI.prepend(Function1CountingComposed(other.asInstanceOf[Any ⇒ Any]), chain))
  }
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

  implicit val collApiList: CollectionAPI[List] = collList(100)

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

