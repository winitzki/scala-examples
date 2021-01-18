package example

import cats.data.Chain

import java.util.ArrayList
import scala.collection.mutable.ArrayBuffer

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
  @inline def of[A, B, Coll[_] : CollectionAPI](f: A ⇒ B): FastCompose[A, B, Coll] = f match {
    case p@FastCompose(_) ⇒ p.asInstanceOf[FastCompose[A, B, Coll]]
    case _ ⇒ FastCompose[A, B, Coll](CollectionAPI[Coll].pure(CountCompose(f.asInstanceOf[Any ⇒ Any])))
  }

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

  private[example] def debugInfo: List[Int] = collAPI.foldLeft(chain)(Nil: List[Int])((l, a) ⇒ l :+ a.composedCount)

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

  def collChain(limit: Int): CollectionAPI[Chain] = new CollectionAPI[Chain] {
    override def foldLeft[A, R](coll: Chain[A])(init: R)(update: (R, A) ⇒ R): R = coll.foldLeft(init)(update)

    override def head[A](c: Chain[A]): A = c.headOption.get

    override def replaceHead[A](c: Chain[A], a: A): Chain[A] = {
      val (_, tail) = c.uncons.get
      tail.prepend(a)
    }

    override def pure[A](a: A): Chain[A] = Chain.one(a)

    override def concat[A](c1: Chain[A], c2: Chain[A]): Chain[A] = c1.concat(c2)

    override def append[A](c: Chain[A], a: A): Chain[A] = c.append(a)

    override def prepend[A](a: A, c: Chain[A]): Chain[A] = c.prepend(a)

    override def fmap[A, B](f: A ⇒ B): Chain[A] ⇒ Chain[B] = _.map(f)

    override def isLengthOne[A](c: Chain[A]): Boolean = {
      val iter = c.iterator
      iter.hasNext && {
        iter.next()
        !iter.hasNext
      }
    }

    override def directCompositionLimit: Int = limit
  }

  def collArrayList(limit: Int): CollectionAPI[ArrayList] = new CollectionAPI[ArrayList] {
    override def foldLeft[A, R](coll: ArrayList[A])(init: R)(update: (R, A) ⇒ R): R = {
      var result = init
      for {i ← 0 until coll.size} {
        result = update(result, coll.get(i))
      }
      result
    }

    override def head[A](c: ArrayList[A]): A = c.get(0)

    override def replaceHead[A](c: ArrayList[A], a: A): ArrayList[A] = {
      c.set(0, a)
      c
    }

    override def pure[A](a: A): ArrayList[A] = {
      val result = new ArrayList[A](1)
      result.add(a)
      result
    }

    override def concat[A](c1: ArrayList[A], c2: ArrayList[A]): ArrayList[A] = {
      c1 addAll c2
      c1
    }

    override def append[A](c: ArrayList[A], a: A): ArrayList[A] = {
      c.add(a)
      c
    }

    override def prepend[A](a: A, c: ArrayList[A]): ArrayList[A] = {
      c.add(0, a)
      c
    }

    override def fmap[A, B](f: A ⇒ B): ArrayList[A] ⇒ ArrayList[B] = ???

    override def isLengthOne[A](c: ArrayList[A]): Boolean = c.size == 1

    override def directCompositionLimit: Int = limit
  }

  def collArrayBuffer(limit: Int): CollectionAPI[ArrayBuffer] = new CollectionAPI[ArrayBuffer] {
    override def foldLeft[A, R](coll: ArrayBuffer[A])(init: R)(update: (R, A) ⇒ R): R = coll.foldLeft(init)(update)

    override def head[A](c: ArrayBuffer[A]): A = c(0)

    override def replaceHead[A](c: ArrayBuffer[A], a: A): ArrayBuffer[A] = {
      c(0) = a
      c
    }

    override def pure[A](a: A): ArrayBuffer[A] = ArrayBuffer(a)

    override def concat[A](c1: ArrayBuffer[A], c2: ArrayBuffer[A]): ArrayBuffer[A] = c1 ++ c2

    override def append[A](c: ArrayBuffer[A], a: A): ArrayBuffer[A] = c :+ a

    override def prepend[A](a: A, c: ArrayBuffer[A]): ArrayBuffer[A] = a +: c

    override def fmap[A, B](f: A ⇒ B): ArrayBuffer[A] ⇒ ArrayBuffer[B] = _.map(f)

    override def isLengthOne[A](c: ArrayBuffer[A]): Boolean = c.lengthCompare(1) == 0

    override def directCompositionLimit: Int = limit
  }
}

