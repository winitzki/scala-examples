package example

import cats.data.Chain

// Stack-safe function composition.
// Inspired by `https://mpilquist.github.io/blog/2017/03/11/stackless-function-composition/` and by `cats.util.AndThen`.
object SafeCompose {
  implicit def toChainF1[A, B](f: A ⇒ B): ChainF1[A, B] = ChainF1(f)
}


final class ChainF1[-A, +B] private(private val fs: Chain[Any ⇒ Any]) extends (A ⇒ B) {

  override def apply(a: A): B =
    fs.foldLeft(a: Any)((x, f) ⇒ f(x)).asInstanceOf[B]

  override def compose[C](g: C ⇒ A): ChainF1[C, B] = g match {
    case gc: ChainF1[C, A] ⇒ new ChainF1(gc.fs ++ fs)
    case _ ⇒ new ChainF1(g.asInstanceOf[Any ⇒ Any] +: fs)
  }

  override def andThen[C](g: B ⇒ C): ChainF1[A, C] = g match {
    case gc: ChainF1[B, C] ⇒ new ChainF1(fs ++ gc.fs)
    case _ ⇒ new ChainF1(fs :+ g.asInstanceOf[Any ⇒ Any])
  }
  
  def after[C](g: C ⇒ A): ChainF1[C, B] = compose(g)

  def before[C](g: B ⇒ C): ChainF1[A, C] = andThen(g)
}

object ChainF1 {
  def apply[A, B](f: A ⇒ B): ChainF1[A, B] = f match {
    case f: ChainF1[A, B] ⇒ f
    case _ ⇒ new ChainF1(Chain.one(f.asInstanceOf[Any => Any]))
  }
}

