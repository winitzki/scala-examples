package example

import org.scalatest.{FlatSpec, Matchers}

class Chapter10_PHOAS5_Cata extends FlatSpec with Matchers {
  // Follow the paper "Boxes go bananas".
  sealed trait ExpF[A]

  final case class Var[A](name: String) extends ExpF[A]

  final case class Lam[A](fun: A => A) extends ExpF[A]

  final case class App[A](fun: A, arg: A) extends ExpF[A]

  def xmap_ExpF[A, B](f: A => B, g: B => A): ExpF[A] => ExpF[B] = {
    case Var(name) => Var((name))
    case Lam(fun) => Lam(b => f(fun(g(b))))
    case App(fun, arg) => App(f(fun), f(arg))
  }

  type ExpC[A] = (ExpF[A] => A) => A // Church encoding

  def pureC[A]: A => ExpC[A] = a => { _ => a }

  def cata[A](alg: ExpF[A] => A)(p: ExpC[A]): A = p(alg)

  // "roll" is analogous to "fix : F T -> T"
  def roll[A](p: ExpF[ExpC[A]]): ExpC[A] = { f =>
    f(xmap_ExpF(cata(f), pureC[A])(p)) // need type annotation for pureC[A]
  }

  def v[A](name: String): ExpC[A] = roll(Var(name))

  def lam[A](f: ExpC[A] => ExpC[A]): ExpC[A] = roll(Lam(f))

  def app[A](f: ExpC[A], arg: ExpC[A]): ExpC[A] = roll(App(f, arg))

  trait Exp {
    def run[A]: ExpC[A]
  }

  def print(exp: ExpC[String]): String = cata[String] {
    case Var(name) => name
    case Lam(fun) => s"(λ(x) → ${fun("x")})"
    case App(fun, arg) => s"$fun $arg"
  }(exp)

  it should "apply identity function to a variable" in {
    val term = app(lam[String](x => x), v("x"))
    print(term) shouldEqual "(λ(x) → x) x"
  }
}
