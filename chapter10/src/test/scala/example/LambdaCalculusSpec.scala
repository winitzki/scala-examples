package example

import org.scalatest.{FlatSpec, Matchers}

object LambdaCalculus1 {
  sealed trait LC

  final case class Var(name: String) extends LC {
    override def toString: String = name
  }

  final case class Lam(arg: Var, body: LC) extends LC {
    override def toString: String = s"(λ$arg → $body)"
  }

  final case class App(func: LC, arg: LC) extends LC {
    override def toString: String = s"$func $arg"
  }

  val zero : LC = Lam(Var("f"), Lam(Var("x"), Var("x")))

  val succ : LC = Lam(Var("n"), Lam(Var("f"), Lam(Var("x"), App(Var("f"), App(App(Var("n"), Var("f")), Var("x"))))))
}

class LambdaCalculusSpec extends FlatSpec with Matchers {

  it should "pretty-print terms" in {
    LambdaCalculus1.zero.toString shouldEqual "(λf → (λx → x))"
  }

  it should "evaluate n - n in the Church encoding, using 1st order LC" in {

  }
}
