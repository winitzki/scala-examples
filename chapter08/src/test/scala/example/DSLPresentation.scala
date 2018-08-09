package example

import fastparse.all
import org.scalatest.{FlatSpec, Matchers}

class DSLPresentation extends FlatSpec with Matchers {

  behavior of "symbolic computation DSL"

  /* Symbolic algebraic expressions with `Double` coefficients:
  
  val c = Const(1.23) // or just 1.23
  val x = Var("x") // or: val x = newVar
  val z = (x + c)*(x + c)*(x + c)
  val zprime = z.diff(x) // z %% x?
 
 // Substitution: zprime % (x -> 3.21)
  zprime.subs(x, Const(3.21)).value shouldEqual ???
  zprime.print shouldEqual "(x + ???)"
  
  Let's implement this DSL now.
  
   */
  sealed trait Expr {
    def +(y: Expr): Expr = Add(this, y)

    def *(y: Expr): Expr = Mul(this, y)

    def value: Option[Double]

    def diff(x: Var): Expr

    def print: String

    def subs(v: Var, x: Expr): Expr
  }

  final case class Const(c: Double) extends Expr {
    override def value: Option[Double] = Some(c)

    override def diff(x: Var): Expr = Const(0)

    override def print: String = c.toString

    override def subs(v: Var, x: Expr): Expr = this
  }

  final case class Var(name: String) extends Expr {
    override def value: Option[Double] = None

    override def diff(v: Var): Expr = if (name == v.name) Const(1) else Const(0)

    override def print: String = name

    override def subs(v: Var, x: Expr): Expr = if (name == v.name) x else this
  }

  final case class Add(x: Expr, y: Expr) extends Expr {
    override def value: Option[Double] = for {
      xv ← x.value
      yv ← y.value
    } yield xv + yv

    override def diff(v: Var): Expr = x.diff(v) + y.diff(v)

    override def print: String = x.print + " + " + y.print

    override def subs(v: Var, e: Expr): Expr = Add(x.subs(v, e), y.subs(v, e))
  }

  final case class Mul(x: Expr, y: Expr) extends Expr {
    override def value: Option[Double] = for {
      xv ← x.value
      yv ← y.value
    } yield xv * yv

    override def diff(v: Var): Expr = x.diff(v) * y + x * y.diff(v)

    override def print: String = s"(${x.print})*(${y.print})"

    override def subs(v: Var, e: Expr): Expr = Mul(x.subs(v, e), y.subs(v, e))
  }

  it should "do simple symbolic computations" in {
    val c = Const(1.23)
    val x = Var("x")
    val z = (x + c) * (x + c) * (x + c)
    val zprime = z.diff(x)

    zprime.subs(x, Const(3.21)).value.get shouldEqual 59.1408 +- 0.00001
    zprime.print shouldEqual "((1.0 + 0.0)*(x + 1.23) + (x + 1.23)*(1.0 + 0.0))*(x + 1.23) + ((x + 1.23)*(x + 1.23))*(1.0 + 0.0)"
  }

  // See spire.math.Algebraic.Expr for an implementation.


  behavior of "simple parsers in FastParse"

  it should "parse a SRT file using FastParse" in {
    val src = new String(scala.reflect.io.Streamable.bytes(getClass.getResourceAsStream("/test.srt")), "UTF-8")

    /*
    SRT file has this format:
    
    47
    00:02:22,840 --> 00:02:30,640
    of all is more less translated literally

    And these blocks are repeated one or more times.
    
    We want to extract the data from this format.
    
    Define the data structure:
     */
    import java.time.LocalTime
    case class Subtitle(index: Int, beginTime: LocalTime, endTime: LocalTime, text: String)

    def ts(a: Int, b: Int, c: Int, d: Int): LocalTime = LocalTime.of(a, b, c, d * 1000000)

    // Define parsers.

    import fastparse.all._
    import fastparse.core.Parsed.Success

    def d(n: Int) = P(CharIn('0' to '9').rep(exactly = n).!).map(_.toInt)

    val tStamp = P(d(2) ~ ":" ~ d(2) ~ ":" ~ d(2) ~ "," ~ d(3))

    val timeLine: Parser[(Int, Int, Int, Int, (Int, Int, Int, Int))] = 
      P(tStamp ~ " --> " ~ tStamp)

    val textLine = P(CharsWhile(_ != '\n').!)

    val ds = P(CharIn('0' to '9').rep(min = 1).!).map(_.toInt)

    val srtBlock = P(ds ~ "\n" ~ timeLine ~ "\n" ~ textLine ~ "\n\n")
      .map { case (index, (b1, b2, b3, b4, (e1, e2, e3, e4)), text) ⇒ Subtitle(index, ts(b1, b2, b3, b4), ts(e1, e2, e3, e4), text) }

    val srtFile: Parser[Seq[Subtitle]] = P(srtBlock.rep(min = 1) ~ "\n".rep ~ End)

    val Success(result, _) = srtFile.parse(src)

    result.map(_.toString) shouldEqual Seq(
      "Subtitle(47,00:02:22.840,00:02:30.640,of all is more less translated literally)",
      "Subtitle(48,00:02:25.840,00:02:32.860,in syntactically into code but in many)",
      "Subtitle(49,00:02:30.640,00:02:35.680,cases mathematical notation does not)"
    )

  }


}
