package example

import example.TypeSafeMeasuresSpec.{FT, KG, KM, LB, MI, OZ, Quantity}
import org.scalatest.{FlatSpec, Matchers}

object TypeSafeMeasuresSpec {
  trait KG; trait LB; trait OZ; trait KM; trait MI; trait FT

  final case class Convertible[U1, U2](multiplier: Double) extends AnyVal
  implicit val cKG = Convertible[KG, KG](1.0)
  implicit val cLB = Convertible[LB, KG](0.453592)
  implicit val cOZ = Convertible[OZ, KG](0.0283495)
  implicit val cKM = Convertible[KM, KM](1.0)
  implicit val cMI = Convertible[MI, KM](1.60934)
  implicit val cFT = Convertible[FT, KM](0.0003048)

  final case class Quantity[U](value: Double) { // Constant functor.
    def +[U2, C](q: Quantity[U2])(implicit ev: Convertible[U, C], ev2: Convertible[U2, C]) =
      Quantity[U2](value * ev.multiplier / ev2.multiplier + q.value)

    def ==[U2, C](q: Quantity[U2])(implicit ev: Convertible[U, C], ev2: Convertible[U2, C]) =
      value * ev.multiplier == q.value * ev2.multiplier
  }

  implicit class QuantityPostfix(x: Double) {
    def kg = Quantity[KG](x)
    def lb = Quantity[LB](x)
    def oz = Quantity[OZ](x)
    def km = Quantity[KM](x)
    def mi = Quantity[MI](x)
    def ft = Quantity[FT](x)
  }

}

class TypeSafeMeasuresSpec extends FlatSpec with Matchers {

  import TypeSafeMeasuresSpec._

  "type-safe units" should "implement length and mass" in {
    1.kg + 1.lb + 1.kg shouldEqual 2.453592.kg
    3.mi + 1.km + 2.mi + 2.km shouldEqual 11.0467.km
    3.mi + 1.km + 2.mi + 2.km + 1.mi shouldEqual 7.86411820994942.mi
    "1.kg + 1.mi" shouldNot compile
    1.kg + 16.oz + 0.kg shouldEqual 1.lb + 1.kg
    1.kg + 16.oz == 1.lb + 1.kg shouldEqual true
  }
}
