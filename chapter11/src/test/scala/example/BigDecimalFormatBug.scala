package example

import java.math.MathContext
import java.text.DecimalFormat

import org.scalatest.{FlatSpec, Matchers}

class BigDecimalFormatBug extends FlatSpec with Matchers {

  behavior of "BigDecimal format"

  // The bug is discussed here: https://github.com/scala/bug/issues/9670
  it should "convert without loss" in {
    val df = new DecimalFormat()
    df.setMaximumFractionDigits(2)
    df.setMinimumFractionDigits(2)
    df.format(BigDecimal("70368744177664.09", MathContext.UNLIMITED).underlying()) shouldEqual "70,368,744,177,664.09"
    df.format(BigDecimal("70368744177664.09", MathContext.UNLIMITED)) shouldEqual "70,368,744,177,664.10"
  }

  it should "round correctly" in {
    val f = new java.text.DecimalFormat("#,##0.000")
    f.setRoundingMode(java.math.RoundingMode.HALF_UP)
    f.format(BigDecimal("1.2345")) shouldEqual "1.234" // Actually should be 1.235 if rounded correctly.
    f.format(new java.math.BigDecimal("1.2345")) shouldEqual "1.235" // This is the workaround: use java.math.BigDecimal instead of scala.math.BigDecimal to have correct behavior.
  }
}
