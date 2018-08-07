import fastparse.all._
import fastparse.core.Parsed.Success
import java.time.LocalTime

final case class Subtitle(index: Int, beginTime: LocalTime, endTime: LocalTime, text: String)

def ts(a: Int, b: Int, c: Int, d: Int): LocalTime = LocalTime.of(a, b, c, d * 1000000)

def d(n: Int) = P(CharIn('0' to '9').rep(exactly = n).!).map(_.toInt)

val tStamp = P(d(2) ~ ":" ~ d(2) ~ ":" ~ d(2) ~ "," ~ d(3))

val timeLine = P(tStamp ~ " --> " ~ tStamp)

val textLine = P(CharsWhile(_ != '\n').!)

val ds = P(CharIn('0' to '9').rep(min = 1).!).map(_.toInt)

val srtBlock = P(ds ~ "\n" ~ timeLine ~ "\n" ~ textLine ~ "\n\n")
  .map { case (index, (b1, b2, b3, b4, (e1, e2, e3, e4)), text) ⇒ Subtitle(index, ts(b1, b2, b3, b4), ts(e1, e2, e3, e4), text) }

val srtFile: Parser[Seq[Subtitle]] = P(srtBlock.rep(min = 1) ~ "\n".rep ~ End)

@main
def main(srcFile: String) = {
  import scala.reflect.io._

  val src = new String(Streamable.bytes(File(srcFile).inputStream), "UTF-8")
  val Success(result, _) = srtFile.parse(src)

  println(
    result.map(_.text).mkString("\n")
  )
}
