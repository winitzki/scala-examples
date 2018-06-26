package example

import org.scalatest.{FlatSpec, Matchers}

class Chapter08_01_parsersSpec extends FlatSpec with Matchers {

  behavior of "parsers"

  /* Language examples:
     123 <EOF>
     <sqrt>121</sqrt> <EOF>
     <sqrt><sqrt>10000</sqrt></sqrt> <EOF>
     The tags must be balanced. The number must be inside all tags.
     
     Examples of invalid strings that we want to flag:
     <sqrt>121<sqrt> <EOF> (tag not closed)
     </sqrt>10</sqrt> <EOF> (tag not opened)
     <sqrt>100</sqrt></sqrt> <EOF> (junk at end of text)
  */

  type Err = List[String]

  val NoNumber = List("no number")

  val NoContent = List("no content")

  val NotClosed = List("tag not closed")

  val NotOpened = List("tag not opened")

  val JunkAtEnd = List("junk at end")

  // The Parser may consume a part of the string and return an error or a result.
  // The result is the tag text or the number.
  case class Parser[A](run: String ⇒ (Either[Err, A], String))

  val eofOK: Parser[Unit] = Parser { s ⇒
    val result = if (s.isEmpty) Right(()) else Left(JunkAtEnd)
    (result, s)
  }

  val eofError: Parser[Unit] = Parser { s ⇒
    val result = if (s.isEmpty) Left(NoContent) else Right(())
    (result, s)
  }

  val NumRegex = "^([0-9]+)(.*)$".r

  val number: Parser[Int] = Parser {
    case NumRegex(num, rest) ⇒
      (Right(num.toInt), rest)
    case s ⇒ (Left(NoNumber), s)
  }

  val TagRegex = "^(</?sqrt>)(.*)$".r

  val openTag: Parser[String] = Parser {
    case TagRegex(tag, rest) ⇒
      if (tag == "<sqrt>")
        (Right(tag), rest)
      else
        (Left(NotOpened), rest)
    case s ⇒
      (Left(NotOpened), s)
  }

  val closeTag: Parser[String] = Parser {
    case TagRegex(tag, rest) ⇒
      if (tag == "</sqrt>")
        (Right(tag), rest)
      else
        (Left(NotClosed), rest)
    case s ⇒
      (Left(NotClosed), s)
  }

  val test0 = "123"
  val test1 = "<sqrt>121</sqrt>"
  val test2 = "<sqrt><sqrt>10000</sqrt></sqrt>"
  val testNoNumber = "<sqrt></sqrt>"
  val testNotClosed = "<sqrt><sqrt>10000</sqrt>"
  val testNotOpened = "</sqrt>"
  val testJunkAtEnd = "<sqrt>10000</sqrt></sqrt>"

  // Parser combinators.

  // Applicative combinator ("zip").
  implicit class ParserCombinators[A](parserA: Parser[A]) {
    def &[B](parserB: Parser[B]): Parser[(A, B)] = Parser { s ⇒
      val (resultA, rest) = parserA.run(s)
      val (resultB, restB) = parserB.run(rest)
      val result = (resultA, resultB) match {
        case (Right(x), Right(y)) ⇒ Right((x, y))
        case (Left(x), Right(_)) ⇒ Left(x)
        case (Right(_), Left(y)) ⇒ Left(y)
        case (Left(x), Left(y)) ⇒ Left(x ++ y)
      }
      (result, restB)
    }

    // Monadic combinator that ignores the result of the previous parser.
    def &&[B](parserB: Parser[B]): Parser[B] = flatMap(_ ⇒ parserB)

    def |(parserB: ⇒ Parser[A]): Parser[A] = Parser { s ⇒
      val (resultA, rest) = parserA.run(s)
      resultA match {
        case Right(_) ⇒ (resultA, rest)
        case Left(_) ⇒ parserB.run(s)
      }
    }

    def map[B](f: A ⇒ B): Parser[B] = Parser { s ⇒
      val (result, rest) = parserA.run(s)
      (result.map(f), rest)
    }

    def flatMap[B](f: A ⇒ Parser[B]): Parser[B] = Parser { s ⇒
      val (result, rest) = parserA.run(s)
      result match {
        case Left(err) ⇒ (Left(err), rest)
        case Right(x) ⇒ f(x).run(rest)
      }
    }
  }

  def languageRec: Parser[Double] = eofError && (number.map(_.toDouble) | (openTag && languageRec & closeTag).map { case (x, _) ⇒ math.sqrt(x) })

  val language: Parser[Double] = (languageRec & eofOK).map(_._1)

  def parseLanguage(s: String): Either[Err, Double] = language.run(s)._1

  it should "parse a simple language" in {
    number.run(test0) shouldEqual(Right(123), "")
    number.run(test1) shouldEqual(Left(NoNumber), test1)
    openTag.run(test0) shouldEqual(Left(NotOpened), test0)
    openTag.run(test1) shouldEqual(Right("<sqrt>"), "121</sqrt>")
    number.run("121</sqrt>") shouldEqual(Right(121), "</sqrt>")

    parseLanguage(test0) shouldEqual Right(123.0)
    parseLanguage(test1) shouldEqual Right(11.0)
    parseLanguage(test2) shouldEqual Right(10.0)
    parseLanguage(testNotClosed) shouldEqual Left(NotClosed)
    parseLanguage(testJunkAtEnd) shouldEqual Left(JunkAtEnd)

    parseLanguage(testNoNumber) shouldEqual Left(NotOpened ++ NotClosed ++ NotClosed)
    parseLanguage(testNotOpened) shouldEqual Left(NotOpened ++ NotClosed)

    parseLanguage("junk <sqrt>121") shouldEqual Left(NotOpened ++ NotClosed ++ JunkAtEnd)
  }

  /* Language:
    123
    <a>123</a>
    <c><b>123</b></c>
    Tag names can be arbitrary, and tags must be properly balanced.
   */
  val AnyTagRegex = "^(</?)([-a-zA-Z_][-A-Za-z0-9_]*)>(.*)$".r

  def NotClosedTag(tag: String, expectedTag: String): Err = List(s"tag $expectedTag not closed, got $tag instead")

  def NotOpenedTag(tag: String): Err = List(s"tag $tag closed instead of opened")

  val anyTag: Parser[String] = Parser {
    case AnyTagRegex(pre, tag, rest) ⇒
      if (pre == "<")
        (Right(tag), rest)
      else (Left(NotOpenedTag(tag)), rest)
    case s ⇒ (Left(NotOpened), s)
  }

  def closeTag(givenTag: String): Parser[String] = Parser {
    case AnyTagRegex(pre, tag, rest) ⇒
      if (tag == givenTag && pre == "</")
        (Right(tag), rest)
      else
        (Left(NotClosedTag(tag, givenTag)), rest)
    case s ⇒ (Left(NotClosed), s)
  }

  def language2Rec: Parser[Int] = eofError && (number | anyTag.flatMap(tag ⇒ language2Rec & closeTag(tag)).map(_._1))

  val language2: Parser[Int] = (language2Rec & eofOK).map(_._1)

  def parseLanguage2(s: String): Either[Err, Int] = language2.run(s)._1

  it should "parse a language that requires monadic combinators" in {
    parseLanguage2("<a>123</a>") shouldEqual Right(123)
    parseLanguage2("<a><b>123</b></a>") shouldEqual Right(123)
    parseLanguage2("<a><b><a>123</a></b></a>") shouldEqual Right(123)
    parseLanguage2("<a><b><a>123</a></b>") shouldEqual Left(NotClosed)
    parseLanguage2("<a><b>123</a>") shouldEqual Left(NotClosedTag("a", "b") ++ NotClosed)
    parseLanguage2("<a><b>123</a></b>") shouldEqual Left(NotClosedTag("a", "b") ++ NotClosedTag("b", "a"))
    parseLanguage2("junk <a>123") shouldEqual Left(NotOpened ++ JunkAtEnd) // We don't see "tag not closed" error.
  }
}
