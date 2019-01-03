package example

import cats.data.AndThen
import cats.{Functor, Monoid, ~>}
import cats.kernel.Semigroup
import cats.syntax.monoid._
import cats.syntax.functor._
import org.scalatest.{FlatSpec, Matchers, run}

import scala.annotation.tailrec
import scala.util.Try

class Chapter11_01_examplesSpec extends FlatSpec with Matchers {

  behavior of "monad transformers"

}
