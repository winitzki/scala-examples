package example

import cats.{Applicative, Bifunctor, Bitraverse, Functor}
import org.scalatest.{FlatSpec, Matchers}
import cats.syntax.functor._
import cats.syntax.bifunctor._
import cats.syntax.bitraverse._
import WuZip.WuZipSyntax
import Trav.TravSyntax
import io.chymyst.ch._

class Chapter10_02_examplesSpec extends FlatSpec with Matchers {

  behavior of "Church encoding constructions"

  it should "define Church encoding of List[Int]" in {

  }

  it should "define Church encoding of Option[A]" in {

  }

  it should "define Church encoding of List[A]" in {

  }

  it should "define Church encoding of free typeclass for any inductive typeclass" in {

  }

  it should "define tree encoding of free typeclass for any inductive typeclass" in {

  }

}
