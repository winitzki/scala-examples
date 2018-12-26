package example

import org.scalatest.{FlatSpec, Matchers}
import cats.{Applicative, Bifunctor, Bitraverse, Eval, Functor, Monoid, Traverse}
import org.scalatest.{FlatSpec, Matchers}
import cats.syntax.functor._
import cats.instances._
import cats.syntax.bifunctor._
import cats.syntax.traverse._
import cats.syntax.monoid._
import cats.syntax.bitraverse._
import WuZip.WuZipSyntax
import Trav._
import cats.data.State
import io.chymyst.ch._

class Chapter10_03_examplesSpec extends FlatSpec with Matchers {

  behavior of "examples of free typeclass constructions"

  it should "implement a free contrafunctor" in {
  
  }

  it should "implement a free pointed functor" in {

  }
  
  it should "implement a free filterable functor" in {

  }

  it should "implement a free monadic functor" in {

  }

  it should "implement a free applicative functor" in {

  }

  it should "combine two operation constructors in a free functor" in {
    
  }
  
  it should "combine a free monad and a free applicative functor" in {

  }
}
