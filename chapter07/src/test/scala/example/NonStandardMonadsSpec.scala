package example

import cats.derived.pure
import io.chymyst.ch._
import org.scalatest.{FlatSpec, Matchers}

class NonStandardMonadsSpec extends FlatSpec with Matchers with SymbolicLawChecking {

  behavior of "nonstandard semimonads and monads"

  it should "check associativity for the nonstandard Pair semimonads" in {

    type Pair[A] = (A, A)

    def fmap[A, B] = ofType[(A ⇒ B) ⇒ Pair[A] ⇒ Pair[B]].lambdaTerm

    def flatten[A] = anyOfType[Pair[Pair[A]] ⇒ Pair[A]]()

    val terms = flatten

    terms.length shouldEqual 16

    // Select the implementations that satisfy rigorously the associativity law.
    // fmap ftn . ftn = ftn . ftn

    def tA[A] = freshVar[A]

    def tC[C] = freshVar[C]

    def tCC[C] = freshVar[Pair[C]]

    def associativeTerms[A] = flatten[A].filter { ftn ⇒
      checkFlattenAssociativity(fmap, ftn.lambdaTerm)
    }

    println("Semimonads:")
    associativeTerms[Int].map(_.lambdaTerm.prettyPrint).foreach(println)
    associativeTerms[Int].length shouldEqual 7 // One standard and six non-standard semimonads.
    /*
a ⇒ a._2 // Choose second outer tuple.
a ⇒ a._1 // Choose first outer tuple.
a ⇒ Tuple2(a._1._1, a._2._2) // The standard monad.
a ⇒ Tuple2(a._1._2, a._2._2) // Choose second inner tuple.
a ⇒ Tuple2(a._1._1, a._2._1) // Choose first inner tuple.
a ⇒ Tuple2(a._1._1, a._1._1) // Choose first element of first inner tuple.
a ⇒ Tuple2(a._2._2, a._2._2) // Choose second element of second inner tuple.
     */

    // Of these, select the implementations that satisfy rigorously the two identity laws.
    // pure . ftn = id
    // fmap pure . ftn = id

    def pure[A] = ofType[A ⇒ Pair[A]].lambdaTerm

    def monadTerms[A] = associativeTerms[A].filter { ftn ⇒
      checkPureFlattenLaws(fmap, pure, ftn.lambdaTerm)
    }

    println("Monads:")
    monadTerms[Int].map(_.lambdaTerm.prettyPrint).foreach(println)
    monadTerms.length shouldEqual 1
  }

  it should "discover the A + A x A x A monad" in {
    type P[A] = Either[A, (A, A, A)]

    def fmap[A, B] = ofType[(A ⇒ B) ⇒ P[A] ⇒ P[B]].lambdaTerm

    def flattens[A] = anyOfType[P[P[A]] ⇒ P[A]]()

    def pure[A] = ofType[A ⇒ P[A]].lambdaTerm

    println(flattens.length)
    println(pure.prettyRenamePrint)

//    val goodMonads: Seq[(TermExpr, TermExpr)] = for {
//      ftn ← flattens
//      if checkFlattenAssociativity(fmap, ftn)
//      pure ← pures
//      if checkPureFlattenLaws(fmap, pure, ftn)
//    } yield (pure, ftn)
//
//    println(goodMonads.map(_._1.prettyRenamePrint), goodMonads.map(_._2.prettyRenamePrint))
  }

}
