package example

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

  it should "check the A + A x A x A monad by hand" in {
    type P[A] = Either[A, (A, A, A)]

    def fmap[A, B] = ofType[(A ⇒ B) ⇒ P[A] ⇒ P[B]].lambdaTerm

    fmap.prettyPrint shouldEqual "a ⇒ b ⇒ b match { c ⇒ (Left(a c.value) + 0); d ⇒ (0 + Right(Tuple3(a d.value._1, a d.value._2, a d.value._3))) }"

    //    def flattens[A] = anyOfType[P[P[A]] ⇒ P[A]]()

    def pure[A] = ofType[A ⇒ P[A]].lambdaTerm

    //    println(flattens.length)
    pure.prettyPrint shouldEqual "a ⇒ (Left(a) + 0)"

    //    val goodMonads: Seq[(TermExpr, TermExpr)] = for {
    //      ftn ← flattens
    //      if checkFlattenAssociativity(fmap, ftn)
    //      pure ← pures
    //      if checkPureFlattenLaws(fmap, pure, ftn)
    //    } yield (pure, ftn)
    //
    //    println(goodMonads.map(_._1.prettyRenamePrint), goodMonads.map(_._2.prettyRenamePrint))
  }

  it should "check the 1 + A x A x A monad by hand" in {
    type P[A] = Option[(A, A, A)]

    def fmap[A, B] = ofType[(A ⇒ B) ⇒ P[A] ⇒ P[B]].lambdaTerm

    fmap.prettyPrint shouldEqual "a ⇒ b ⇒ b match { c ⇒ (None() + 0); d ⇒ (0 + Some(Tuple3(a d.value._1, a d.value._2, a d.value._3))) }"

    //    def flattens[A] = anyOfType[P[P[A]] ⇒ P[A]]() // too slow!

    def pure[A] = ofType[A ⇒ P[A]].lambdaTerm

    pure.prettyPrint shouldEqual "a ⇒ (0 + Some(Tuple3(a, a, a)))"

    // Construct flatten by hand.
    def ppa[A] = freshVar[P[P[A]]]

    def pa[A] = freshVar[P[A]]

    val none = freshVar[None.type]
    val ppaNone = ppa(none)
    val paNone = pa(none)
    ppaNone.t.prettyPrint shouldEqual "Option[Tuple3[Option[Tuple3[A,A,A]],Option[Tuple3[A,A,A]],Option[Tuple3[A,A,A]]]]"
    ppa.t.prettyPrint shouldEqual ppaNone.t.prettyPrint

    def triplePa[A] = freshVar[Some[(P[A], P[A], P[A])]]

    def tripleA0[A] = freshVar[Some[(A, A, A)]]

    def tripleA1[A] = freshVar[Some[(A, A, A)]]

    def tripleA2[A] = freshVar[Some[(A, A, A)]]

    val ftn = (
      ppa =>: ppa.cases(
        none =>: paNone,
        triplePa =>: triplePa("value")(0).cases(
          none =>: paNone,
          tripleA0 =>: triplePa("value")(1).cases(
            none =>: paNone,
            tripleA1 =>: triplePa("value")(2).cases(
              none =>: paNone,
              tripleA2 =>:
                pa(tripleA0(tripleA0("value").t(tripleA0("value")(0), tripleA1("value")(1), tripleA2("value")(2))))
            )
          )
        )
      )
      ).prettyRename
    ftn.t.prettyPrint shouldEqual "Option[Tuple3[Option[Tuple3[A,A,A]],Option[Tuple3[A,A,A]],Option[Tuple3[A,A,A]]]] ⇒ Option[Tuple3[A,A,A]]"
    ftn.prettyPrint shouldEqual "a ⇒ a match { b ⇒ (b + 0); c ⇒ c.value._1 match { d ⇒ (d + 0); e ⇒ c.value._2 match { f ⇒ (f + 0); g ⇒ c.value._3 match { h ⇒ (h + 0); i ⇒ (0 + Some(Tuple3(e.value._1, g.value._2, i.value._3))) } } } }"
    // Fails due to impossibility of comparing functions.
    checkFlattenAssociativity(fmap, ftn) shouldEqual true
    checkPureFlattenLaws(fmap, pure, ftn) shouldEqual true

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
