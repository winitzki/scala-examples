package example

import cats.instances.equiv
import io.chymyst.ch._

trait SymbolicLawChecking {

  def checkFmap(fmap: TermExpr): Boolean = {
    // fmap id = id
    // (fmap f) . (fmap g) = fmap (f . g)
    ???
  }

  def checkFlattenAssociativity(fmap: TermExpr, flatten: TermExpr): Boolean = {
    // fmap ftn . ftn = ftn . ftn
    val lhs = flatten :@@ flatten
    val rhs = (fmap :@ flatten) :@@ flatten
            println(s"check associativity laws for flatten = ${flatten.prettyPrint}:\n\tlhs = ${lhs.simplify.prettyRenamePrint}\n\trhs = ${rhs.simplify.prettyRenamePrint}")
    lhs equiv rhs
  }

  def checkPureFlattenLaws(fmap: TermExpr, pure: TermExpr, flatten: TermExpr): Boolean = {
    // pure . ftn = id
    val pf = (pure :@@ flatten).simplify.prettyRename // pf: F[A] ⇒ F[A]
    val faType = pf.t.asInstanceOf[#->].head // This should fail if pf is not a function.
    val x = VarE("x", faType)
    val idFA = x =>: x

    // fmap pure . ftn = id
    val fpf = (fmap :@ pure) :@@ flatten

        println(s"check identity laws for pure = ${pure.prettyPrint} and flatten = ${flatten.prettyPrint}:\n\tlhs1 = ${pf.simplify.prettyPrint}\n\trhs1 = ${idFA.simplify.prettyPrint}\n\tlhs2 = ${fpf.simplify.prettyPrint}\n\trhs2 = ${idFA.simplify.prettyPrint}")
    (pf equiv idFA) && (fpf equiv idFA)
  }
}
