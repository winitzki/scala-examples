package example

import cats.instances.equiv
import io.chymyst.ch._

trait SymbolicLawChecking {
  def checkFlattenAssociativity(fmap: TermExpr, flatten: TermExpr): Boolean = {
    // fmap ftn . ftn = ftn . ftn
    (flatten :@@ flatten) equiv ((fmap :@ flatten) :@@ flatten)
  }

  def checkPureFlattenLaws(fmap: TermExpr, pure: TermExpr, flatten: TermExpr): Boolean = {
    // pure . ftn = id
    val pf = (pure :@@ flatten).simplify.prettyRename // pf: F[A] ⇒ F[A]
    val faType = pf.t.asInstanceOf[#->].head // This should fail if pf is not a function.
    val x = freshVar[Nothing].copy(t = faType)
    val idFA = x =>: x

    // fmap pure . ftn = id
    val fpf = (fmap :@ pure) :@@ flatten
    (pf equiv idFA) && (fpf equiv idFA)
  }
}
