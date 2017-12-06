package example

import scala.language.experimental.macros
import scala.reflect.macros.blackbox
import scala.reflect.macros.whitebox

object CurryHoward {

  private[example] def testType[T]: (String, String) = macro testTypeImpl[T]

  def matchType(c: blackbox.Context)(t: c.Type): String = {
    
    t match {
      case _ if t.typeSymbol.fullName == "scala.Function1" ⇒
        val args = t.typeArgs
        s"${matchType(c)(args(0))} ..=>.. ${matchType(c)(args(1))}"
      case _ if !t.typeSymbol.isConstructor ⇒ t.toString
      case _ ⇒ s"<unrecognized so far>$t"
    }
  }

  def testTypeImpl[T: c.WeakTypeTag](c: blackbox.Context): c.Expr[(String, String)] = {
    import c.universe._

    val typeT: c.Type = c.weakTypeOf[T]
    val enclosingType = c.internal.enclosingOwner.typeSignature

    val s1 = matchType(c)(typeT.resultType)
    val s2 = matchType(c)(enclosingType)

    c.Expr[(String, String)](q"($s1,$s2)")
  }

  def inhabit[T]: T = macro inhabitImpl[T]

  def inhabit1[T]: T = macro inhabitImpl1[T]

  def inhabitImpl[T: c.WeakTypeTag](c: blackbox.Context): c.Expr[T] = {
    import c.universe._
    // TODO: implement
    val typeT: c.Type = c.weakTypeOf[T]
//
    typeT match {
      case tq"..$a => $b" ⇒
      case _ ⇒
    }
    c.Expr[T](q"null.asInstanceOf[$typeT]")
  }

  def inhabitImpl1[T](c: whitebox.Context): c.Tree = {
    import c.universe._
    // TODO: implement

    val enclosingType = c.internal.enclosingOwner.typeSignature
    q"null.asInstanceOf[$enclosingType]"
  }
}

sealed trait Term {
  def apply[T](x: T): Term = this // dummy implementation to simplify code
}

final case class \[T](v: Term ⇒ T) extends Term

final case class \:[T](v: Any ⇒ T) extends Term
