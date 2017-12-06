package example

import scala.language.experimental.macros
import scala.reflect.macros.blackbox
import scala.reflect.macros.whitebox

object CurryHoward {

  private[example] def testType[T]: (String, String) = macro testTypeImpl[T]

  val basicTypes = List("Int", "String", "Boolean", "Float", "Double", "Long", "Symbol", "Char")
  val basicRegex = s"(?:scala.|java.lang.)*(${basicTypes.mkString("|")})".r

  def matchType(c: blackbox.Context)(t: c.Type): String = {
    // Could be the weird type [X, Y] => (type expression), or it could be an actual tuple type.
    // `finalResultType` seems to help here.
    val args = t.finalResultType.typeArgs
    val typeParams = t.typeParams // This is nonempty only for the weird types mentioned above.

    t.typeSymbol.fullName match {
      case name if name matches "scala.Tuple[0-9]+" ⇒ s"(${args.map(matchType(c)).mkString(", ")})"
      case "scala.Function1" ⇒ s"${matchType(c)(args(0))} ..=>.. ${matchType(c)(args(1))}"
      case "scala.Option" ⇒ s"(1 + ${matchType(c)(args.head)})"
      case "scala.util.Either" ⇒ s"(${matchType(c)(args(0))} + ${matchType(c)(args(1))})"
      case "scala.Any" ⇒ "_"
      case "scala.Nothing" ⇒ "0"
      case "scala.Unit" ⇒ "1"
      case basicRegex(name) ⇒ s"<basic>$name"
      case _ if args.isEmpty && t.baseClasses.map(_.fullName) == Seq("scala.Any") ⇒ s"<tparam>$t"
      case _ if args.isEmpty ⇒ s"<base classes: ${t.baseClasses.map(_.fullName).mkString(", ")}>$t"
      case _ ⇒ s"<constructor>$t"
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

  def ofType[T]: T = macro ofTypeImpl[T]

  def inhabit[T]: T = macro inhabitImpl[T]

  // TODO: can we replace this with blackbox? Probably, as long as `def f3[X, Y]: X ⇒ Y ⇒ X = ofType` does not work with whitebox anyway.
  def ofTypeImpl[T: c.WeakTypeTag](c: whitebox.Context): c.Expr[T] = {
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

  def inhabitImpl[T](c: whitebox.Context): c.Tree = {
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
