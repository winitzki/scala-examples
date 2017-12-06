package example

import scala.language.experimental.macros
import scala.reflect.macros.blackbox
import scala.reflect.macros.whitebox

object CHTypes {

  sealed trait TypeExpr {
    override def toString: String = this match {
      case BasicT(name) ⇒ s"<basic>$name"
      case ConstructorT(fullExpr) ⇒ s"<constructor>$fullExpr"
      case AnyT ⇒ "_"
      case NothingT ⇒ "0"
      case UnitT ⇒ "1"
      case DisjunctT(terms) ⇒ "(" + terms.map(_.toString).mkString(" + ") + ")"
      case ConjunctT(terms) ⇒ "(" + terms.map(_.toString).mkString(", ") + ")"
      case ImplicT(head, body) ⇒ head.toString + " ..=>.. " + body.toString
      case ParamT(name) ⇒ s"<tparam>$name"
      case OtherT(name) ⇒ s"<other>$name"
    }
  }

  final case class BasicT(name: String) extends TypeExpr

  final case class ConstructorT(fullExpr: String) extends TypeExpr

  final case class DisjunctT(terms: Seq[TypeExpr]) extends TypeExpr

  final case class ConjunctT(terms: Seq[TypeExpr]) extends TypeExpr

  final case class ImplicT(head: TypeExpr, body: TypeExpr) extends TypeExpr

  case object AnyT extends TypeExpr

  case object NothingT extends TypeExpr

  case object UnitT extends TypeExpr

  case class ParamT(name: String) extends TypeExpr

  case class OtherT(name: String) extends TypeExpr

}

object CurryHoward {

  import CHTypes._

  private[example] def testType[T]: (String, String) = macro testTypeImpl[T]

  val basicTypes = List("Int", "String", "Boolean", "Float", "Double", "Long", "Symbol", "Char")
  val basicRegex = s"(?:scala.|java.lang.)*(${basicTypes.mkString("|")})".r

  def matchType(c: blackbox.Context)(t: c.Type): TypeExpr = {
    // Could be the weird type [X, Y] => (type expression), or it could be an actual tuple type.
    // `finalResultType` seems to help here.
    val args = t.finalResultType.typeArgs
    val typeParams = t.typeParams // This is nonempty only for the weird types mentioned above.

    t.typeSymbol.fullName match {
      case name if name matches "scala.Tuple[0-9]+" ⇒ ConjunctT(args.map(matchType(c))) //s"(${args.map(matchType(c)).mkString(", ")})"
      case "scala.Function1" ⇒ ImplicT(matchType(c)(args(0)), matchType(c)(args(1))) // s"${matchType(c)(args(0))} ..=>.. ${matchType(c)(args(1))}"
      case "scala.Option" ⇒ DisjunctT(Seq(UnitT, matchType(c)(args(0)))) //s"(1 + ${matchType(c)(args.head)})"
      case "scala.util.Either" ⇒ DisjunctT(Seq(matchType(c)(args(0)), matchType(c)(args(1)))) //s"(${matchType(c)(args(0))} + ${matchType(c)(args(1))})"
      case "scala.Any" ⇒ AnyT
      case "scala.Nothing" ⇒ NothingT
      case "scala.Unit" ⇒ UnitT
      case basicRegex(name) ⇒ BasicT(name)
      case _ if args.isEmpty && t.baseClasses.map(_.fullName) == Seq("scala.Any") ⇒ ParamT(t.toString)
      case _ if args.isEmpty ⇒ OtherT(t.toString)
      case _ ⇒ ConstructorT(t.toString)
    }
  }

  def testTypeImpl[T: c.WeakTypeTag](c: blackbox.Context): c.Expr[(String, String)] = {
    import c.universe._

    val typeT: c.Type = c.weakTypeOf[T]
    val enclosingType = c.internal.enclosingOwner.typeSignature

    val s1 = matchType(c)(typeT.resultType).toString
    val s2 = matchType(c)(enclosingType).toString

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
