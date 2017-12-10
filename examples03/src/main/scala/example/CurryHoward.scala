package example

import scala.language.experimental.macros
import scala.reflect.macros.blackbox
import scala.reflect.macros.whitebox

object CHTypes {

  final case class Sequent[T](premises: Set[T], goal: T)

  def proofs[T](sequent: Sequent[T]): Seq[TermExpr[T]] = {
    ???
  }

  sealed trait TypeExpr[+T] {
    override def toString: String = this match {
      case BasicT(name) ⇒ s"<basic>$name"
      case ConstructorT(fullExpr) ⇒ s"<constructor>$fullExpr"
      case AnyT ⇒ "_"
      case NothingT ⇒ "0"
      case UnitT ⇒ "1"
      case DisjunctT(terms) ⇒ terms.map(_.toString).mkString(" + ")
      case ConjunctT(terms) ⇒ "(" + terms.map(_.toString).mkString(", ") + ")"
      case ImplicT(head, body) ⇒ s"($head) ..=>.. $body"
      case ParamT(name) ⇒ s"<tparam>$name"
      case OtherT(name) ⇒ s"<other>$name"
    }
  }

  final case class BasicT[T](name: T) extends TypeExpr[T]

  final case class ConstructorT[T](fullExpr: String) extends TypeExpr[T]

  final case class DisjunctT[T](terms: Seq[TypeExpr[T]]) extends TypeExpr[T]

  final case class ConjunctT[T](terms: Seq[TypeExpr[T]]) extends TypeExpr[T]

  final case class ImplicT[T](head: TypeExpr[T], body: TypeExpr[T]) extends TypeExpr[T]

  case object AnyT extends TypeExpr[Nothing]

  case object NothingT extends TypeExpr[Nothing]

  case object UnitT extends TypeExpr[Nothing]

  case class ParamT[T](name: T) extends TypeExpr[T]

  case class OtherT[T](name: T) extends TypeExpr[T]

  object TermExpr {
    def propositions[T](termExpr: TermExpr[T]): Set[PropE[T]] = termExpr match {
      case p: PropE[T] ⇒ Set(p) // Need to specify type parameter in match... `case p@PropE(_)` does not work.
      case AppE(head, arg) ⇒ propositions(head) ++ propositions(arg)
      case l: LamE[T] ⇒ // Can't pattern-match directly for some reason! Some trouble with the type parameter T.
        Set(l.head) ++ propositions(l.body)
      case UnitE ⇒ Set()
      case AbsurdumE ⇒ Set()
      case ConjunctE(terms) ⇒ terms.flatMap(propositions).toSet
    }

  }

  sealed trait TermExpr[+T] {
    override def toString: String = this match {
      case PropE(name, typeName) => s"($name:$typeName)"
      case AppE(head, arg) => s"($head)($arg)"
      case LamE(head, body) => s"\\($head -> $body)"
      case UnitE => "()"
      case AbsurdumE => "Nothing"
      case ConjunctE(terms) => "(" + terms.map(_.toString).mkString(", ") + ")"
    }
  }

  final case class PropE[T](name: String, typeName: T) extends TermExpr[T]

  final case class AppE[T](head: TermExpr[T], arg: TermExpr[T]) extends TermExpr[T]

  final case class LamE[T](head: PropE[T], body: TermExpr[T]) extends TermExpr[T]

  case object UnitE extends TermExpr[Nothing]

  case object AbsurdumE extends TermExpr[Nothing]

  final case class ConjunctE[T](terms: Seq[TermExpr[T]]) extends TermExpr[T]

}

object CurryHoward {

  import CHTypes._

  private[example] def testType[T]: (String, String) = macro testTypeImpl[T]

  val basicTypes = List("Int", "String", "Boolean", "Float", "Double", "Long", "Symbol", "Char")
  val basicRegex = s"(?:scala.|java.lang.)*(${basicTypes.mkString("|")})".r

  def matchType(c: whitebox.Context)(t: c.Type): TypeExpr[String] = {
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

  def reifyParam(c: whitebox.Context)(term: PropE[String]): c.Tree = {
    import c.universe._
    term match {
      case PropE(name, typeName) ⇒
        val tpt = if (typeName.isEmpty) tq"" else {
          val tpn = TypeName(typeName)
          tq"$tpn"
        }
        val termName = TermName("t_" + name)
        val param = q"val $termName: $tpt"
        param
    }
  }

  def reifyTerms(c: whitebox.Context)(termExpr: TermExpr[String], paramTerms: Map[PropE[String], c.Tree]): c.Tree = {
    import c.universe._

    termExpr match {
      case p@PropE(name, typeName) =>
        val tn = TermName("t_" + name)
        q"$tn"
      case AppE(head, arg) => q"${reifyTerms(c)(head, paramTerms)}(${reifyTerms(c)(arg, paramTerms)})"
      case LamE(p@PropE(name, typeName), body) =>
        val param = paramTerms(p)
        q"($param ⇒ ${reifyTerms(c)(body, paramTerms)})"
      case UnitE => q"()"
      case AbsurdumE => q"???"
      case ConjunctE(terms) => q"(..${terms.map(t ⇒ reifyTerms(c)(t, paramTerms))})"
    }
  }

  def testTypeImpl[T: c.WeakTypeTag](c: whitebox.Context): c.Expr[(String, String)] = {
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
  def ofTypeImpl[T: c.WeakTypeTag](c: whitebox.Context): c.Tree = {
    val typeT: c.Type = c.weakTypeOf[T]
    inhabitInternal(c)(typeT)
  }

  def inhabitImpl[T](c: whitebox.Context): c.Tree = {
    val typeT = c.internal.enclosingOwner.typeSignature
    inhabitInternal(c)(typeT)
  }

  def inhabitInternal(c: whitebox.Context)(typeT: c.Type): c.Tree = {
    import c.universe._
    val typeStructure: TypeExpr[String] = matchType(c)(typeT)
    val termFound = ITP(typeStructure) match {
      case Nil ⇒
        c.error(c.enclosingPosition, s"type $typeStructure cannot be inhabited")
        UnitE
      case List(term) ⇒ term
      case list ⇒
        c.error(c.enclosingPosition, s"type $typeStructure can be inhabited in ${list.length} different ways")
        UnitE
    }

    println(s"DEBUG: Term found: $termFound, propositions: ${TermExpr.propositions(termFound)}")
    val paramTerms: Map[PropE[String], c.Tree] = TermExpr.propositions(termFound).toSeq.map(p ⇒ p → reifyParam(c)(p)).toMap
    val result = reifyTerms(c)(termFound, paramTerms)
    println(s"DEBUG: returning code: ${showCode(result)}")
    result
  }
}

sealed trait Term {
  def apply[T](x: T): Term = this // dummy implementation to simplify code
}

final case class \[T](v: Term ⇒ T) extends Term

final case class \:[T](v: Any ⇒ T) extends Term

object ITP {
  def apply(typeStructure: CHTypes.TypeExpr[String]): List[CHTypes.TermExpr[String]] = {
    import CHTypes._
    // TODO: implement intuitionistic theorem prover here

    // only accept types of the form a=>b=>c here
    val term = typeStructure match {
      case BasicT(name) => PropE(name, name)
      case ConstructorT(fullExpr) => PropE("_", fullExpr)
      case ConjunctT(terms) => ConjunctE(terms.map(t ⇒ ITP(t).head))
      case ImplicT(ParamT(name), body) => LamE(PropE(name, name), ITP(body).head)
      case ImplicT(BasicT(name), body) => LamE(PropE(name, name), ITP(body).head)
      case AnyT => PropE("_", "Any")
      case NothingT => AbsurdumE
      case UnitT => UnitE
      case ParamT(name) => PropE(name, name)
      case OtherT(name) => PropE("_", name)
      case _ => UnitE
    }
    List(term)
  }
}
