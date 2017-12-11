package example

import java.util.concurrent.atomic.AtomicInteger

import scala.language.experimental.macros
import scala.reflect.macros.blackbox
import scala.reflect.macros.whitebox

class FreshIdents(prefix: String) {
  private val identCount = new AtomicInteger(0)

  private def newIdentCount: Int = identCount.incrementAndGet()

  def apply(): String = prefix + newIdentCount.toString
}

object CHTypes {
  private val freshSubformulas = new FreshIdents(prefix = "f")

  def subformulas[T](typeStructure: TypeExpr[T]): Set[TypeExpr[T]] = Set(typeStructure) ++ (typeStructure match {
    case DisjunctT(terms) ⇒ terms.flatMap(subformulas)
    case ConjunctT(terms) ⇒ terms.flatMap(subformulas)
    case head :-> body ⇒ subformulas(head) ++ subformulas(body) ++ (head match {
      case DisjunctT(terms) ⇒ terms.flatMap(t ⇒ subformulas(:->(t, body)))
      case ConjunctT(terms) ⇒ subformulas(terms.foldRight(body) { case (t, prev) ⇒ t :-> prev })
      case _ :-> bd ⇒ subformulas(bd :-> body) // Special subformula case for implication of the form (hd ⇒ bd) ⇒ body
      case _ ⇒ Seq() // `head` is an atomic type
    })
    case _ ⇒ Seq() // `typeStructure` is an atomic type
  }).toSet

  def explode[T](src: Seq[Seq[T]]): Seq[Seq[T]] = {
    src.foldLeft[Seq[Seq[T]]](Seq(Seq())) { case (prevSeqSeq, newSeq) ⇒
      for {
        prev ← prevSeqSeq
        next ← newSeq
      } yield prev :+ next
    }
  }

  // Subformula index.
  type SFIndex = Int

  final case class Sequent[T](premises: List[SFIndex], goal: SFIndex, sfIndexOfTExpr: Map[TypeExpr[T], SFIndex]) {

    val tExprAtSFIndex: Map[SFIndex, TypeExpr[T]] = sfIndexOfTExpr.map { case (k, v) ⇒ v → k }

    val premiseVars: Seq[PropE[T]] = premises.map { premiseSFIndex ⇒ PropE(freshVar(), tExprAtSFIndex(premiseSFIndex)) }

    def constructResultTerm(result: TermExpr[T]): TermExpr[T] = {
      premises.zip(premiseVars).foldLeft(result) { case (prevTerm, (sfIndex, premiseVar)) ⇒
        LamE(premiseVar, prevTerm, premiseVar.tExpr :-> prevTerm.tExpr)
      }
    }

    // Convenience method.
    def goalExpr: TypeExpr[T] = tExprAtSFIndex(goal)
  }

  type ProofTerm[T] = TermExpr[T]

  type BackTransform[T] = Seq[ProofTerm[T]] ⇒ ProofTerm[T]

  final case class ForwardTransform[T](name: String, applyTo: Sequent[T] ⇒ Option[(Seq[Sequent[T]], BackTransform[T])])

  private val freshVar = new FreshIdents(prefix = "x")

  def followsFromAxioms[T](sequent: Sequent[T]): Seq[ProofTerm[T]] = {
    // The LJT calculus has three axioms. We use the Id axiom and the T axiom because F axiom is not useful for code generation.

    val fromIdAxiom: Seq[TermExpr[T]] = sequent.premiseVars
      .zip(sequent.premises)
      .filter(_._2 == sequent.goal)
      .map { case (premiseVar, _) ⇒
        // Generate a new term x1 ⇒ x2 ⇒ ... ⇒ xN ⇒ xK with fresh names. Here `xK` is one of the variables, selecting the premise that is equal to the goal.
        // At this iteration, we already selected the premise that is equal to the goal.
        sequent.constructResultTerm(premiseVar)
      }
    val fromTAxiom: Seq[TermExpr[T]] = sequent.goalExpr match {
      case unitT: UnitT[T] ⇒ Seq(sequent.constructResultTerm(UnitE(unitT)))
      case _ ⇒ Seq()
    }
    fromIdAxiom ++ fromTAxiom
  }

  def invertibleRules[T]: Seq[ForwardTransform[T]] = Seq(
    ForwardTransform[T]("->R", sequent ⇒ sequent.goalExpr match {
      case a :-> b ⇒
        val aIndex = sequent.sfIndexOfTExpr(a)
        val bIndex = sequent.sfIndexOfTExpr(b)
        val newSequent = sequent.copy(premises = aIndex :: sequent.premises, goal = bIndex)
        Some(Seq(newSequent), { proofTerms ⇒
          // This rule expects only one sub-proof term.
          val subProof = proofTerms.head
          // `subProof` is the proof of (G, A) |- B, and we need a proof of G |- A ⇒ B.
          // `subProof` is x ⇒ y ⇒ ... ⇒ z ⇒ a ⇒ <some term depending on (a, x, y, z)>
          // This proof will be exactly what we need if we reverse the order of curried arguments w.r.t. the list order of premises.
          subProof
        })
      case _ ⇒ None
    })
  )

  def nonInvertibleRules[T]: Seq[ForwardTransform[T]] = Seq(

  )

  // Main recursive function that computes the list of available proofs for a sequent.
  // The main assumption is that the depth-first proof search terminates.
  // No loop checking is performed on sequents.
  def findProofTerms[T](sequent: Sequent[T]): Seq[ProofTerm[T]] = {
    // Check whether the sequent follows directly from an axiom.
    val fromAxioms: Seq[ProofTerm[T]] = followsFromAxioms(sequent) // This could be empty or non-empty.
    // Even if the sequent follows from axioms, we should try applying rules in hopes of getting more proofs.

    // Try each rule on sequent. If rule applies, obtain the next sequent.
    // If all rules were invertible, we would return `fromAxioms ++ fromInvertibleRules`.

    // We try applying just one invertible rule and proceed from there.
    val fromRules: Seq[ProofTerm[T]] = invertibleRules[T].view.flatMap(_.applyTo(sequent)).headOption match {
      case Some((newSequents, backTransform)) ⇒
        // All the new sequents need to be proved before we can continue. They may have several proofs each.
        val newProofs: Seq[Seq[ProofTerm[T]]] = newSequents.map(findProofTerms)
        val explodedNewProofs: Seq[Seq[ProofTerm[T]]] = explode(newProofs)
        explodedNewProofs.map(backTransform) ++ fromAxioms

      case None ⇒
        // No invertible rules apply, so we need to try all non-invertible (i.e. not guaranteed to work) rules.
        // Each non-invertible rule will generate some proofs or none.
        // If a rule generates no proofs, another rule should be used.
        // If a rule generates some proofs, we append them to `fromAxioms` and keep trying another rule.
        // If no more rules apply here, we return `fromAxioms`.
        // Use flatMap to concatenate all results from all applicable non-invertible rules.
        val fromNoninvertibleRules: Seq[ProofTerm[T]] = nonInvertibleRules[T].flatMap(_.applyTo(sequent)).flatMap { case ((newSequents, backTransform)) ⇒
          val newProofs: Seq[Seq[ProofTerm[T]]] = newSequents.map(findProofTerms)
          val explodedNewProofs: Seq[Seq[ProofTerm[T]]] = explode(newProofs)
          val finalNewProofs: Seq[ProofTerm[T]] = explodedNewProofs.map(backTransform)
          finalNewProofs
        }
        fromNoninvertibleRules ++ fromAxioms
    }
    fromRules
  }

  sealed trait TypeExpr[+T] {
    override def toString: String = this match {
      case DisjunctT(terms) ⇒ terms.map(_.toString).mkString(" + ")
      case ConjunctT(terms) ⇒ "(" + terms.map(_.toString).mkString(", ") + ")"
      case head :-> body ⇒ s"($head) ..=>.. $body"
      case BasicT(name) ⇒ s"<basic>$name"
      case ConstructorT(fullExpr) ⇒ s"<constructor>$fullExpr"
      case TP(name) ⇒ s"<tparam>$name"
      case OtherT(name) ⇒ s"<other>$name"
      case NothingT(_) ⇒ "0"
      case UnitT(_) ⇒ "1"
    }

    def isAtomic: Boolean

    def map[U](f: T ⇒ U): TypeExpr[U]
  }

  sealed trait NonAtomicTypeExpr {
    def isAtomic: Boolean = false
  }

  sealed trait AtomicTypeExpr[T] {
    def isAtomic: Boolean = true

    def name: T
  }

  object TypeExpr {

    implicit class WithImplication[T](tpe1: TypeExpr[T]) {
      def :->(tpe2: TypeExpr[T]): TypeExpr[T] = CHTypes.:->(tpe1, tpe2)
    }

  }

  final case class DisjunctT[T](terms: Seq[TypeExpr[T]]) extends TypeExpr[T] with NonAtomicTypeExpr {
    override def map[U](f: T ⇒ U): TypeExpr[U] = DisjunctT(terms.map(_.map(f)))
  }

  final case class ConjunctT[T](terms: Seq[TypeExpr[T]]) extends TypeExpr[T] with NonAtomicTypeExpr {
    override def map[U](f: T ⇒ U): TypeExpr[U] = ConjunctT(terms.map(_.map(f)))
  }

  final case class :->[T](head: TypeExpr[T], body: TypeExpr[T]) extends TypeExpr[T] with NonAtomicTypeExpr {
    override def map[U](f: T ⇒ U): TypeExpr[U] = :->(head.map(f), body.map(f))
  }

  final case class NothingT[T](name: T) extends TypeExpr[T] with AtomicTypeExpr[T] {
    override def map[U](f: T ⇒ U): TypeExpr[U] = NothingT(f(name))
  }

  final case class UnitT[T](name: T) extends TypeExpr[T] with AtomicTypeExpr[T] {
    override def map[U](f: T ⇒ U): TypeExpr[U] = UnitT(f(name))
  }

  // Type parameter. Use a short name for convenience.
  case class TP[T](name: T) extends TypeExpr[T] with AtomicTypeExpr[T] {
    override def map[U](f: T ⇒ U): TypeExpr[U] = TP(f(name))
  }

  case class OtherT[T](name: T) extends TypeExpr[T] with AtomicTypeExpr[T] {
    override def map[U](f: T ⇒ U): TypeExpr[U] = OtherT(f(name))
  }

  final case class BasicT[T](name: T) extends TypeExpr[T] with AtomicTypeExpr[T] {
    override def map[U](f: T ⇒ U): TypeExpr[U] = BasicT(f(name))
  }

  final case class ConstructorT[T](name: T) extends TypeExpr[T] with AtomicTypeExpr[T] {
    override def map[U](f: T ⇒ U): TypeExpr[U] = ConstructorT(f(name))
  }

  object TermExpr {
    def propositions[T](termExpr: TermExpr[T]): Set[PropE[T]] = termExpr match {
      case p: PropE[T] ⇒ Set(p) // Need to specify type parameter in match... `case p@PropE(_)` does not work.
      case AppE(head, arg, _) ⇒ propositions(head) ++ propositions(arg)
      case l: LamE[T] ⇒ // Can't pattern-match directly for some reason! Some trouble with the type parameter T.
        Set(l.head) ++ propositions(l.body)
      case UnitE(tExpr) ⇒ Set()
      case ConjunctE(terms, _) ⇒ terms.flatMap(propositions).toSet
    }

  }

  sealed trait TermExpr[+T] {
    def tExpr: TypeExpr[T]

    override def toString: String = this match {
      case PropE(name, tExpr) => s"($name:$tExpr)"
      case AppE(head, arg, _) => s"($head)($arg)"
      case LamE(head, body, _) => s"\\($head -> $body)"
      case UnitE(tExpr) => "()"
      case ConjunctE(terms, _) => "(" + terms.map(_.toString).mkString(", ") + ")"
    }

    def map[U](f: T ⇒ U): TermExpr[U]
  }

  final case class PropE[T](name: String, tExpr: TypeExpr[T]) extends TermExpr[T] {
    override def map[U](f: T ⇒ U): PropE[U] = PropE(name, tExpr.map(f))
  }

  final case class AppE[T](head: TermExpr[T], arg: TermExpr[T], tExpr: TypeExpr[T]) extends TermExpr[T] {
    override def map[U](f: T ⇒ U): TermExpr[U] = AppE(head map f, arg map f, tExpr map f)
  }

  final case class LamE[T](head: PropE[T], body: TermExpr[T], tExpr: TypeExpr[T]) extends TermExpr[T] {
    override def map[U](f: T ⇒ U): TermExpr[U] = LamE(head map f, body map f, tExpr map f)
  }

  final case class UnitE[T](tExpr: TypeExpr[T]) extends TermExpr[T] {
    override def map[U](f: T ⇒ U): TermExpr[U] = UnitE(tExpr map f)
  }

  final case class ConjunctE[T](terms: Seq[TermExpr[T]], tExpr: TypeExpr[T]) extends TermExpr[T] {
    override def map[U](f: T ⇒ U): TermExpr[U] = ConjunctE(terms.map(_.map(f)), tExpr map f)
  }

}

object CurryHoward {

  import CHTypes._

  private[example] def testType[T]: (String, String) = macro testTypeImpl[T]

  private[example] val basicTypes = List("Int", "String", "Boolean", "Float", "Double", "Long", "Symbol", "Char")
  private val basicRegex = s"(?:scala.|java.lang.)*(${basicTypes.mkString("|")})".r

  // TODO: use c.Type instead of String
  def matchType(c: whitebox.Context)(t: c.Type): TypeExpr[String] = {
    // Could be the weird type [X, Y] => (type expression), or it could be an actual tuple type.
    // `finalResultType` seems to help here.
    val args = t.finalResultType.typeArgs
    val typeParams = t.typeParams // This is nonempty only for the weird types mentioned above.

    t.typeSymbol.fullName match {
      case name if name matches "scala.Tuple[0-9]+" ⇒ ConjunctT(args.map(matchType(c))) //s"(${args.map(matchType(c)).mkString(", ")})"
      case "scala.Function1" ⇒ :->(matchType(c)(args.head), matchType(c)(args(1))) // s"${matchType(c)(args(0))} ..=>.. ${matchType(c)(args(1))}"
      case "scala.Option" ⇒ DisjunctT(Seq(UnitT("Unit"), matchType(c)(args.head))) //s"(1 + ${matchType(c)(args.head)})"
      case "scala.util.Either" ⇒ DisjunctT(Seq(matchType(c)(args.head), matchType(c)(args(1)))) //s"(${matchType(c)(args(0))} + ${matchType(c)(args(1))})"
      case "scala.Any" ⇒ OtherT("_")
      case "scala.Nothing" ⇒ NothingT("Nothing")
      case "scala.Unit" ⇒ UnitT("Unit")
      case basicRegex(name) ⇒ BasicT(name)
      case _ if args.isEmpty && t.baseClasses.map(_.fullName) == Seq("scala.Any") ⇒ TP(t.toString)
      case _ if args.isEmpty ⇒ OtherT(t.toString)
      case _ ⇒ ConstructorT(t.toString)
    }
  }

  def reifyParam(c: whitebox.Context)(term: PropE[String]): c.Tree = {
    import c.universe._
    term match {
      case PropE(name, tExpr) ⇒
        val tpt = tExpr match {
          case _: NothingT[String] ⇒ tq""
          // TODO: Stop using String as type parameter T, use c.Type instead
          case TP(name) ⇒
            val tpn = TypeName(name)
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
      case AppE(head, arg, _) => q"${reifyTerms(c)(head, paramTerms)}(${reifyTerms(c)(arg, paramTerms)})"
      case LamE(p, body, _) =>
        val param = paramTerms(p)
        q"($param ⇒ ${reifyTerms(c)(body, paramTerms)})"
      case UnitE(_) => q"()"
      case ConjunctE(terms, _) => q"(..${terms.map(t ⇒ reifyTerms(c)(t, paramTerms))})"
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
    val termFound: TermExpr[String] = ITP(typeStructure) match {
      case Nil ⇒
        c.error(c.enclosingPosition, s"type $typeStructure cannot be inhabited")
        null
      case List(term) ⇒ term
      case list ⇒
        c.error(c.enclosingPosition, s"type $typeStructure can be inhabited in ${list.length} different ways")
        null
    }

    println(s"DEBUG: Term found: $termFound, propositions: ${TermExpr.propositions(termFound)}")
    val paramTerms: Map[PropE[String], c.Tree] = TermExpr.propositions(termFound).toSeq.map(p ⇒ p → reifyParam(c)(p)).toMap
    val result = reifyTerms(c)(termFound, paramTerms)
    println(s"DEBUG: returning code: ${showCode(result)}")
    result
  }
}

object ITP {

  def apply(typeStructure: CHTypes.TypeExpr[String]): List[CHTypes.TermExpr[String]] = findProofs(typeStructure)

  def findProofs[T](typeStructure: CHTypes.TypeExpr[T]): List[CHTypes.TermExpr[T]] = {
    import CHTypes._
    val subformulaDictionary: Map[TypeExpr[T], SFIndex] = CHTypes.subformulas(typeStructure).zipWithIndex.toMap
    val mainSequent = Sequent[T](List(), subformulaDictionary(typeStructure), subformulaDictionary)
    val proofs: Seq[ProofTerm[T]] = CHTypes.findProofTerms(mainSequent)
    proofs.toList
  }
}
