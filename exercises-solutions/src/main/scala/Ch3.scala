package swscala


object Ch3Ex1 {

  /**
    Problem 1

    id(id)
    def id[T]: (T => T) = t => t
    can be rewritten as
    id[T](t: T): T = t
    pass in argument id[U]: U => U
    id[T](id[U]: U => U): T
    substitute T with (U => U)
    id[U => U](id[U]: U => U): U => U
    combine id(id) to idid
    idid[U] = U => U
    val idid[A] = id[A => A](id[A])
    idid does the same as what id[A] does

    id(const)
    def const[C, X]: (C ⇒ X ⇒ C) = c ⇒ x ⇒ c
    id[T](const[C, X]: C => X => C): T
    substitute T with C => X => C
    id[C => X => C](const[C, X]: C => X => C): C => X => C

    id(id)(id)
    substitute id(id) with idid
    idid(id)
    Using same logic to get from id(id) to idid,
    ididid[V] = V => V

    id((id(id))) works, using simliar logic as above
    */
  def id[T]: (T => T) = t => t

  /**
    Problem 2

    const(const)
    def const[C, X]: C ⇒ X ⇒ C = c ⇒ x ⇒ c
    can be rewritten as
    def const[C, X](c: C): X => C = x => c
    const[C, X](const[D, Y]: (D => Y => D)): X => C
    substitute D => Y => D into C
    const[D => Y => D, X](const[D, Y]): X => D => Y => D
    const(const) returns a function that takes in an argument of type X and returns const[D, Y]
    */
  def const[C, X]: C ⇒ X ⇒ C = c ⇒ x ⇒ c

  /**
    Problem 3

    twice(twice(twice))) returns a function take takes in an argument x and apply f 16 times
  
    twice[T](f: T => T): T => T
    twice[T]: (T => T) => T => T
    twice[T](twice[U]: (U => U) => U => U): T => T
    Substitute U => U for T
    twice[U => U](twice[U]): (U => U) => U => U
    twicetwice = twice(twice)
    twicetwice[U]: (U => U) => U => U
    Similarly we can derive
    twicetwicetwice[U]: (U => U) => U => U
    each wrap of twice basically squares the previous - not intuitive?
    */
  def twice[T](f: T => T): T => T = x => f(f(x))

  /**
    Problem 4

    thrice3 = thrice(thrice(thrice)) takes as argument a function f, and returns
    a function that applies f 3 ^ (3 ^ 3) times to an input argument (^ indicates exponent)
    */
  def thrice[T](f: T => T): T => T = x => f(f(f(x)))

  // Problem 5
  def ence[T](f: T => T, n: Int): T => T = {
    if (n == 0) x => x
    else if (n == 1) f
    else x => ence(f, n - 1)(f(x))
  }

  // Problem 6
  def swapFunc[A, B, C](f: (A, B) => C): (B, A) => C = (b: B, a: A) => f(a, b)

  /**
    Problem 7
    Infer types
    def r[...]:... = p ⇒ q ⇒ p(t ⇒ t(q))
    let q: A, t: A => B, p: ((A => B) => B) => C
    def r[...]:... = (p: ((A => B) => B) => C) => (q: A) => (p(t => t(q)): C)
    r[A, B, C]: (((A => B) => B) => C) => A => C
    */

  /**
    Problem 8
    Show not well typed
    def r[...]:... = p ⇒ p(q ⇒ q(p))
    let p: A and q: A => B
    def r[...]:... = p: A ⇒ p(q:(A => B) ⇒ q(p: A): B)
    then p: ((A => B) => B) => C
    but p: A, which cannot equal the above
    Cannot infer types for this function
    */

  /**
    Problem 9
    Infer types
    def s[...]:... = f ⇒ g ⇒ g(x ⇒ x(f(g)))
    let g: A => B, f: (A => B) => C, x: C => D
    def s[...]:... = (f: (A => B) => C) ⇒ (g: A => B) ⇒ g((x: C => D) ⇒ x(f(g):C):D)
    A = (C => D) => D
    def s[...]:... = (f: (((C => D) => D) => B) => C) ⇒ (g: ((C => D) => D) => B) ⇒ g((x: C => D) ⇒ x(f(g):C):D)
    s[B, C, D]: ((((C => D) => D) => B) => C) => (((C => D) => D) => B) => B
    */

  /**
    Problem 10
    Show not well typed
    def s[...]:... = f ⇒ g ⇒ g(x ⇒ f(g(x)))
    let x: A, g: A => B, f: B => C
    def s[...]:... = (f: B => C) ⇒ (g: A => B) ⇒ g(x: A ⇒ f(g(x:A):B):C)
    g: A => C contradicts with A => B, therefore not well typed
    */
}

object Ch3Ex2 {

  // Problem 1
  sealed trait CellState {
    def isBomb: Boolean = this match {
      case BombCell() => true
      case _ => false
    }
  }

  final case class ClosedCell() extends CellState
  final case class BombCell() extends CellState
  final case class OpenCell(neighborBombs: Int) extends CellState

  // Problem 2
  def numCellsShowingZeroNeighborBombs(cells: Seq[Seq[CellState]]): Int = {
    cells.flatten.count {
      _ match {
        case OpenCell(0) => true
        case _ => false
      }
    }
  }

  // Problem 3
  sealed trait RootOfLinear
  final case class NoRoot() extends RootOfLinear
  final case class OneRoot(x: Double) extends RootOfLinear
  final case class AllXRoots() extends RootOfLinear
  def solve1(a: Double, b: Double): RootOfLinear = (a, b) match {
    case (0, 0) => AllXRoots()
    case (0, _) => NoRoot()
    case (a, b) => OneRoot(-b / a)
  }

  // Problem 4
  def solve1(pairs: Seq[(Double, Double)]): Seq[Double] = {
    pairs.map{ case (a, b) => solve1(a, b) }.flatMap {
      _ match {
        case OneRoot(x) => Some(x)
        case _ => None
      }
    }
  }

  // Problem 5
  def f1[A, B](ab: Option[(A, B)]): (Option[A], Option[B]) =
    ab.map{case (a, b) => (Some(a), Some(b))}.getOrElse((None, None))

  def f2[A, B](aXorB: Either[A,B]): (Option[A], Option[B]) = aXorB match {
    case Left(a) => (Some(a), None)
    case Right(b) => (None, Some(b))
  }

  def f3[A,B,C](abc: Either[A, Either[B,C]]): Either[Either[A,B], C] = abc match {
    case Left(a) => Left(Left(a))
    case Right(Left(b)) => Left(Right(b))
    case Right(Right(c)) => Right(c)
  }
}

object Ch3Ex3 {

  // Problem 1
  sealed trait MyTU[T, U]
  final case class EmptyValuesTU[T, U]() extends MyTU[T, U]
  final case class TAndU[T, U](t: T, u: U) extends MyTU[T, U]
  final case class IntAndT[T, U](i: Int, t: T) extends MyTU[T, U]
  final case class StringAndU[T, U](s: String, u: U) extends MyTU[T, U]

  // Problem 2
  // Show that A ⇒ (B + C) != (A ⇒ B) + (A ⇒ C) in logic
  // forward (=>)
  // def p2Forward[A, B, C](fOfA: A => Either[B, C]): Either[A => B, A => C] = {
  //   // Need to produce either A => B or A => C and must now decide which one!
  //   // Suppose we decide to produce Left(A => B)
  //   Left((a: A) => fOfA(a))
  //   // However, this doesn't work since fOfA(a) returns Either[B, C], not B
  // }
  // We can't decide whether to return Left[A => B] or Right[A => C] without first
  // evaluating fOfA(a)

  // backward (<=) works
  // def p2Backward[A, B, C](abOrAc: Either[A => B, A => C]): A => Either[B, C] = abOrAc match {
  //   case Left(l: (A => B)) => { a => Left(l(a))  }
  //   case Right(r: (A => C)) => { a => Right(r(a))  }
  // }

  // Problem 3
  // (A * Int) + ((A * Char) + (A * Float))
  // = (A * Int) + (A * Char) + (A * Float)
  // = A * (Int + Char + Float)
  type P3T1[A] = Either[(A, Int), Either[(A, Char), (A, Float)]]
  type P3T2[A] = (A, IntOrCharOrFloat)

  sealed trait IntOrCharOrFloat
  final case class ItsInt(i: Int) extends IntOrCharOrFloat
  final case class ItsChar(c: Char) extends IntOrCharOrFloat
  final case class ItsFloat(f: Float) extends IntOrCharOrFloat

  def p3Forward[A]: P3T1[A] => P3T2[A] = {
    case Left((a, i)) => (a, ItsInt(i))
    case Right(aCharFloat) => aCharFloat match {
      case Left((a, c)) => (a, ItsChar(c))
      case Right((a, f)) => (a, ItsFloat(f))
    }
  }

  def p3Backward[A]: P3T2[A] => P3T1[A] = {
    case (a, ItsInt(i)) => Left((a, i))
    case (a, ItsChar(c)) => Right(Left((a, c)))
    case (a, ItsFloat(f)) => Right(Right((a, f)))
  }

  // Problem 4
  sealed trait OptEither[A, B]
  final case class OptNone[A, B]() extends OptEither[A, B]
  final case class OptLeft[A, B](a: A) extends OptEither[A, B]
  final case class OptRight[A, B](b: B) extends OptEither[A, B]

  def map[A, B, T](oe: OptEither[A, B])(f: B => T): OptEither[A, T] = oe match {
    case OptRight(b) => OptRight(f(b))
    case OptLeft(a) => OptLeft(a)
    case OptNone() => OptNone()
  }

  def flatMap[A, B, T](oe: OptEither[A, B])(f: B => OptEither[A, T]): OptEither[A, T] = oe match {
    case OptRight(b) => f(b)
    case OptLeft(a) => OptLeft(a)
    case OptNone() => OptNone()
  }

  // Problem 5
  type MyT[T] = Boolean ⇒ MyTVals[T]
  sealed trait MyTVals[T]
  final case class EmptyValueT[T]() extends MyTVals[T]
  final case class SingleValueT[T](t: T) extends MyTVals[T]
  final case class IntWithT[T](i: Int, t: T) extends MyTVals[T]
  final case class StringToT[T](st: String ⇒ T) extends MyTVals[T]

  def mapMyT[T, U](myT: MyT[T])(f: T => U): MyT[U] = b => {
    myT(b) match {
      case EmptyValueT() => EmptyValueT()
      case SingleValueT(t) => SingleValueT(f(t))
      case IntWithT(i, t) => IntWithT(i, f(t))
      case StringToT(sToT) => StringToT(s => f(sToT(s)))
    }
  }

  // Map for MyTU preferring T over U
  def mapMyTU[T, U, V](myTU: MyTU[T, U])(f: T => V): MyTU[V, U] = myTU match {
    case EmptyValuesTU() => EmptyValuesTU()
    case TAndU(t, u) => TAndU(f(t), u)
    case IntAndT(i, t) => IntAndT(i, f(t))
    case StringAndU(s, u) => StringAndU(s, u)
  }

  // Problem 6.1
  type P6State[S, A] = S ⇒ (A, S)
  def p6P1Map[S, A, B](sa: P6State[S, A])(f: ((S, A)) ⇒ (S, B)): P6State[S, B] = { s =>
    val (a, s2) = sa(s)
    val (s3, b) = f((s2, a))
    (b, s3)
  }

  // Problem 6.2
  // A + Z ⇒  (A ⇒  B) ⇒  B + Z
  def p6P2Map[A, B, Z](az: Either[A, Z])(aToB: A => B): Either[B, Z] = az match {
    case Left(a) => Left(aToB(a))
    case Right(z) => Right(z)
  }
  // A + Z ⇒  B + Z ⇒  (A ⇒  B ⇒  C) ⇒  C + Z
  def p6P2Map2[A, B, C, Z](az: Either[A, Z])(bz: Either[B, Z])(aToBToC: A => B => C): Either[C, Z] = az match {
    case Left(a) => bz match {
      case Left(b) => Left(aToBToC(a)(b))
      case Right(z) => Right(z)
    }
    case Right(z) => Right(z)
  }

  // Problem 6.3
  // flatMap[E, A, B]: Reader[E, A] ⇒  (A ⇒ Reader[E, B]) ⇒  Reader[E, B]
  type Reader[E, T] = E ⇒  T
  def p6P3FlatMap[E, A, B](r: Reader[E, A])(fOfA: (A ⇒ Reader[E, B])): Reader[E, B] = { e =>
    val a: A = r(e)
    val rEB: Reader[E, B] = fOfA(a)
    rEB(e)
  }

  // Problem 7.1
  type Density[Z, T] = (T => Z) ⇒  T
  def p7Map[Z, A, B](dza: Density[Z, A])(fAToB: (A => B)): Density[Z, B] = { fBToZ: (B => Z) =>
    // Simplified version:
    // fAToB(dza(a => fBToZ(fAToB(a))))
    // Long version
    val fAToZ: (A => Z) = (a : A) => fBToZ(fAToB(a))
    val a = dza(fAToZ)
    val b = fAToB(a)
    b // need to return type B
  }

  // Problem 7.2
  def p7FlatMap[Z, A, B](dza: Density[Z, A])(aToDzb: (A => Density[Z, B])): Density[Z, B] = { (bToZ: B => Z) =>
    // dza: (A => Z) => A
    // aToDzb: (A => ((B => Z) => B))
    // bToZ: B => Z
    val aToZ: A => Z = (a: A) => bToZ(aToDzb(a)(bToZ))
    val a: A = dza(aToZ)
    val dzb: Density[Z, B] = aToDzb(a)
    val b: B = dzb(bToZ)
    // must return type B
    b
  }

  // Problem 8.1
  type Cont[R, T] = (T => R) => R
  def p8Map[R, T, U]: Cont[R, T] ⇒ (T => U) => Cont[R, U] = { contRT => tToU => uToR =>
    // contRT: (T => R) => R
    // tToU: T => U
    // uToR: U => R
    val tToR: T => R = (t: T) => uToR(tToU(t))
    val r: R = contRT(tToR)
    r // need to return type R
  }

  def p8FlatMap[R, T, U]: Cont[R, T] ⇒ (T ⇒ Cont[R, U]) ⇒ Cont[R, U] = { contRT => f => uToR =>
    // contRT: (T => R) => R
    // f: T => (U => R) => R
    // uToR: U => R
    val tToR: T => R = (t: T) => f(t)(uToR)
    val r: R = contRT(tToR)
    r // need to return type R
  }

  // Problem 9
  // Tr3[A] ≡ 1 + A × A × A × Tr3[A]
  sealed trait Tr3[A]
  final case class Tr3Nil[A]() extends Tr3[A]
  final case class Tr3Vals[A](a1: A, a2: A, a3: A, rest: Tr3[A]) extends Tr3[A]

  def p9Map[A, B](tr3: Tr3[A])(f: A => B): Tr3[B] = tr3 match {
    case Tr3Nil() => Tr3Nil()
    case Tr3Vals(a1, a2, a3, rest) => Tr3Vals(f(a1), f(a2), f(a3), p9Map(rest)(f))
  }
}
