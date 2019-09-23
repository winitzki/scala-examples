package example

import org.scalatest.FlatSpec

class CustomTypeclasses extends FlatSpec with CatsLawChecking {

  behavior of "test"

  it should "implement typeclass inheritance using implicit conversions" in {

    // Define some typeclasses using case class constructors.

    case class TC1[A]()
    object TC1 extends TCSub // Problem: this works, but TC1 must know about all its future extensions.

    case class TC2[A]()
    object TC2 extends TCSub

    // This typeclass is an intersection of TC1 and TC2's type domains.
    case class TC[A]()(implicit val ti1: TC1[A], val ti2: TC2[A])

    // If we have TC1 and TC2, we should automatically get TC.

    object TC extends TCSub {
      implicit def toTC[A: TC1 : TC2]: TC[A] = TC()
    }

    // If we have TC, we should automatically get TC1 and TC2.
    trait TCSub {
      implicit def tc1[A: TC]: TC1[A] = implicitly[TC[A]].ti1

      implicit def tc2[A: TC]: TC2[A] = implicitly[TC[A]].ti2
    }

    // Type Int has TC1 and TC2.
    implicit val tc1Int: TC1[Int] = TC1()
    implicit val tc2Int: TC2[Int] = TC2()

    // Should be able to get TC automatically.
    implicitly[TC[Int]]


    // Type A has TC. It should automatically have TC1.
    def getTC1[A: TC]: TC1[A] = {
      implicitly[TC1[A]]
    }

    // Type A has TC. It should automatically have TC1 and TC2. Accessing TC should not be a problem after that.
    def getTC12[A: TC]: (TC1[A], TC2[A]) = {
      val ti1 = implicitly[TC1[A]]
      val ti2 = implicitly[TC2[A]]
      val ti = implicitly[TC[A]]
      (ti1, ti2)
    }

    getTC12[Int]
  }

  it should "implement typeclass inheritance using implicit conversions in one object" in {

    // Define some typeclasses using case class constructors.

    case class TC1[A]()
    case class TC2[A]()
    // This typeclass is an intersection of TC1 and TC2's type domains.
    case class TC[A]()(implicit val ti1: TC1[A], val ti2: TC2[A])

    // Can we inherit two case classes? No. Can only mix in a trait.
    //    case class TCC[A]() extends TC1[A] with TC2[A] // Does not compile.

    // If we have TC1 and TC2, we should automatically get TC.
    // If we have TC, we should automatically get TC1 and TC2.
    object TC {
      implicit def toTC[A: TC1 : TC2]: TC[A] = TC()

      implicit def tc1[A: TC]: TC1[A] = implicitly[TC[A]].ti1

      implicit def tc2[A: TC]: TC2[A] = implicitly[TC[A]].ti2
    }

    // Type Int has TC1 and TC2.
    implicit val tc1Int: TC1[Int] = TC1()
    implicit val tc2Int: TC2[Int] = TC2()

    // Should be able to get TC automatically.
    implicitly[TC[Int]]


    // Type A has TC. It should automatically have TC1.
    def getTC1[A: TC]: TC1[A] = {
      import TC.tc1 // Need this import for converting TC to TC1.
      implicitly[TC1[A]]
    }

    // Type A has TC. It should automatically have TC1 and TC2. Accessing TC should not be a problem after that.
    def getTC12[A: TC]: (TC1[A], TC2[A]) = {
      import TC.{tc1, tc2} // Need this import.
      val ti1 = implicitly[TC1[A]]
      val ti2 = implicitly[TC2[A]]
      val ti = implicitly[TC[A]]
      (ti1, ti2)
    }

    getTC12[Int]
  }

  it should "implement typeclass inheritance using OO trait inheritance" in {

    trait TC1[A]

    trait TC2[A]

    trait TC[A] extends TC1[A] with TC2[A]
    object TC {
      implicit def toTC[A: TC1 : TC2]: TC[A] = new TC[A] {} // Boilerplate code that overrides methods.
    }

    // If we have TC, we should automatically get TC1 and TC2 via OO subtyping.
    // Type Int has TC1 and TC2.
    implicit val tc1Int: TC1[Int] = new TC1[Int] {}
    implicit val tc2Int: TC2[Int] = new TC2[Int] {}


    // Should be able to get TC automatically.
    val p = implicitly[TC[Int]]
    implicit val p1 = p // This is OK because TC[Int] is a result of implicit conversion and so has lower priority.
    //    implicit val p2 = p // This would break compilation: ambiguous implicits.

    // Type A has TC. It should automatically have TC1.
    def getTC1[A: TC]: TC1[A] = {
      implicitly[TC1[A]]
    }

    // Type A has TC. It should automatically have TC1 and TC2. Accessing TC should not be a problem after that.
    def getTC12[A: TC]: (TC1[A], TC2[A]) = {
      val ti1 = implicitly[TC1[A]]
      implicit val x = ti1
      implicit val y = ti1
      val z = implicitly[TC1[A]]
      val ti2 = implicitly[TC2[A]]
      lazy val ti = getTC12[A] // Still compiles.
      (ti1, ti2)
    }

    getTC12[Int]
  }

  it should "inherit the same typeclass from two typeclasses using case classes" in {

    // TC1 and TC2 both inherit from TC.
    // We want automatic conversions from TC1 to TC and from TC2 to TC. 
    // Can we then have a type that has both TC1 and TC2 instances? Yes, but with some manual work.
    final case class TC[A]()
    final case class TC1[A]()(implicit val tc0: TC[A])
    object TC1 {
      implicit def toTC[A](implicit x: TC1[A]): TC[A] = x.tc0
    }
    final case class TC2[A]()(implicit val tc0: TC[A])
    object TC2 {
      implicit def toTC[A](implicit x: TC2[A]): TC[A] = x.tc0
    }
    
    // A function requires A to have both TC1 and TC2 instances and then wants to access TC instance.
    
    def f[A: TC1 : TC2]() = {
      import TC1._ // Manual work here: use this import but not the other one.
//      import TC2._ // Compilation fails when this is uncommented because two implicits of type TC[A] will be found!
      implicitly[TC[A]]
    }
    
    val tcInt: TC[Int] = TC() // Do not make this implicit.
    implicit val tc1Int: TC1[Int] = TC1()(tcInt)
    implicit val tc2Int: TC2[Int] = TC2()(tcInt)
    
    f[Int]()
  }

  it should "inherit the same typeclass from two typeclasses using OO inheritance" in {
    // TC1 and TC2 both inherit from TC.
    // We want automatic conversions from TC1 to TC and from TC2 to TC. 
    // Can we then have a type that has both TC1 and TC2 instances? No.
    trait TC[A]
    trait TC1[A] extends TC[A]
    trait TC2[A] extends TC[A]
    // A function requires A to have both TC1 and TC2 instances and then wants to access TC instance.

    def f[A: TC1 : TC2]() = {
      "implicitly[TC[A]]" shouldNot compile // Compilation fails when this is uncommented because two implicits of type TC[A] will be found!
    }

    implicit val tc1Int: TC1[Int] = new TC1[Int]{}
    implicit val tc2Int: TC2[Int] = new TC2[Int]{}
    
    f[Int]()
  }
  
  it should "define a closed type domain" in {
    final class HasBitsize[T] private (val size: Int)
    object HasBitsize {
      implicit val bitsizeShort = new HasBitsize[Short](16)
      implicit val bitsizeInt = new HasBitsize[Int](32)
      implicit val bitsizeLong = new HasBitsize[Long](64)
    }
    def bitsize[T: HasBitsize]: Int = implicitly[HasBitsize[T]].size
    
    bitsize[Int] shouldEqual 32
    
    "bitsize[String]" shouldNot compile
    
    "new HasBitsize[Boolean](1)" shouldNot compile
  }
}
