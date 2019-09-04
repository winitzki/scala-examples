package example

import org.scalatest.FlatSpec

class CustomTypeclasses extends FlatSpec with CatsLawChecking {

  behavior of "test"

  it should "implement typeclass inheritance using implicit conversions" in {

    // Define some typeclasses using case class constructors.

    case class TC1[A]()
    object TC1 extends TCSub // Problem: this works, but TC1 must know about all its future intersections.

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
    //    case class TCC[A]() extends TC1[A] with TC2[A]

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

}
