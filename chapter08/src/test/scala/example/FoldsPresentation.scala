package example

import org.scalatest.{FlatSpec, Matchers}

class FoldsPresentation extends FlatSpec with Matchers {

  behavior of "folds"

  val data = (1 to 10).toList.map(_.toDouble)

  // Compute the standard deviation of data.
  {
    val n = data.length
    val average = data.sum / n
    val average_sq = data.map(x ⇒ x * x).sum / n
    val stdev = math.sqrt((n / (n - 1)) * (average_sq - average * average_sq))
  }

  val n = data.foldLeft(0)((b, x) ⇒ b + 1)
  val sum = data.foldLeft(0.0)((b, x) ⇒ b + x)
  val sum_sq = data.foldLeft(0.0)((b, x) ⇒ b + x * x)

  // How to combine "folds" ? Combine the *data* that `.foldLeft` takes.

  case class Fold0[Z, A](init: A, update: (A, Z) ⇒ A)

  // Syntax to perform foldLeft using this data:
  implicit class Fold0Syntax[A](s: List[A]) {
    def fold0[B](fold0: Fold0[A, B]): B = s.foldLeft(fold0.init)(fold0.update)
  }

  val length0: Fold0[Double, Double] = Fold0(0, (acc, _) => acc + 1)

  it should "apply fold0" in {
    data.fold0(length0) shouldEqual 10
  }

  // Combine: Fold0[Z, A], Fold0[Z, B] ⇒ Fold0[Z, (A, B)]
  def zip0[Z, A, B](f0: Fold0[Z, A], f1: Fold0[Z, B]): Fold0[Z, (A, B)] = {
    // We have: A, (A, Z) ⇒ A; B, (B, Z) ⇒ B
    // We need (A, B), ((A, B), Z) ⇒ (A, B)
    val newInit: (A, B) = (f0.init, f1.init)
    val newUpdate: ((A, B), Z) ⇒ (A, B) = {
      case ((a, b), z) ⇒
        (f0.update(a, z), f1.update(b, z))
    }
    Fold0(newInit, newUpdate)
  }

  val sum0: Fold0[Double, Double] = Fold0(0, (acc, x) => acc + x)

  val sumsq0: Fold0[Double, Double] = Fold0(0, (acc, x) => acc + x * x)
  
  val sumAndLength0 = zip0(sum0, length0)
  it should "compute average using zip" in {
    val (sum, length) = data.fold0(sumAndLength0)
    sum / length shouldEqual 5.5
  }
  
  // Inconvenient! We would like the code to be 
  // data.fold((sumsq / length - sum*sum) / length)
  // Need to combine the values out of tuples into a single, final value!
  
  // Put the final computation into Fold0. Get Fold1:
  case class Fold1[Z, A, R](init: A, update: (A, Z) ⇒ A, result: A ⇒ R)

  // Syntax to perform foldLeft using this data:
  implicit class Fold1Syntax[A](s: List[A]) {
    def fold1[B, R](fold1: Fold1[A, B, R]): R = fold1.result(s.foldLeft(fold1.init)(fold1.update))
  }

  val length1: Fold1[Double, Int, Int] = Fold1(0, (acc, _) => acc + 1, identity)

  it should "apply fold1" in {
    data.fold1(length1) shouldEqual 10
  }

  // Combine: Fold0[Z, A], Fold0[Z, B] ⇒ Fold0[Z, (A, B)]
  def zip1[Z, A1, A2, R1, R2](f0: Fold1[Z, A1, R1], f1: Fold1[Z, A2, R2]): Fold1[Z, (A1, A2), (R1, R2)] = {
    // We have: A, (A, Z) ⇒ A; B, (B, Z) ⇒ B
    // We need (A, B), ((A, B), Z) ⇒ (A, B)
    val newInit: (A1, A2) = (f0.init, f1.init)
    val newUpdate: ((A1, A2), Z) ⇒ (A1, A2) = {
      case ((a, b), z) ⇒
        (f0.update(a, z), f1.update(b, z))
    }
    val newResult: ((A1, A2)) ⇒(R1, R2) = {
      case (a0, a1) ⇒ (f0.result(a0), f1.result(a1))
    }
    Fold1(newInit, newUpdate, newResult)
  }

  val sum1: Fold1[Double, Double, Double] = Fold1(0, (acc, x) => acc + x, identity)

  val sumsq1: Fold1[Double, Double, Double] = Fold1(0, (acc, x) => acc + x * x, identity)

  val sumAndLength1: Fold1[Double, (Double, Int), (Double, Int)] = zip1(sum1, length1)
  
  it should "compute average using zip1" in {
    val (sum, length) = data.fold1(sumAndLength1)
    sum / length shouldEqual 5.5
  }
  
  // Syntax for adding a transformation.
  implicit class Fold1Syntax1[Z, A, R](fold1: Fold1[Z, A, R]) {
    def andThen[T](f: R ⇒ T): Fold1[Z, A, T] = {
      fold1.copy(result = fold1.result andThen f)
    }
  }
  
  // Define a "constant" fold.
  
  def pure1[Z, R](x: R): Fold1[Z, R, R] = Fold1(x, (a, z) => x, _ => x)
  
  val ave1 = zip1(sum1, length1) andThen { case (s, len) ⇒ s / len }
  
}
