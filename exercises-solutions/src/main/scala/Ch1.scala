package swscala


object Ch1 {

  // Problem 1
  def normalize(s: Seq[Double]): Seq[Double] = {
    val maxVal = s.map(Math.abs).max
    if (maxVal != 0) s.map(_ / maxVal) else s
  }

  // Problem 2
  def add202DSeq(seq2D: Seq[Seq[Int]]): Seq[Seq[Int]] =
    seq2D.map(_.map(_ + 20))

  // Problem 3
  def isThreeFactor(n: Int): Boolean = (2 until n).filter(j => n % j == 0).size == 3
  lazy val threeFactor: Seq[Int] = (1 to 1000).filter(isThreeFactor)

  // Problem 4
  def threeFactorGeneral(f: Int => Boolean): Seq[Int] = (1 to 1000).filter(f)
  lazy val threeFactorAlt: Seq[Int] = threeFactorGeneral(isThreeFactor)

  // Problem 5
  def compose[A, B, C](f: A => B, g: B => C): A => C = (a: A) => g(f(a))
}
