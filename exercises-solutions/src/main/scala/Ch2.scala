package swscala


object Ch2Ex1 {

  val zeroToNine = (0 to 9)

  // Problem 1
  lazy val p1: Seq[(Int, Int)] = zeroToNine.flatMap(i => zeroToNine.map(j => (i, j)))
    .filter{case (i, j) => i + 4 * j  > i * j}

  // Problem 2
  def p2(ss: Seq[String], bs: Seq[Boolean]): Seq[String] =
    ss.zip(bs).filter(_._2).map(_._1)

  // Problem 3
  def p3(nums: Seq[Int]): Seq[(Int, Boolean)] =
    nums.zip(nums.zip(nums.tail).map{case (former: Int, latter: Int) => former < latter})

  // Problem 4
  def p4[S, I](a: Seq[S], b: Seq[I]): Map[I, S] = b.zip(a).toMap

  // Problem 5
  def p5[S](a: Seq[S], b: Seq[Int]) = a.zip(b).sortBy(_._2).map(_._1)
}

object Ch2Ex2 {

  // Problem 1
  def p1(purchasedItems: Seq[(String, Int)]): Map[String, Int] =
    purchasedItems.groupBy(_._1).map{case (k, v) => (k, v.map(_._2).sum)}

  // Problem 2
  def p2(numsLists: Seq[List[Int]]): Seq[List[Int]] =
    numsLists.map(_.sortBy(-_).take(3))

  // Problem 3
  def p3[I, J](a: Set[I], b: Set[J]): Set[(I, J)] =
    a.flatMap(x => b.map(y => (x, y)))

  // Problem 4
  def p4[Person, Amount](daily: Seq[Map[Person, Amount]]): Map[Person, Seq[Amount]] =
    daily.flatMap(_.toSeq).groupBy(_._1).mapValues(_.map(_._2))
}

object Ch2Ex3 {


  // Problem 1
  def sumDigitsIterator(f: Int => Int)(num: Int): Int = {
    Iterator.iterate((num, 0)) { case (m, _) ⇒ (m / 10, m % 10)  }
      .takeWhile{case (m, d) ⇒ m > 0 || d > 0 }
      .drop(1).map(p => f(p._2)).sum
  }

  val sqSumDigits = sumDigitsIterator(x => x * x) _

  // Problem 2
  def cubeSumDigits: Int => Int = sumDigitsIterator((x: Int) => x * x * x)
  def isNotCubeHappy(num: Int): Boolean = {
    val (currNum: Int, seen: Set[Int]) = Iterator.iterate(num, Set[Int]()){
      case (currNum, seen) => (cubeSumDigits(currNum), seen + currNum)}
      .dropWhile{ case (currNum, seen) => currNum != 1 && !seen.contains(currNum) }
      .next()

    // Return true if repetition, otherwise current number is 1, so return false
    seen.contains(currNum) // || currNum != 1
  }

  // // Generate seqs
  // def generateCubeSums(num: Int): Unit = {
  //   var cubeSum = 0
  //   var seen = Set[Int]()
  //   while (cubeSum != 1 && !seen.contains(cubeSum)) {
  //     cubeSum = cubeSumDigits(num)
  //     print(cubeSum + " ")
  //     seen = seen + cubeSum
  //   }
  //   if (seen.contains(cubeSum))
  //     println(s"$cubeSum repeats")
  // }

  // Problem 3
  def collatz(n: Int): Seq[Int] = {
    Iterator.iterate((n, false)) {
      case (x, _) => (if (x % 2 == 0) x / 2  else 3 * x + 1, x == 1) }
      .takeWhile(!_._2).map(_._1).toSeq
  }

  // Problem 4
  def set3(a: Set[Int], b: Set[Int], c: Set[Int]): Set[Set[Int]] =
    a.flatMap(x => b.flatMap(y => c.map(z => Set(x, y, z))))

  // Problem 5
  def set3Alt(sets: Set[Set[Int]]): Set[Set[Int]] = {
    sets.foldLeft(Set[Set[Int]](Set())) {
      // Combine each of results so far with each element in current set
      case (accumSet: Set[Set[Int]], currSet: Set[Int]) => {
        for {
          s <- accumSet
          i <- currSet
        } yield s + i
      }
    }
  }
}
