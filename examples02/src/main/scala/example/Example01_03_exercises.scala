package example

import scala.annotation.tailrec

object Example01_03_exercises {

  def digits(n: Int): Iterator[Int] = unfold((n, 0, true)) { case (m, _, x) ⇒ (m / 10, m % 10, x && (m > 0)) }.takeWhile(_._3).map(_._2)

  // Compute the sum of digits of `n`.
  def ex07(n: Int): Int = unfoldWhile((n, 0)) { case (m, d) ⇒
    if (m > 0) Some((m / 10, m % 10)) else None
  }.map(_._2).sum

  def ex07a(n: Int): Int = {
    @tailrec
    def ex07rec(n: Int, sum: Int): Int = {
      if (n == 0)
        sum
      else
        ex07rec(n / 10, sum + (n % 10))
    }

    ex07rec(n, 0)
  }

  // Use unfold and takeWhile.
  def ex07b(n: Int): Int = digits(n).sum

  // Compute repeated sum of digits of `n`.

  def ex08(n: Int): Int = unfold(n)(x ⇒ digits(x).sum).find(_ < 10).getOrElse(0)

  // Compute sum of squared digits of `n`.

  def ex09(n: Int): Int = digits(n).map(x ⇒ x * x).sum

  // Compute repeated sum of squared digits of `n`. Repeat until a periodic subsequence is found. Return the smallest number seen.

  def ex10(n: Int): Int = {
    unfold((n, n, false)) { case (fast, slow, toggle) ⇒
      val newSlow = if (toggle) ex09(slow) else slow
      (ex09(fast), newSlow, !toggle)
    } // produce an Iterator[(Int, Int, Boolean)]
      .drop(1) // first element is always (n, n, false), not useful
      .scanLeft((Integer.MAX_VALUE, false)) { case ((oldMin, finished), (fast, slow, _)) ⇒
        (math.min(fast, oldMin), finished || (fast == slow))
      } // produce an Iterator[(Int, Boolean)]
      .find(_._2).get // find a `true` value among booleans - guaranteed to exist
      ._1 // get the integer value
  }

}
