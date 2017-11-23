val s = Seq(1, 3, 5, 7, 3, 5, 7, 3, 5, 7, 3, 5, 7).toIterator
def ex06[T](s: Iterator[T]): Iterator[T] = {
  s.map(x ⇒ Iterator.fill(2)(x)).flatten
}

def ex07[T](s: Seq[T]) = {
  s.toIterator.drop(1)
    .zip(ex06(s.toIterator).drop(1))
    .takeWhile { case (x, y) ⇒ x != y }
    .map { case (x, y) ⇒ x }
    .toList
}

ex07(Seq(1,3,5,7,3,5,7,3,5,7))


