package object example {
  def unfold[T](init: T)(updater: T ⇒ T): Iterator[T] = new Iterator[T] {
    var current: T = init

    override def hasNext: Boolean = true

    override def next(): T = {
      val result = current
      current = updater(current)
      result
    }
  }

  def unfoldWhile[T](init: T)(updater: T ⇒ Option[T]): Iterator[T] = new Iterator[T] {
    var current: T = init

    var willHaveNext: Boolean = true

    override def hasNext: Boolean = willHaveNext

    override def next(): T = {
      val result = current
      updater(current) match {
        case Some(n) ⇒ current = n
        case None ⇒ willHaveNext = false
      }
      result
    }
  }
}
