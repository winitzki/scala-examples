package example


object PipeOps {

  // Define pipe.
  implicit class PipeOp[A](val x: A) extends AnyVal {
    def pipe[B](f: A ⇒ B): B = f(x)

    def |>[B](f: A ⇒ B): B = f(x)
  }

}
