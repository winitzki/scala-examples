package example

trait PointedT[+A] {
  def point: A
}

object PointedT {
  def apply[A](implicit ev: PointedT[A]): A = ev.point

  implicit def pointedOption[A]: PointedT[Option[A]] = new PointedT[Option[A]] {
    override def point: Option[A] = None
  }

  implicit val pointedString: PointedT[String] = new PointedT[String] {
    override def point: String = ""
  }

  implicit def pointedFunction[A]: PointedT[A ⇒ A] = new PointedT[A ⇒ A] {
    override def point: A ⇒ A = identity
  }
}

trait Pointed[F[_]] {
  def point[A](x: A): F[A]
}

object Pointed {

  implicit class Syntax[F[_], A](f: F[A])(implicit ev: Pointed[F]) {
    def point(x: A): F[A] = ev.point(x)
  }
  implicit val pointedOption: Pointed[Option] = new Pointed[Option] {
    override def point[A](x: A): Option[A] = Some(x)
  }
}
