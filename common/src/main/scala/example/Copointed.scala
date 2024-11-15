package example


abstract class Copointed[F[_]] {
  def run[A](fa: F[A]): A
}

object Copointed {
  def apply[F[_] : Copointed]: Copointed[F] = implicitly[Copointed[F]]
}
