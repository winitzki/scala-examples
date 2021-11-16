package example

trait Functor[F[_]]:
  def fmap[A, B](f: A ⇒ B): F[A] ⇒ F[B]

object Functor:
  def apply[F[_]: Functor]: Functor[F] = summon[Functor[F]]

extension [F[_]: Functor, A] (fa: F[A])
  def map[B](f: A ⇒ B): F[B] = Functor[F].fmap(f)(fa)

trait Zippable[F[_]]:
  def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)]

object Zippable:
  def apply[F[_] : Zippable]: Zippable[F] = implicitly[Zippable[F]]

  implicit class ZipOp[F[_] : Zippable, A](fa: F[A]):
    def zip[B](fb: F[B]): F[(A, B)] = implicitly[Zippable[F]].zip(fa, fb)

    def zipLeft[B](fb: F[B])(implicit fn: Functor[F]): F[A] = (fa zip fb).map(_._1)

    def zipRight[B](fb: F[B])(implicit fn: Functor[F]): F[B] = (fa zip fb).map(_._2)


