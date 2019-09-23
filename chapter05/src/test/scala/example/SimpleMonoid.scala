package example

import org.scalatest.{FlatSpec, Matchers}

final case class SimpleMonoid[T](combine: (T, T) ⇒ T, empty: T)

object SimpleMonoid {
  implicit val simpleMonoidInt: SimpleMonoid[Int] = SimpleMonoid(_ + _, 0)
  implicit val simpleMonoidString: SimpleMonoid[String] = SimpleMonoid(_ + _, "")

  implicit class SimpleMonoidOps[T: SimpleMonoid](t: T) {
    def |+|(x: T): T = implicitly[SimpleMonoid[T]].combine(t, x)
  }

  def monoidPair[A: SimpleMonoid, B: SimpleMonoid]: SimpleMonoid[(A, B)] = SimpleMonoid[(A, B)](
    { case ((a1, b1), (a2, b2)) => (a1 |+| a2, b1 |+| b2) },
    (implicitly[SimpleMonoid[A]].empty, implicitly[SimpleMonoid[B]].empty)
  )

  def monoidEitherPreferB[A: SimpleMonoid, B: SimpleMonoid] = SimpleMonoid[Either[A, B]]({
    case (Left(a1), Left(a2)) => Left(a1 |+| a2)
    case (Left(a), Right(b)) => Right(b) // "Take B".
    case (Right(b), Left(a)) => Right(b)
    case (Right(b1), Right(b2)) => Right(b1 |+| b2)
  }, Left(implicitly[SimpleMonoid[A]].empty))

  def monoidFunc1[R]: SimpleMonoid[R => R] = SimpleMonoid((f, g) => f andThen g, identity)
}

class SimpleMonoidSpec extends FlatSpec with Matchers {

  import SimpleMonoid._

  it should "define monoid instance for example 2" in {

    val monoidX1: SimpleMonoid[Either[Unit, String => String]] = {
      implicit val m1 = SimpleMonoid[Unit]((_, _) => (), ())
      implicit val m2: SimpleMonoid[String ⇒ String] = monoidFunc1[String]
      monoidEitherPreferB[Unit, String => String]
    }

    val monoidX: SimpleMonoid[Option[String => String]] = SimpleMonoid({
      case (None, None) => None
      case (None, Some(f)) => Some(f)
      case (Some(f), None) => Some(f)
      case (Some(f), Some(g)) => Some(f andThen g)
    }, None
    )
  }

  it should "define Monoid for partial functions" in {
    type Path = String // Here, the types `Path` and `Response` are defined as `String` only as
    type Response = String // an illustration. The code must treat these types as type parameters.

    type Route = PartialFunction[Path, Response]

    val r1: Route = {
      case "/get_users" => "user1, user2, user3"
    }
    val r2: Route = {
      case "/get_names" => "name1, name2, name3"
    }
    import cats.Monoid
    import cats.syntax.monoid._
    implicit val simpleMonoidRoute: Monoid[Route] = new Monoid[Route] {
      override def empty: Route = new PartialFunction[Path, Response] {
        override def isDefinedAt(path: Path): Boolean = false

        override def apply(path: Path): Response = ???
      }

      override def combine(x: Route, y: Route): Route = new PartialFunction[Path, Response] {
        override def isDefinedAt(path: Path): Boolean = x.isDefinedAt(path) || y.isDefinedAt(path)

        override def apply(path: Path): Response = if (x.isDefinedAt(path)) x(path) else y(path)
      }
    }
    val route = r1 |+| r2
    route("/get_users") shouldEqual "user1, user2, user3"
    route("/get_names") shouldEqual "name1, name2, name3"
    route.isDefinedAt("abcd") shouldEqual false

    def combine(r1: Route, r2: Route): Route = new PartialFunction[Path, Response] {
      def isDefinedAt(path: Path): Boolean = r1.isDefinedAt(path) || r2.isDefinedAt(path)
      def apply(path: Path): Response = if (r1.isDefinedAt(path)) r1(path) else r2(path)
    }

    def empty: Route = new PartialFunction[Path, Response] {
      def isDefinedAt(path: Path): Boolean = false
      def apply(path: Path): Response = ???
    }
  }
}