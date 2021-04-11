package example

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.PathMatchers.Segment
import akka.http.scaladsl.server.{PathMatcher, Route}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import cats.data.Writer
import cats.free.Free
import cats.~>
import example.DirectiveMonad._
import org.scalatest.{FlatSpec, Matchers}


class AkkaHttpFreeMonad extends FlatSpec with Matchers with ScalatestRouteTest {

  behavior of "akka-http monad adapter"

  // Define a data type for "route programs" that contain some akka-http directives.

  sealed trait RteProg[A]

  final case class PathD[A](pathMatcher: PathMatcher[A]) extends RteProg[A]

  final case object GetD extends RteProg[Unit]

  final case class PathPrefixD(prefix: String) extends RteProg[Unit]

  // Define an implicit conversion into the free monad.

  implicit def toFreeAHDir[A](aHDir: RteProg[A]): Free[RteProg, A] = Free.liftF(aHDir)

  val routeProg1: Free[RteProg, Unit] = for {
    _ ← GetD
    _ ← PathPrefixD("test")
  } yield ()

  val routeProg2: Free[RteProg, Route] = for {
    _ ← routeProg1
    s ← PathD("id" / Segment)
  } yield complete(s"OK, got id=$s")

  it should "interpret a route program using toWriter" in {
    // We can write route programs now, and also compose them.

    import cats.instances.all._

    // Define an interpreter from RteProg to Writer[String, ?].
    val toWriter: RteProg ~> Writer[String, *] = new (RteProg ~> Writer[String, *]) {
      override def apply[A](fa: RteProg[A]): Writer[String, A] = (fa match {
        case GetD ⇒ Writer(s"GET ", ())
        case PathPrefixD(pathPrefix) ⇒ Writer(s"/$pathPrefix", ())
        case PathD(pathMatcher) ⇒ Writer("/<pathMatcher>", null)
      }).map(_.asInstanceOf[A]) // Seems to be required.
    }

    // Run and extract the route information.
    routeProg2.foldMap(toWriter).run._1 shouldEqual "GET /test/<pathMatcher>"
  }

  it should "check that the route program works normally with akka-http" in {

    import CatsMonad._
    // Define a cats.Monad instance for WrapDirective.
    implicit val catsMonadWrapDirective: CatsMonad[WrapDirective] = new CatsMonad[WrapDirective] {
      override def flatMap[A, B](fa: WrapDirective[A])(f: A ⇒ WrapDirective[B]): WrapDirective[B] = fa.flatMap(f)

      override def pure[A](x: A): WrapDirective[A] = WrapDirective.pure(x)
    }

    val toDirective: RteProg ~> WrapDirective = new (RteProg ~> WrapDirective) {
      override def apply[A](fa: RteProg[A]): WrapDirective[A] = (fa match {
        case PathD(pathMatcher) ⇒ path(pathMatcher)
        case GetD ⇒ get
        case PathPrefixD(prefix) ⇒ pathPrefix(prefix)
      }).map(_.asInstanceOf[A])
    }

    val route: Route = routeProg2.foldMap(toDirective)

    Get("/test/id/1") ~> route ~> check {
      responseAs[String] shouldEqual "OK, got id=(1)"
    }
  }
}
