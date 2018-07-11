package example

import org.scalatest.{FlatSpec, Matchers}
import cats.data.Writer
import cats.{Monad, ~>}
import cats.free.Free
import cats.syntax.monad._
import akka.http.scaladsl.server.Directives._
import DirectiveMonad._
import akka.http.scaladsl.server.{PathMatcher, Route}
import akka.http.scaladsl.server.PathMatchers.Segment

class AkkaHttpFreeMonad extends FlatSpec with Matchers {

  behavior of "akka-http monad adapter"

  // Define a data type for "route programs" that contain some akka-http directives.

  sealed trait RteProg[A]

  final case class PathD[A](pathMatcher: PathMatcher[A]) extends RteProg[A]

  final case object GetD extends RteProg[Unit]

  final case class PathPrefixD(prefix: String) extends RteProg[String]
  
  // Define an implicit conversion into the free monad.
  
  implicit def toFreeAHDir[A](aHDir: RteProg[A]): Free[RteProg, A] = Free.liftF(aHDir)
  
  // We can write route programs now, and also compose them.
  
  val routeProg1 = for {
    _ ← GetD
    _ ← PathPrefixD("test")
  } yield ()
  
  val routeProg2: Free[RteProg, Route] = for {
    _ ← routeProg1
    (s1, s2) ← PathD(Segment / Segment)
  } yield complete(s"OK, got $s1/$s2")
  
  // Define an interpreter from RteProg to Writer[String, ?].
  val toWriter
}
