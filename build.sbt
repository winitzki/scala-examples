val scalaV = "2.12.4"

lazy val scalatest = "org.scalatest" %% "scalatest" % "3.0.4" % Test
lazy val scalacheck = "org.scalacheck" %% "scalacheck" % "1.13.5" % Test

lazy val common = Seq(
  scalaVersion := scalaV,
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-language:existentials",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-Ypartial-unification"
  ),
  libraryDependencies ++= Seq(
    scalatest, scalacheck,
    "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.6" % Test
  )
)

lazy val scala_examples = (project in file("."))
  .settings(common)
  .aggregate(chapter01, chapter02, chapter03, chapter04)

lazy val chapter01 = (project in file("chapter01"))
  .settings(common)

lazy val chapter02 = (project in file("chapter02"))
  .settings(common)

lazy val chapter03 = (project in file("chapter03"))
  .settings(common)

lazy val chapter04 = (project in file("chapter04"))
  .settings(common)
  .settings(
    libraryDependencies ++= Seq(
      "io.chymyst" %% "curryhoward" % "latest.integration",
      "org.typelevel" %% "cats-core" % "1.0.0"
    )
  )
