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
    "-language:implicitConversions"
  ),
  libraryDependencies ++= Seq(
    scalatest, scalacheck
  )
)

lazy val scala_examples = (project in file("."))
  .settings(common)
  .aggregate(examples01, examples02)

lazy val examples01 = (project in file("examples01"))
  .settings(common)

lazy val examples02 = (project in file("examples02"))
  .settings(common)

lazy val examples03 = (project in file("examples03"))
  .settings(common)
  .settings(
    libraryDependencies ++= Seq(
      "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.6" % Test
    )
  )

lazy val examples04 = (project in file("examples04"))
  .settings(common)
  .settings(
    libraryDependencies ++= Seq(
      "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.6" % Test,
      "io.chymyst" %% "curryhoward" % "latest.integration"
    )
  )
