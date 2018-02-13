val scalaV = "2.12.4"

lazy val commonSettings = Seq(
  addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.4" cross CrossVersion.binary),
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
    "org.scalatest" %% "scalatest" % "3.0.4"
    , "org.scalacheck" %% "scalacheck" % "1.13.5"
    , "org.typelevel" %% "cats-core" % "1.0.0"
    , "org.typelevel" %% "kittens" % "1.0.0-RC2"
//    , "org.scalaz" %% "scalaz-core" % "7.2.18"
    , "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.6" % Test
    , "io.chymyst" %% "curryhoward" % "latest.integration"
  )
)

lazy val scala_examples = (project in file("."))
  .settings(commonSettings)
  .aggregate(common, chapter01, chapter02, chapter03, chapter04,
    chapter05, chapter06, chapter07, exercises_solutions)

lazy val common = (project in file("common"))
  .settings(commonSettings)

lazy val chapter01 = (project in file("chapter01"))
  .settings(commonSettings)
  .dependsOn(common)

lazy val chapter02 = (project in file("chapter02"))
  .settings(commonSettings)
  .dependsOn(common)

lazy val chapter03 = (project in file("chapter03"))
  .settings(commonSettings)
  .dependsOn(common)

lazy val chapter04 = (project in file("chapter04"))
  .settings(commonSettings)
  .dependsOn(common)

lazy val chapter05 = (project in file("chapter05"))
  .settings(commonSettings)
  .dependsOn(common)

lazy val chapter06 = (project in file("chapter06"))
  .settings(commonSettings)
  .dependsOn(common)

lazy val chapter07 = (project in file("chapter07"))
  .settings(commonSettings)
  .dependsOn(common)

lazy val exercises_solutions = (project in file("exercises-solutions"))
  .settings(commonSettings)
  .dependsOn(common)
