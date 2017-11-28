val scalaV = "2.12.4"

//lazy val rootProject = Some(scala_examples)

lazy val scala_examples = (project in file("."))
.settings(
scalaVersion := scalaV
)
.aggregate(examples01, examples02)

lazy val examples01 = (project in file("examples01"))
  .settings(
    scalaVersion := scalaV,
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.0.4" % Test
    )
  )

lazy val examples02 = (project in file("examples02"))
  .settings(
    scalaVersion := scalaV,
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.0.4" % Test
    )
  )
