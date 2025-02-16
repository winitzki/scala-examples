val scalaV = "2.12.16"
val scala3V = "3.6.2"

lazy val commonSettings = Seq(
  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.13.2" cross CrossVersion.full),
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
    , "org.scalacheck" %% "scalacheck" % "1.14.0"
    , "org.typelevel" %% "cats-core" % "1.5.0"
    , "org.typelevel" %% "kittens" % "1.0.0-RC2" // 1.0.0-RC3 has an API change for derive.functor
    //    , "org.scalaz" %% "scalaz-core" % "7.2.24"
    , "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.6" % Test
    , "io.chymyst" %% "curryhoward" % "0.3.8"
    , "com.eed3si9n.expecty" %% "expecty" % "0.11.0" % Test
    , "org.typelevel" %% "spire" % "0.16.0"
    //    , "co.fs2" %% "fs2-core" % "1.0.2"
  )
)

lazy val scala_examples = (project in file("."))
  .settings(commonSettings)
  .aggregate(common, chapter01, chapter02, chapter03, chapter04,
    chapter05, chapter06, chapter07, chapter08, chapter09, chapter10, chapter11, trydotty)

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
  .settings(
    Test / parallelExecution := true,
//    Test / testOptions += Tests.Argument("-P"), // Make some tests run in parallel.
  )

lazy val chapter08 = (project in file("chapter08"))
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.apache.hadoop" % "hadoop-common" % "3.4.0" excludeAll(
        ExclusionRule(organization = "org.mortbay.jetty")
        , ExclusionRule(organization = "net.java.dev.jets3t")
        , ExclusionRule(organization = "org.apache.curator")
        , ExclusionRule(organization = "org.apache.zookeeper")
        , ExclusionRule(organization = "com.sun.jersey")
        // Replaced by a later version of commons-beanutils, see comment below.
        , ExclusionRule(organization = "commons-beanutils", name = "commons-beanutils-core")
      )
      /*
       * See http://commons.apache.org/proper/commons-beanutils/#BeanUtils_Core_And_Modules
       * Prior to 1.9.0, commons-beanutils was split into commons-beanutils, commons-beanutils-core, and
       * commons-beanutils-bean-collections. commons-beanutils was an aggregate of the latter two. This causes both
       * commons-beanutils-1.7.0 and commons-beanutils-core-1.8.0 to be on the classpath at the same time. Furthermore,
       * commons-beanutils contains classes that were copied from commons-collections, creating further conflicts. This
       * was all resolved in 1.9.0 which reverted to a single jar, commons-beanutils, without any copies of collections
       * classes. So here we explicitly use the new jar, and above, exclude the older commons-beanutils-core.
       */
      , "commons-beanutils" % "commons-beanutils" % "1.9.4"
      , "org.apache.hadoop" % "hadoop-hdfs-client" % "3.1.0"
      , "org.typelevel" %% "cats-free" % "1.1.0"
      , "com.typesafe.akka" %% "akka-http-testkit" % "10.0.10" % Test
      , "com.typesafe.akka" %% "akka-http" % "10.0.10"
      , "com.lihaoyi" %% "fastparse" % "1.0.0"
    )
  )
  .dependsOn(common)

lazy val chapter09 = (project in file("chapter09"))
  .settings(commonSettings)
  .dependsOn(common)

lazy val chapter10 = (project in file("chapter10"))
  .settings(commonSettings)
  .dependsOn(common)

lazy val chapter11 = (project in file("chapter11"))
  .settings(commonSettings)
  .dependsOn(common)

lazy val trydotty = project in file("trydotty") settings(
  scalaVersion := scala3V,
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.2.10" % Test,
    "org.scalatest" %% "scalatest-flatspec" % "3.2.10" % Test,
    "org.scalacheck" %% "scalacheck" % "1.15.4" % Test,
  )
)

lazy val prokopecBook = (project in file("prokopec-book"))
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "com.netflix.rxjava" % "rxjava-scala" % "0.20.7"
      , "com.lihaoyi" %% "scalarx" % "0.4.0"
      , "com.typesafe.akka" %% "akka-http-testkit" % "10.0.10" % Test
      , "com.typesafe.akka" %% "akka-http" % "10.0.10"
      , "io.chymyst" %% "chymyst-core" % "0.2.0"
      , "javax.xml.bind" % "jaxb-api" % "2.3.0" % Compile
    )
  )
  .dependsOn(common)
