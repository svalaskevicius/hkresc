
ThisBuild / scalaVersion     := "2.13.0"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.svalaskevicius"
ThisBuild / organizationName := "svalaskevicius"

val hedgehogVersion = "7ab9f74d7ca93864170cda181f4b4909156c7413"

lazy val root = (project in file("."))
  .settings(
    name := "hkresc",
    libraryDependencies ++= Seq(
      "org.specs2"                 %% "specs2-core"               % "4.6.0" % Test
    ),
    addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3"),
    scalacOptions ++= Seq(
      "-deprecation",
      "-language:_",
      "-target:jvm-1.8",
      "-encoding",
      "UTF-8",
      "-Yrangepos",
      /* "-Xlog-implicits", */
      "-explaintypes"
    ),
  )

