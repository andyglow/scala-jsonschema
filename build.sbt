import xerial.sbt.Sonatype._

lazy val commonSettings = Seq(

  organization := "com.github.andyglow",

  homepage := Some(new URL("http://github.com/andyglow/scala-jsonschema")),

  startYear := Some(2017),

  organizationName := "andyglow",

  publishTo := sonatypePublishTo.value,

  scalaVersion := "2.12.6",

  crossScalaVersions := Seq("2.12.6", "2.11.12"),

  scalacOptions ++= Seq(
    "-encoding", "UTF-8",
    "-feature",
    "-unchecked",
    "-deprecation",
    //  "-Xfatal-warnings",
    "-Xlint",
    "-Yno-adapted-args",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Xfuture"),

  scalacOptions in (Compile,doc) ++= Seq(
    "-groups",
    "-implicits",
    "-no-link-warnings"),

  licenses := Seq(("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))),

  sonatypeProfileName := "com.github.andyglow",

  publishMavenStyle := true,

  sonatypeProjectHosting := Some(
    GitHubHosting(
      "andyglow",
      "scala-jsonschema",
      "andyglow@gmail.com")),

  scmInfo := Some(
    ScmInfo(
      url("https://github.com/andyglow/scala-jsonschema"),
      "scm:git@github.com:andyglow/scala-jsonschema.git")),

  developers := List(
    Developer(
      id    = "andyglow",
      name  = "Andriy Onyshchuk",
      email = "andyglow@gmail.com",
      url   = url("https://ua.linkedin.com/in/andyglow"))),

  releaseCrossBuild := true,

  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % Test
)


lazy val core = { project in file("core") }.settings(
  commonSettings,

  name := "scala-jsonschema-core"
)

lazy val macros = project in file("macros") dependsOn core settings (
  commonSettings,

  name := "scala-jsonschema-macros",

  libraryDependencies ++= Seq(
    (scalaVersion apply ("org.scala-lang" % "scala-reflect" % _ % Compile)).value)
)

lazy val api = { project in file("api") }.dependsOn(core, macros).settings(
  commonSettings,

  name := "scala-jsonschema-api"
)

lazy val `play-json` = { project in file("play-json") }.dependsOn(core, api).settings(
  commonSettings,

  name := "scala-jsonschema-play-json",

  libraryDependencies += "com.typesafe.play" %% "play-json" % "2.6.3"
)

lazy val `spray-json` = { project in file("spray-json") }.dependsOn(core, api).settings(
  commonSettings,

  name := "scala-jsonschema-spray-json",

  libraryDependencies += "io.spray" %%  "spray-json" % "1.3.3"
)

lazy val `circe-json` = { project in file("circe-json") }.dependsOn(core, api).settings(
  commonSettings,

  name := "scala-jsonschema-circe-json",

  libraryDependencies ++= {
    val circeVersion = "0.8.0"

    Seq(
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-generic",
      "io.circe" %% "circe-parser"
    ) map { _ % circeVersion }
  }
)

lazy val root = { project in file(".") }.aggregate(
  core,
  macros,
  api,
  `play-json`,
  `circe-json`,
  `spray-json`).settings(

  commonSettings,

  name := "scala-jsonschema",

  aggregate in update := false
)