import xerial.sbt.Sonatype._
import ReleaseTransformations._
import scala.sys.process._

// https://github.com/xerial/sbt-sonatype/issues/71
publishTo in ThisBuild := sonatypePublishTo.value

lazy val commonSettings = Seq(

  organization := "com.github.andyglow",

  homepage := Some(new URL("http://github.com/andyglow/scala-jsonschema")),

  startYear := Some(2017),

  organizationName := "andyglow",

  scalaVersion := "2.12.10",

  crossScalaVersions := Seq("2.11.12", "2.12.10", "2.13.1"),

  scalacOptions ++= {
    val options = Seq(
      "-encoding", "UTF-8",
      "-feature",
      "-unchecked",
      "-deprecation",
      "-Xfatal-warnings",
      "-Xlint",
      "-Yno-adapted-args",
      "-Ywarn-dead-code",
      "-Ywarn-numeric-widen",
//      "-Xlog-implicits",
//      "-Ytyper-debug",
      "-Xfuture",
      "-language:higherKinds")

    // WORKAROUND https://github.com/scala/scala/pull/5402
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 12)) => options.map {
        case "-Xlint"               => "-Xlint:-unused,_"
        case "-Ywarn-unused-import" => "-Ywarn-unused:imports,-patvars,-privates,-locals,-params,-implicits"
        case other                  => other
      }
      case Some((2, n)) if n >= 13  => options.filterNot { opt =>
        opt == "-Yno-adapted-args" || opt == "-Xfuture"
      } :+ "-Xsource:2.13"
      case _             => options
    }
  },

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

  releasePublishArtifactsAction := PgpKeys.publishSigned.value,

  releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runClean,
    runTest,
    setReleaseVersion,
    commitReleaseVersion,
    tagRelease,
    ReleaseStep(action = Command.process("publishSigned", _), enableCrossBuild = true),
    setNextVersion,
    commitNextVersion,
    ReleaseStep(action = Command.process("sonatypeReleaseAll", _), enableCrossBuild = true),
    pushChanges),
  
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test
)


lazy val core = { project in file("core") }.settings(
  commonSettings,

  name := "scala-jsonschema-core",

  libraryDependencies ++= Seq(
    (scalaVersion apply ("org.scala-lang" % "scala-reflect" % _ % Compile)).value.withSources.withJavadoc)
)

lazy val macros = project in file("macros") dependsOn core settings (
  commonSettings,

  name := "scala-jsonschema-macros",

  libraryDependencies ++= Seq(
    (scalaVersion apply ("org.scala-lang" % "scala-reflect" % _ % Compile)).value.withSources.withJavadoc)
)

lazy val api = { project in file("api") }.dependsOn(core, macros).settings(
  commonSettings,

  name := "scala-jsonschema-api"
)

lazy val `play-json` = { project in file("play-json") }.dependsOn(core, api % "compile->compile;test->test").settings(
  commonSettings,

  name := "scala-jsonschema-play-json",

  libraryDependencies += "com.typesafe.play" %% "play-json" % "2.7.4"
)

lazy val `spray-json` = { project in file("spray-json") }.dependsOn(core, api % "compile->compile;test->test").settings(
  commonSettings,

  name := "scala-jsonschema-spray-json",

  libraryDependencies += "io.spray" %%  "spray-json" % "1.3.5"
)

lazy val `circe-json` = { project in file("circe-json") }.dependsOn(core, api % "compile->compile;test->test").settings(
  commonSettings,

  name := "scala-jsonschema-circe-json",

  libraryDependencies ++= {
    val circeVersion = "0.12.0-M3"

    Seq(
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-generic",
      "io.circe" %% "circe-parser"
    ) map { _ % circeVersion }
  }
)

lazy val `json4s-json` = { project in file("json4s-json") }.dependsOn(core, api % "compile->compile;test->test").settings(
  commonSettings,

  name := "scala-jsonschema-json4s-json",

  libraryDependencies += "org.json4s" %% "json4s-core" % "3.6.7"
)

lazy val `u-json` = { project in file("u-json") }.dependsOn(core, api % "compile->compile;test->test").settings(
  commonSettings,

  name := "scala-jsonschema-ujson",

  libraryDependencies ++= {
    val ujsonVersion = CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 11)) => "0.7.4"
      case _             => "0.7.5"
    }
    
    Seq(
      "com.lihaoyi" %% "ujson" % ujsonVersion,
      "com.lihaoyi" %% "upickle" % ujsonVersion)

  }
)

lazy val `joda-time` = { project in file("joda-time") }.dependsOn(core, api).settings(
  commonSettings,

  name := "scala-jsonschema-joda-time",

  libraryDependencies += "joda-time" % "joda-time" % "2.10.3"
)

lazy val `cats` = { project in file("cats") }.dependsOn(core, api).settings(
  commonSettings,

  name := "scala-jsonschema-cats",

  libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0"
)

lazy val parser = { project in file("parser") }.dependsOn(core % "compile->compile;test->test", api).settings(
  commonSettings,

  name := "scala-jsonschema-parser"
)

lazy val root = { project in file(".") }.aggregate(
  core,
  macros,
  api,
  parser,
  `joda-time`,
  `play-json`,
  `circe-json`,
  `spray-json`,
  `json4s-json`,
  `u-json`).settings(

  commonSettings,

  name := "scala-jsonschema",
  
  crossScalaVersions := Nil,
  
  publish / skip := true,
  
  publishArtifact := false,

  aggregate in update := false
)