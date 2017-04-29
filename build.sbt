
name := "scala-jsonschema"

organization := "com.github.andyglow"

homepage := Some(new URL("http://github.com/andyglow/scala-jsonschema"))

startYear := Some(2017)

organizationName := "AndyGlow"

organizationHomepage := Some(url("http://evolutiongaming.com"))

bintrayOrganization := Some("andyglow")

scalaVersion := "2.12.1"

crossScalaVersions := Seq("2.12.1", "2.11.8")

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
  "-Xfuture"
)

scalacOptions in (Compile,doc) ++= Seq("-groups", "-implicits", "-no-link-warnings")

libraryDependencies ++= Seq(
  (scalaVersion apply ("org.scala-lang" % "scala-reflect" % _ % Compile)).value,
  "org.scalatest" %% "scalatest" % "3.0.1" % Test
)

licenses := Seq(("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0")))

releaseCrossBuild := true