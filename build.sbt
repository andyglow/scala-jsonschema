
name := "scala-jsonschema"

organization := "com.github.andyglow"

homepage := Some(new URL("http://github.com/andyglow/scala-jsonschema"))

startYear := Some(2017)

organizationName := "andyglow"

organizationHomepage := Some(url("http://evolutiongaming.com"))

scalaVersion := "2.12.3"

crossScalaVersions := Seq("2.12.3", "2.11.8")

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

publishMavenStyle := false

bintrayPackageLabels := Seq("scala", "tools", "websocket", "client")

bintrayRepository := "scala-tools"

bintrayOrganization := Some("andyglow")

pomExtra :=
  <scm>
    <url>git://github.com/andyglow/scala-jsonschema.git</url>
    <connection>scm:git://github.com/andyglow/scala-jsonschema.git</connection>
  </scm>
    <developers>
      <developer>
        <id>andyglow</id>
        <name>Andrey Onistchuk</name>
        <url>https://ua.linkedin.com/in/andyglow</url>
      </developer>
    </developers>

lazy val root = project in file(".")