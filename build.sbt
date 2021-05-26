import xerial.sbt.Sonatype._
import ReleaseTransformations._
import scala.sys.process._
import ScalaVer.scalaV

// https://github.com/xerial/sbt-sonatype/issues/71
ThisBuild / publishTo  := sonatypePublishTo.value

ThisBuild / versionScheme := Some("pvp")

lazy val buildInfo = taskKey[Unit]("Prints Build Info")

lazy val commonSettings = ScalaVer.settings ++ Seq(

  // logBuffered := false,

  organization := "com.github.andyglow",

  homepage := Some(new URL("http://github.com/andyglow/scala-jsonschema")),

  startYear := Some(2017),

  organizationName := "andyglow",

  Compile / unmanagedSourceDirectories ++= {
    val bd = baseDirectory.value
    def extraDirs(suffix: String): Seq[File] = Seq(bd / "src" / "main" / s"scala$suffix")
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, y)) if y <= 12 => extraDirs("-2.12-")
      case Some((2, y)) if y >= 13 => extraDirs("-2.13+")
      case _                       => Nil
    }
  },

  Test / unmanagedSourceDirectories ++= {
    val bd = baseDirectory.value
    def extraDirs(suffix: String): Seq[File] = Seq(bd / "src" / "test" / s"scala$suffix")
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, y)) if y <= 12 => extraDirs("-2.12-")
      case Some((2, y)) if y >= 13 => extraDirs("-2.13+")
      case _                       => Nil
    }
  },

  scalacOptions := CompilerOptions(scalaV.value),

  Compile / doc / scalacOptions := {
    val muted = Seq(
      "-Ywarn-unused")

    val base = scalacOptions.value filterNot muted.contains

    base ++ Seq(
      "-groups",
      "-implicits",
      "-no-link-warnings")
  },

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
  
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test
)


lazy val core = { project in file("core") }.settings(
  commonSettings,

  name := "scala-jsonschema-core"
)

lazy val macros = { project in file("macros") }.dependsOn(core).settings (
  commonSettings,

  name := "scala-jsonschema-macros",

  libraryDependencies ++= Seq(
    "com.github.andyglow" %% "scaladoc-ast"    % "0.0.12",
    "com.github.andyglow" %% "scaladoc-parser" % "0.0.12",
    (scalaVersion apply ("org.scala-lang" % "scala-reflect" % _ % Compile)).value.withSources.withJavadoc)
)

lazy val api = { project in file("api") }.dependsOn(core, macros).settings(
  commonSettings,

  name := "scala-jsonschema"
)

lazy val `play-json` = { project in file("modules/play-json") }.dependsOn(core, api % "compile->compile;test->test").settings(
  commonSettings,

  name := "scala-jsonschema-play-json",

  libraryDependencies += {
    val playV = CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 11)) => "2.7.4"
      case _             => "2.8.1"
    }

    "com.typesafe.play" %% "play-json" % playV
  }
)

lazy val `spray-json` = { project in file("modules/spray-json") }.dependsOn(core, api % "compile->compile;test->test").settings(
  commonSettings,

  name := "scala-jsonschema-spray-json",

  libraryDependencies += "io.spray" %%  "spray-json" % "1.3.6"
)

lazy val `circe-json` = { project in file("modules/circe-json") }.dependsOn(core, api % "compile->compile;test->test").settings(
  commonSettings,

  name := "scala-jsonschema-circe-json",

  libraryDependencies ++= {
    val circeV = CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 11)) => "0.12.0-M3"
      case _             => "0.13.0"
    }

    Seq(
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-generic",
      "io.circe" %% "circe-parser"
    ) map { _ % circeV }
  }
)

lazy val `json4s-json` = { project in file("modules/json4s-json") }.dependsOn(core, api % "compile->compile;test->test").settings(
  commonSettings,

  buildInfo := {
    val sb = new StringBuilder("*** SCALA INFO ***\n")
    sb append s"- Version: ${scalaVersion.value}\n"
    sb append s"- Version: ${scalaV.value}\n"
    sb append "- Compiler Options:\n"
    scalacOptions.value foreach { opt =>
      sb append s"  [$opt]\n"
    }

    streams.value.log.info(sb.toString)
  },
  Compile / compile := (Compile / compile).dependsOn(buildInfo).value,
  Test / compile := (Test / compile).dependsOn(buildInfo).value,

  name := "scala-jsonschema-json4s-json",

  libraryDependencies ++= Seq(
    "org.json4s" %% "json4s-core" % "4.0.0",
    "org.json4s" %% "json4s-native" % "4.0.0" % Test)
)

lazy val `u-json` = { project in file("modules/u-json") }.dependsOn(core, api % "compile->compile;test->test").settings(
  commonSettings,

  name := "scala-jsonschema-ujson",

  libraryDependencies ++= {
    val uV = CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 11)) => "0.7.4"
      case _             => "1.0.0"
    }

    Seq(
      "com.lihaoyi" %% "ujson" % uV,
      "com.lihaoyi" %% "upickle" % uV)

  }
)

lazy val `joda-time` = { project in file("modules/joda-time") }.dependsOn(core, api).settings(
  commonSettings,

  name := "scala-jsonschema-joda-time",

  libraryDependencies += "joda-time" % "joda-time" % "2.10.10"
)

lazy val `cats` = { project in file("modules/cats") }.dependsOn(core, api).settings(
  commonSettings,

  name := "scala-jsonschema-cats",

  libraryDependencies += {
    val catsV = CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 11)) => "2.0.0"
      case _             => "2.1.1"
    }

    "org.typelevel" %% "cats-core" % catsV
  }
)


lazy val `refined` = { project in file("modules/refined") }.dependsOn(core, api).settings(
  commonSettings,

  name := "scala-jsonschema-refined",

  libraryDependencies += {
    val refinedV = CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 11)) => "0.9.12"
      case _             => "0.9.13"
    }

    "eu.timepit" %% "refined" % refinedV
  },
  libraryDependencies += (scalaVersion apply ("org.scala-lang" % "scala-reflect" % _ % Compile)).value.withSources.withJavadoc,

  // mute unused imports because otherwise IN TESTS it complains on
  // unused `import com.github.andyglow.jsonschema.RefinedSupport._`
  // which is not true
  Test / scalacOptions -= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 11))           => "-Ywarn-unused-import"
      case Some((2, 12))           => "-Ywarn-unused:imports,-patvars,-privates,-locals,-implicits"
      case Some((2, n)) if n >= 13 => "-Ywarn-unused:imports,-patvars,-privates,-locals,-implicits"
      case _                       => ""
    }
  }
)

lazy val parser = { project in file("modules/parser") }.dependsOn(core % "compile->compile;test->test", api).settings(
  commonSettings,

  name := "scala-jsonschema-parser"
)

lazy val enumeratum = { project in file("modules/enumeratum") }.dependsOn(core, api).settings(
  commonSettings,

  name := "scala-jsonschema-enumeratum",

  libraryDependencies += "com.beachape" %% "enumeratum" % "1.6.1",
)

lazy val docs = { project in file("documentation") }
  .enablePlugins(
    // - variable substitution
    // - snippets evaluation
    MdocPlugin,
    // - markdown -> html
    ParadoxPlugin,
    // material design theme
    ParadoxMaterialThemePlugin,
    // paradox integration with sbt-site
    ParadoxSitePlugin,
    // publishing to GitHub Pages
    GhpagesPlugin)
  .dependsOn(api, `joda-time`)
  .settings(
    commonSettings,

    mdocIn := baseDirectory.value / "main" / "paradox",

    mdocVariables := Map("VERSION" -> version.value, "SCALA_VERSION" -> scalaVersion.value),

    paradoxProperties ++= Map(
      "project.name" -> "Scala JsonSchema",
      "github.base_url" -> "https://github.com/andyglow/scala-jsonschema"),

    Compile / paradox / sourceDirectory := mdocOut.value,

    git.remoteRepo := scmInfo.value.get.connection,

    Compile / paradoxMaterialTheme ~= {
      _.withCustomStylesheet("assets/styles.css")
    },

    Compile / paradoxMaterialTheme ~= {
      _.withCustomJavaScript("assets/dependencies.js")
    },

    //#color
    Compile / paradoxMaterialTheme ~= {
      _.withColor("teal", "indigo")
    }
    //#color
    ,
    //#repository
    Compile / paradoxMaterialTheme ~= {
      _.withRepository(uri("https://github.com/andyglow/scala-jsonschema"))
    }
    //#repository
    ,
    //#social
    Compile / paradoxMaterialTheme ~= {
      _.withSocial(
        uri("https://github.com/andyglow"))
    }
    //#social
    ,
    //#language
    Compile / paradoxMaterialTheme ~= {
      _.withLanguage(java.util.Locale.ENGLISH)
    }
    //#language
    ,
    //#analytics
//    Compile / paradoxMaterialTheme ~= {
//      _.withGoogleAnalytics("UA-107934279-1") // Remember to change this!
//    }
    //#analytics
//    ,
    //#copyright
    Compile / paradoxMaterialTheme ~= {
      _.withCopyright("""
        Inspired by <a href="https://github.com/coursera/autoschema">AutoSchema</a>
        by <a href="https://github.com/coursera">Coursera</a>
      """)
    }
    //#copyright
  )

lazy val root = { project in file(".") }
  .aggregate(
    core,
    macros,
    api,
    parser,
    refined,
    enumeratum,
    `joda-time`,
    `cats`,
    `play-json`,
    `circe-json`,
    `spray-json`,
    `json4s-json`,
    `u-json`)
  .settings(

    commonSettings,

    name := "scala-jsonschema-root",

    crossScalaVersions := Nil,

    publish / skip := true,

    publishArtifact := false,

    update / aggregate := false
  )

addCommandAlias("makeDocs", ";docs/mdoc;docs/makeSite")
addCommandAlias("publishDocs", ";makeDocs;ghpagesPushSite")
//ParadoxMaterialThemePlugin.paradoxMaterialThemeSettings(Paradox)

