import ReleaseTransformations._
import scala.sys.process._
import ScalaVer.scalaV
import CustomGithubActions._

// Sonatype Central Portal (sbt 1.11+ built-in): releases are staged locally by `publishSigned`
// and uploaded + released as a single bundle by `sonaRelease`
ThisBuild / publishTo := {
  val centralSnapshots = "https://central.sonatype.com/repository/maven-snapshots/"
  if (isSnapshot.value) Some("central-snapshots" at centralSnapshots)
  else localStaging.value
}
// The macro evaluates `Option` defaults of case classes with `c.eval`, which is not safe when several
// modules are compiled concurrently in the same JVM (toolbox importer races on `package com`).
// Compile one module at a time until that is fixed in the macro.
Global / concurrentRestrictions += Tags.limit(Tags.Compile, 1)

ThisBuild / versionScheme                       := Some("pvp")
ThisBuild / crossScalaVersions                  := ScalaVer.values.map(_.full)
ThisBuild / githubWorkflowPublishTargetBranches := Seq()
ThisBuild / githubWorkflowJavaVersions          := Seq(JavaSpec.temurin("21"))
ThisBuild / githubWorkflowBuildPreamble         := Seq(checkFormatting)
ThisBuild / githubWorkflowBuildPostamble        := Seq(generateCC, aggregateCC, uploadCC)

ThisBuild / scmInfo := Some(ScmInfo(url("https://github.com/andyglow/scala-jsonschema"), "scm:git@github.com:andyglow/scala-jsonschema.git"))

// Worktree-friendly: sbt-git (pulled in by sbt-ghpages) uses JGit, which fails on `git worktree` checkouts
// with "Bare Repository has neither a working tree, nor an index". The console git reader works everywhere.
useReadableConsoleGit

def scalaReflect(scalaVersion: String): Seq[ModuleID] =
  if (scalaVersion startsWith "3.") Nil else Seq(("org.scala-lang" % "scala-reflect" % scalaVersion % Compile).withSources().withJavadoc())

def versionRangeDirs(base: File, scalaVersion: String): Seq[File] =
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, y)) if y <= 12 => Seq(base / "scala-2.12-")
    case Some((2, _))            => Seq(base / "scala-2.13+")
    case Some((3, _))            => Seq(base / "scala-2.13+")
    case _                       => Nil
  }

// ============================================================================================================
// TEMPORARY SCAFFOLDING for the Scala 3 port. Delete this block, and its two uses below, once refined and
// derived are ported to Scala 3. Nothing here affects Scala 2.12/2.13 builds.
//
// scala2OnlyForNow: modules whose Scala 3 port hasn't landed yet (refined, derived). They still list
//    Scala 3 in crossScalaVersions, because with plain crossScalaVersions `++3.x` would otherwise leave them
//    on 2.12 while their dependencies switch to 3, which breaks resolution. On Scala 3 they compile nothing
//    and publish nothing.
lazy val scala2OnlyForNow = Seq(
  Compile / unmanagedSourceDirectories := { if (scalaV.value.isScala3) Nil else (Compile / unmanagedSourceDirectories).value },
  Test / unmanagedSourceDirectories    := { if (scalaV.value.isScala3) Nil else (Test / unmanagedSourceDirectories).value },
  publish / skip                       := scalaV.value.isScala3 || (publish / skip).value
)
// ============================ end of TEMPORARY SCAFFOLDING ==================================================

def runCommand(command: String): State => State =
  Command.process(command, _, msg => sys.error(s"can't parse command `$command`: $msg"))

lazy val buildInfo = taskKey[Unit]("Prints Build Info")

lazy val commonSettings = ScalaVer.settings ++ Seq(
  // logBuffered := false,

  organization     := "io.github.andyglow", // groupId only; Scala packages stay com.github.andyglow.* / json.*
  homepage         := Some(new URI("http://github.com/andyglow/scala-jsonschema").toURL),
  startYear        := Some(2017),
  organizationName := "andyglow",
  // sbt adds src/{main,test}/scala-2, scala-3, scala-2.12, scala-2.13 itself.
  // On top of that: scala-2.12- (2.12 collections) and scala-2.13+ (2.13 collections, also used by Scala 3).
  Compile / unmanagedSourceDirectories ++= versionRangeDirs(baseDirectory.value / "src" / "main", scalaVersion.value),
  Test / unmanagedSourceDirectories ++= versionRangeDirs(baseDirectory.value / "src" / "test", scalaVersion.value),
  scalacOptions := CompilerOptions(scalaV.value),
  // Scala 3 consumers of the derivation: read scaladoc from TASTy (other modules, files not recompiled), and check the generated trees
  Test / scalacOptions ++= { if (scalaV.value.isScala3) Seq("-Yread-docs", "-Xcheck-macros") else Nil },
  Compile / doc / scalacOptions := {
    val muted = Seq("-Ywarn-unused")

    val base = scalacOptions.value filterNot muted.contains

    base ++ CompilerOptions.doc(scalaV.value)
  },
  licenses          := Seq(("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))),
  publishMavenStyle := true,
  developers        := List(
    Developer(id = "andyglow", name = "Andriy Onyshchuk", email = "andyglow@gmail.com", url = url("https://ua.linkedin.com/in/andyglow"))
  ),
  releaseCrossBuild             := true,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  releaseProcess                := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runClean,
    runTest,
    setReleaseVersion,
    commitReleaseVersion,
    tagRelease,
    ReleaseStep(action = runCommand("publishSigned"), enableCrossBuild = true),
    // upload while the version is still the release one: after setNextVersion it's a SNAPSHOT and Central refuses it
    ReleaseStep(action = runCommand("sonaRelease")),
    setNextVersion,
    commitNextVersion,
    pushChanges
  ),
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.20" % Test
)

lazy val core = { project in file("core") }.settings(
  commonSettings,
  name := "scala-jsonschema-core",
  // the Scala 3 derivation lives in core (`derives json.Schema` needs `Schema.derived`); it parses `Symbol.docstring` with scaladoc-parser
  libraryDependencies ++= { if (scalaV.value.isScala3) Seq("io.github.andyglow" %% "scaladoc-parser" % "0.1.0") else Nil }
)

lazy val macros = { project in file("macros") }
  .dependsOn(core)
  .settings(
    commonSettings,
    name := "scala-jsonschema-macros",
    libraryDependencies ++= {
      if (scalaV.value.isScala3) Nil
      else
        Seq(
          "io.github.andyglow" %% "scaladoc-ast"    % "0.1.0",
          "io.github.andyglow" %% "scaladoc-parser" % "0.1.0"
        ) ++ scalaReflect(scalaVersion.value)
    }
  )

lazy val api = { project in file("api") }
  .dependsOn(core, macros)
  .settings(
    commonSettings,
    name := "scala-jsonschema"
  )

lazy val `play-json` = { project in file("modules/play-json") }
  .dependsOn(core, api % "compile->compile;test->test")
  .settings(
    commonSettings,
    name                                       := "scala-jsonschema-play-json",
    libraryDependencies += "org.playframework" %% "play-json" % "3.0.6",
    // play-json 3.0.6's own Scala 3 macro (`Json.writes`) fails -Xcheck-macros ("Expression created in a splice was used outside of that splice")
    Test / scalacOptions -= "-Xcheck-macros"
  )

lazy val `spray-json` = { project in file("modules/spray-json") }
  .dependsOn(core, api % "compile->compile;test->test")
  .settings(
    commonSettings,
    name                              := "scala-jsonschema-spray-json",
    libraryDependencies += "io.spray" %% "spray-json" % "1.3.6"
  )

lazy val `circe-json` = { project in file("modules/circe-json") }
  .dependsOn(core, api % "compile->compile;test->test")
  .settings(
    commonSettings,
    name := "scala-jsonschema-circe-json",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-generic",
      "io.circe" %% "circe-parser"
    ) map { _ % "0.14.17" }
  )

lazy val `json4s-json` = { project in file("modules/json4s-json") }
  .dependsOn(core, api % "compile->compile;test->test")
  .settings(
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
    Test / compile    := (Test / compile).dependsOn(buildInfo).value,
    name              := "scala-jsonschema-json4s-json",
    libraryDependencies ++= Seq("org.json4s" %% "json4s-core" % "4.0.7", "org.json4s" %% "json4s-native" % "4.0.7" % Test)
  )

lazy val `u-json` = { project in file("modules/u-json") }
  .dependsOn(core, api % "compile->compile;test->test")
  .settings(
    commonSettings,
    name := "scala-jsonschema-ujson",
    libraryDependencies ++= Seq("com.lihaoyi" %% "ujson" % "4.4.3", "com.lihaoyi" %% "upickle" % "4.4.3")
  )

lazy val `joda-time` = { project in file("modules/joda-time") }
  .dependsOn(core, api)
  .settings(
    commonSettings,
    name                              := "scala-jsonschema-joda-time",
    libraryDependencies += "joda-time" % "joda-time" % "2.15.1"
  )

lazy val `cats` = { project in file("modules/cats") }
  .dependsOn(core, api)
  .settings(
    commonSettings,
    name                                   := "scala-jsonschema-cats",
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.13.0"
  )

lazy val `refined` = { project in file("modules/refined") }
  .dependsOn(core, api)
  .settings(
    commonSettings,
    scala2OnlyForNow, // TEMPORARY SCAFFOLDING, see above
    name                                := "scala-jsonschema-refined",
    libraryDependencies += "eu.timepit" %% "refined" % "0.11.4",
    libraryDependencies ++= scalaReflect(scalaVersion.value),

    // mute unused imports because otherwise IN TESTS it complains on
    // unused `import com.github.andyglow.jsonschema.RefinedSupport._`
    // which is not true
    Test / scalacOptions -= "-Ywarn-unused:imports,-patvars,-privates,-locals,-implicits"
  )

lazy val parser = { project in file("modules/parser") }
  .dependsOn(core % "compile->compile;test->test", api)
  .settings(
    commonSettings,
    name := "scala-jsonschema-parser"
  )

lazy val enumeratum = { project in file("modules/enumeratum") }
  .dependsOn(core, api)
  .settings(
    commonSettings,
    name                                  := "scala-jsonschema-enumeratum",
    libraryDependencies += "com.beachape" %% "enumeratum" % "1.9.8",
    // enumeratum's own Scala 3 macro (`findValues`) needs the trees of the enum's members
    Test / scalacOptions ++= { if (scalaV.value.isScala3) Seq("-Yretain-trees") else Nil }
  )

lazy val derived = { project in file("modules/derived") }
  .dependsOn(core, api, macros)
  .settings(
    commonSettings,
    scala2OnlyForNow, // TEMPORARY SCAFFOLDING, see above
    name := "scala-jsonschema-derived",
    libraryDependencies ++= scalaReflect(scalaVersion.value)
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
    GhpagesPlugin
  )
//  .dependsOn(api, `joda-time`)
  .settings(
    ScalaVer.settings213,
    mdocIn        := baseDirectory.value / "main" / "paradox",
    mdocVariables := Map("VERSION" -> version.value, "SCALA_VERSION" -> scalaVersion.value),
    paradoxProperties ++= Map("project.name" -> "Scala JsonSchema", "github.base_url" -> "https://github.com/andyglow/scala-jsonschema"),
    Compile / paradox / sourceDirectory := mdocOut.value,
    git.remoteRepo                      := scmInfo.value.get.connection,
    Compile / paradoxMaterialTheme ~= {
      _.withCustomStylesheet("assets/styles.css")
    },
    Compile / paradoxMaterialTheme ~= {
      _.withCustomJavaScript("assets/dependencies.js")
    },

    // #color
    Compile / paradoxMaterialTheme ~= {
      _.withColor("teal", "indigo")
    }
    // #color
    ,
    // #repository
    Compile / paradoxMaterialTheme ~= {
      _.withRepository(uri("https://github.com/andyglow/scala-jsonschema"))
    }
    // #repository
    ,
    // #social
    Compile / paradoxMaterialTheme ~= {
      _.withSocial(uri("https://github.com/andyglow"))
    }
    // #social
    ,
    // #language
    Compile / paradoxMaterialTheme ~= {
      _.withLanguage(java.util.Locale.ENGLISH)
    }
    // #language
    // #analytics
//    Compile / paradoxMaterialTheme ~= {
//      _.withSocial(uri("https://github.com/andyglow"))
//    }
//    // #social
//    ,
//    // #language
//    Compile / paradoxMaterialTheme ~= {
//      _.withLanguage(java.util.Locale.ENGLISH)
//    }
//    // #language
//    ,
//    // #analytics
////    Compile / paradoxMaterialTheme ~= {
////      _.withGoogleAnalytics("UA-107934279-1") // Remember to change this!
////    }
//    // #analytics
////    ,
//    // #copyright
//    Compile / paradoxMaterialTheme ~= {
//      _.withCopyright("""
//        Inspired by <a href="https://github.com/coursera/autoschema">AutoSchema</a>
//        by <a href="https://github.com/coursera">Coursera</a>
//      """)
//    }
//    // #copyright
  )

lazy val root = { project in file(".") }
  .aggregate(
    core,
    macros,
    derived,
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
    `u-json`
  )
  .settings(
    commonSettings,
    name               := "scala-jsonschema-root",
    crossScalaVersions := Nil,
    publish / skip     := true,
    publishArtifact    := false,
    update / aggregate := false
  )

addCommandAlias("makeDocs", ";docs/mdoc;docs/makeSite")
addCommandAlias("publishDocs", ";makeDocs;ghpagesPushSite")
//ParadoxMaterialThemePlugin.paradoxMaterialThemeSettings(Paradox)
