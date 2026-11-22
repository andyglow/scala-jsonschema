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

// scaladoc-ast/-parser for the published modules; keep in step with sbt-scaladoc in project/plugins.sbt
val scaladocV = "0.2.0"

def versionRangeDirs(base: File, scalaVersion: String): Seq[File] =
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, y)) if y <= 12 => Seq(base / "scala-2.12-")
    case Some((2, _))            => Seq(base / "scala-2.13+")
    case Some((3, _))            => Seq(base / "scala-2.13+")
    case _                       => Nil
  }

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
  // Not sbt-release's cross build: it loops over the ROOT's crossScalaVersions, which is Nil here, so test and
  // publishSigned would run for no Scala version at all. sbt's `+` uses each module's own crossScalaVersions.
  releaseCrossBuild             := false,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  releaseProcess                := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runClean,
    releaseStepCommandAndRemaining("+test"),
    setReleaseVersion,
    commitReleaseVersion,
    tagRelease,
    releaseStepCommandAndRemaining("+publishSigned"),
    // upload while the version is still the release one: after setNextVersion it's a SNAPSHOT and Central refuses it
    releaseStepCommand("sonaRelease"),
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
  libraryDependencies ++= { if (scalaV.value.isScala3) Seq("io.github.andyglow" %% "scaladoc-parser" % scaladocV) else Nil }
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
          "io.github.andyglow" %% "scaladoc-ast"    % scaladocV,
          "io.github.andyglow" %% "scaladoc-parser" % scaladocV
        ) ++ scalaReflect(scalaVersion.value)
    }
  )

// Models for api's tests, compiled in their own module: scaladoc of classes from another module. sbt-scaladoc embeds
// the scaladoc into the bytecode on Scala 2 (compiler plugin); on Scala 3 it's in the TASTy, and api's tests read it
// with -Yread-docs. Not published.
lazy val `test-models` = { project in file("modules/test-models") }
  .enablePlugins(ScaladocPlugin)
  .settings(
    commonSettings,
    name           := "scala-jsonschema-test-models",
    publish / skip := true
  )

lazy val api = { project in file("api") }
  .dependsOn(core, macros)
  .settings(
    commonSettings,
    name := "scala-jsonschema",
    // the profiles acceptance suite (api's petshop harness, run by every bridge module via test->test) validates real codec output against rendered schemas.
    // 1.5.x is the last line built for Java 8 (classfile 52); slf4j-nop mutes its logging
    libraryDependencies ++= Seq(
      "com.networknt" % "json-schema-validator" % "1.5.9"  % Test,
      "org.slf4j"     % "slf4j-nop"             % "2.0.17" % Test
    ),
    // test-models on the test classpath without `dependsOn`, which would put the unpublished module into api's POM
    Test / internalDependencyClasspath ++= (`test-models` / Compile / exportedProducts).value
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
    ) map { _ % "0.14.17" },
    // the acceptance suite's configured-codec case on Scala 2 (circe-generic-extras has no Scala 3 build; Scala 3 uses circe-core's io.circe.derivation)
    libraryDependencies ++= { if (scalaV.value.isScala3) Nil else Seq("io.circe" %% "circe-generic-extras" % "0.14.4" % Test) }
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
    libraryDependencies ++= Seq("com.lihaoyi" %% "ujson" % "4.4.3", "com.lihaoyi" %% "upickle" % "4.4.3"),
    // upickle 4.4.3's own Scala 3 macro (`ReadWriter.derived` for an enum) fails -Xcheck-macros ("Reference to a method must be eta-expanded")
    Test / scalacOptions -= "-Xcheck-macros"
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
    name                                := "scala-jsonschema-refined",
    libraryDependencies += "eu.timepit" %% "refined" % "0.11.4",
    libraryDependencies ++= scalaReflect(scalaVersion.value),

    // mute unused imports because otherwise IN TESTS it complains on
    // unused `import com.github.andyglow.jsonschema.RefinedSupport._`
    // which is not true (the derivation macro's own implicit search uses it; Scala 3 doesn't count that either)
    Test / scalacOptions --= Seq("-Ywarn-unused:imports,-patvars,-privates,-locals,-implicits", "-Wunused:imports")
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
    name := "scala-jsonschema-derived",
    libraryDependencies ++= scalaReflect(scalaVersion.value)
  )

// The documentation site. Sources: docs/src/main/paradox. `sbt makeDocs` runs mdoc (it compiles the ```scala mdoc snippets
// against the modules below, on 2.13) and then paradox via sbt-site; the site is written to docs/target/site.
lazy val docs = { project in file("docs") }
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
  .dependsOn(api, `joda-time`, cats, refined, enumeratum, derived, parser, `play-json`)
  .settings(
    ScalaVer.settings213,
    publish / skip := true,
    mdocIn         := baseDirectory.value / "src" / "main" / "paradox",
    mdocVariables  := Map("VERSION" -> version.value, "SCALA_VERSION" -> scalaVersion.value),
    // drop "github.base_url" (paradox derives it from scmInfo): its "Edit this page" links would point at the mdoc output
    // (docs/target/mdoc), which isn't in git
    Compile / paradoxProperties ~= { _ - "github.base_url" + ("project.name" -> "Scala JsonSchema") },
    Compile / paradox / sourceDirectory := mdocOut.value,
    git.remoteRepo                      := scmInfo.value.get.connection,

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
    `test-models`,
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

// ++2.13.18: the docs project is 2.13 only, and its dependencies must be built for the same Scala version
addCommandAlias("makeDocs", ";++2.13.18;docs/mdoc;docs/makeSite")
addCommandAlias("publishDocs", ";makeDocs;ghpagesPushSite")
//ParadoxMaterialThemePlugin.paradoxMaterialThemeSettings(Paradox)
