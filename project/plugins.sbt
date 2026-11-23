addSbtPlugin("org.scoverage" %% "sbt-scoverage" % "2.4.4")
addSbtPlugin("com.github.sbt" % "sbt-release"   % "1.6.0")
addSbtPlugin("com.github.sbt" % "sbt-pgp"       % "2.4.0")

// github actions
addSbtPlugin("com.github.sbt" % "sbt-github-actions" % "0.32.2")

// Scala.js. 1.23.0 is the floor anyway: scaladoc-ast/-parser _sjs1 (compile deps of core on Scala 3 and of macros on Scala 2)
// are built with it, and consumers' linkers must be at least as new as the IR in the jars they link
addSbtPlugin("org.scala-js"       % "sbt-scalajs"              % "1.23.0")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.4.0")

// scaladoc: only on the unpublished test-models module (embeds scaladoc on Scala 2); the published modules declare
// scaladoc-ast/-parser by hand, because the plugin makes scaladoc-parser `Provided`
addSbtPlugin("io.github.andyglow" % "sbt-scaladoc" % "0.3.0")

// Format
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.6.2")

// Documentation
addSbtPlugin("com.lightbend.paradox" % "sbt-paradox"                % "0.11.1")
addSbtPlugin("com.github.sbt"        % "sbt-paradox-material-theme" % "0.8.0")
addSbtPlugin("com.github.sbt"        % "sbt-site-paradox"           % "1.8.0")
addSbtPlugin("com.github.sbt"        % "sbt-ghpages"                % "0.10.0")
addSbtPlugin("org.scalameta"         % "sbt-mdoc"                   % "2.9.2")

ThisBuild / libraryDependencySchemes += "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always
