// Build
addSbtPlugin("org.scala-js"       % "sbt-scalajs"              % "1.17.0")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2")

// Publishing
addSbtPlugin("org.scoverage" %% "sbt-scoverage" % "2.0.12")
addSbtPlugin("org.xerial.sbt" % "sbt-sonatype"  % "3.11.3")
addSbtPlugin("com.github.sbt" % "sbt-release"   % "1.4.0")
addSbtPlugin("com.github.sbt" % "sbt-pgp"       % "2.2.1")

// github actions
addSbtPlugin("com.github.sbt" % "sbt-github-actions" % "0.24.0")

// Format
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.5.2")

// Documentation
addSbtPlugin("com.lightbend.paradox" % "sbt-paradox"                % "0.10.6")
addSbtPlugin("com.github.sbt"        % "sbt-paradox-material-theme" % "0.7.0")
addSbtPlugin("com.typesafe.sbt"      % "sbt-site"                   % "1.4.1")
addSbtPlugin("com.typesafe.sbt"      % "sbt-ghpages"                % "0.6.3")
addSbtPlugin("org.scalameta"         % "sbt-mdoc"                   % "2.5.4")

ThisBuild / libraryDependencySchemes += "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always
