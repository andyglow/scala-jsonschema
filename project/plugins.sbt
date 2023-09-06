addSbtPlugin("org.scoverage" %% "sbt-scoverage" % "2.0.8")
addSbtPlugin("org.xerial.sbt" % "sbt-sonatype"  % "3.9.21")
addSbtPlugin("com.github.sbt" % "sbt-release"   % "1.1.0")
addSbtPlugin("com.github.sbt" % "sbt-pgp"       % "2.2.1")

// github actions
addSbtPlugin("com.github.sbt" % "sbt-github-actions" % "0.16.0")

// Format
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.5.0")

// Documentation
addSbtPlugin("com.lightbend.paradox" % "sbt-paradox"                % "0.10.5")
addSbtPlugin("io.github.jonas"       % "sbt-paradox-material-theme" % "0.6.0")
addSbtPlugin("com.typesafe.sbt"      % "sbt-site"                   % "1.4.1")
addSbtPlugin("com.typesafe.sbt"      % "sbt-ghpages"                % "0.6.3")
addSbtPlugin("org.scalameta"         % "sbt-mdoc"                   % "2.3.7")

ThisBuild / libraryDependencySchemes += "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always
