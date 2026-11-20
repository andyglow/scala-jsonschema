addSbtPlugin("org.scoverage" %% "sbt-scoverage" % "2.4.4")
addSbtPlugin("com.github.sbt" % "sbt-release"   % "1.6.0")
addSbtPlugin("com.github.sbt" % "sbt-pgp"       % "2.4.0")

// github actions
addSbtPlugin("com.github.sbt" % "sbt-github-actions" % "0.32.2")

// Format
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.6.2")

// Documentation
addSbtPlugin("com.lightbend.paradox" % "sbt-paradox"                % "0.11.1")
addSbtPlugin("com.github.sbt"        % "sbt-paradox-material-theme" % "0.8.0")
addSbtPlugin("com.github.sbt"        % "sbt-site-paradox"           % "1.8.0")
addSbtPlugin("com.github.sbt"        % "sbt-ghpages"                % "0.10.0")
addSbtPlugin("org.scalameta"         % "sbt-mdoc"                   % "2.9.2")

ThisBuild / libraryDependencySchemes += "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always
