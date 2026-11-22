import sbtghactions.GenerativePlugin.autoImport.{UseRef, WorkflowStep}

object CustomGithubActions {

  // The build matrix has a `platform` axis: `jvm` rows run the JVM projects (root), `js` rows the Scala.js ones (rootJS)
  val onJvm = "matrix.platform == 'jvm'"
  val onJs  = "matrix.platform == 'js'"

  // the Node.js major the Scala.js tests run on (an LTS line); the owner's machine runs the same major
  val nodeVersion = "26"

  lazy val setupNode = WorkflowStep.Use(
    name = Some("Setup Node.js"),
    ref = UseRef.Public("actions", "setup-node", "v7"),
    params = Map("node-version" -> nodeVersion),
    cond = Some(onJs)
  )

  lazy val buildJvm = WorkflowStep.Sbt(name = Some("Build project (JVM)"), commands = List("test"), cond = Some(onJvm))

  lazy val buildJs = WorkflowStep.Sbt(name = Some("Build project (Scala.js)"), commands = List("rootJS/test"), cond = Some(onJs))

  // scalafmtCheckAll only sees the source dirs of the current Scala version (e.g. scala-3/ only under ++3.x),
  // so this runs in every Scala version's row; once per version (the JVM row): rootJS/scalafmtCheckAll covers the scala-js/ dirs.
  lazy val checkFormatting = WorkflowStep.Sbt(
    name = Some("Check formatting"),
    commands = List("scalafmtCheckAll", "rootJS/scalafmtCheckAll", "scalafmtSbtCheck"),
    cond = Some(onJvm)
  )

  private val coverageRow = s"matrix.scala == '${ScalaVer._213.full}' && $onJvm"

  lazy val generateCC = WorkflowStep.Sbt(
    name = Some("Generate Code Coverage Reports"),
    commands = List("clean", "coverage", "test"),
    cond = Some(coverageRow)
  )

  lazy val aggregateCC = WorkflowStep.Sbt(
    name = Some("Aggregate Code Coverage Report"),
    commands = List("coverageAggregate"),
    cond = Some(coverageRow)
  )

  lazy val uploadCC = WorkflowStep.Use(
    name = Some("Upload Code Coverage Report"),
    ref = UseRef.Public("codecov", "codecov-action", "v7"),
    cond = Some(coverageRow),
    params = Map(
      "token" -> "${{ secrets.CODECOV_TOKEN }}"
    )
  )
}
