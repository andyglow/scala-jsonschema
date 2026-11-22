import sbtghactions.GenerativePlugin.autoImport.{UseRef, WorkflowStep}

object CustomGithubActions {

  // scalafmtCheckAll only sees the source dirs of the current Scala version (e.g. scala-3/ only under ++3.x),
  // so this runs in every matrix entry.
  lazy val checkFormatting = WorkflowStep.Sbt(
    name = Some("Check formatting"),
    commands = List("scalafmtCheckAll", "scalafmtSbtCheck")
  )

  lazy val generateCC = WorkflowStep.Sbt(
    name = Some("Generate Code Coverage Reports"),
    commands = List("clean", "coverage", "test"),
    cond = Some(s"matrix.scala == '${ScalaVer._213.full}'")
  )

  lazy val aggregateCC = WorkflowStep.Sbt(
    name = Some("Aggregate Code Coverage Report"),
    commands = List("coverageAggregate"),
    cond = Some(s"matrix.scala == '${ScalaVer._213.full}'")
  )

  lazy val uploadCC = WorkflowStep.Use(
    name = Some("Upload Code Coverage Report"),
    ref = UseRef.Public("codecov", "codecov-action", "v7"),
    cond = Some(s"matrix.scala == '${ScalaVer._213.full}'"),
    params = Map(
      "token" -> "${{ secrets.CODECOV_TOKEN }}"
    )
  )
}
