import sbtghactions.GenerativePlugin.autoImport.{UseRef, WorkflowStep}

object CustomGithubActions {

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
    ref = UseRef.Public("codecov", "codecov-action", "v3"),
    cond = Some(s"matrix.scala == '${ScalaVer._213.full}'"),
    params = Map(
      "token" -> "${{ secrets.CODECOV_TOKEN }}"
    )
  )
}
