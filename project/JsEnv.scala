import sbt._
import sbt.Keys._
import org.scalajs.jsenv.nodejs.NodeJSEnv
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

import scala.sys.process._
import scala.util.Try

/** The JavaScript runtime of the Scala.js tests: Node.js, `$SCALAJS_RUNTIME` if set (a path to `node`), else `node` on the PATH.
  *
  * sbt always loads, and JVM-only work never needs Node. A JS task that needs a runtime (`test`, `testOnly`, `run`) fails at once with an explicit message when there is none; it is never skipped.
  */
object JsEnv {

  def runtime: String = sys.env.get("SCALAJS_RUNTIME").filter(_.trim.nonEmpty) getOrElse "node"

  private def version(executable: String): Option[String] =
    Try(Seq(executable, "--version").!!(ProcessLogger(_ => ()))).toOption.map(_.trim).filter(_.nonEmpty)

  lazy val checkJsRuntime = taskKey[Unit]("Fails unless the JS runtime of the Scala.js tests (Node.js) can be started")

  def nodeJSEnv(env: Map[String, String] = Map.empty): NodeJSEnv = new NodeJSEnv(NodeJSEnv.Config().withExecutable(runtime).withEnv(env))

  def settings: Seq[Setting[_]] = Seq(
    jsEnv          := nodeJSEnv(),
    checkJsRuntime := {
      val exe = runtime
      version(exe) match {
        case Some(v) => streams.value.log.debug(s"JS runtime: $exe $v")
        case None    =>
          throw new MessageOnlyException(
            s"No JavaScript runtime: can't run `$exe --version`. The Scala.js tests run on Node.js: put `node` on the PATH or set SCALAJS_RUNTIME to its path."
          )
      }
    },
    // every task that starts the JS runtime (test, testOnly, testQuick) loads the test frameworks first
    Test / loadedTestFrameworks := (Test / loadedTestFrameworks).dependsOn(checkJsRuntime).value,
    Compile / run               := (Compile / run).dependsOn(checkJsRuntime).evaluated
  )
}
