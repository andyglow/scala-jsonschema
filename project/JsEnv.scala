import sbt._
import sbt.Keys._
import org.scalajs.jsenv.{Input, JSComRun, JSEnv, JSRun, RunConfig}
import org.scalajs.jsenv.nodejs.NodeJSEnv
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

import scala.sys.process._
import scala.util.Try

/** The JavaScript runtime of the Scala.js tests: Bun or Node.js, the first of these that answers `--version`:
  *
  *   - `$SCALAJS_RUNTIME` if set (a path to `bun` or `node`; `SCALAJS_RUNTIME=node` forces Node locally), and nothing else;
  *   - on CI (`$CI` set, as on GitHub Actions): `node`, then `bun`;
  *   - locally: `bun`, then `node`.
  *
  * Both run through Scala.js' `NodeJSEnv`, which pipes its launcher script to the runtime's stdin. Node reads stdin when it gets no script argument, Bun prints its usage and exits, so the runtime
  * always gets `-` ("read the script from stdin"), which both understand.
  *
  * The runtime is probed once per sbt session, by the first JS task that needs it, never when sbt loads: sbt always loads, and JVM-only work never needs a JS runtime. A JS task that needs one
  * (`test`, `testOnly`, `run`) fails at once with an explicit message when there is none; it is never skipped.
  */
object JsEnv {

  private def nonEmptyEnv(name: String): Option[String] = sys.env.get(name).map(_.trim).filter(_.nonEmpty)

  private def onCI: Boolean = nonEmptyEnv("CI").exists(v => v != "false" && v != "0")

  /** The executables to try, in order. */
  private def candidates: List[String] = nonEmptyEnv("SCALAJS_RUNTIME") match {
    case Some(exe)    => List(exe)
    case None if onCI => List("node", "bun")
    case None         => List("bun", "node")
  }

  private def version(executable: String): Option[String] =
    Try(Seq(executable, "--version").!!(ProcessLogger(_ => ()))).toOption.map(_.trim).filter(_.nonEmpty)

  /** The runtime's executable and version. */
  lazy val runtime: Option[(String, String)] = candidates.iterator.map(exe => version(exe).map(exe -> _)).collectFirst { case Some(found) => found }

  private def noRuntime = new MessageOnlyException(
    s"No JavaScript runtime: tried `--version` of ${candidates.mkString(", ")}. The Scala.js tests run on Bun or Node.js: put `bun` or `node` on the PATH, or set SCALAJS_RUNTIME to the path of one."
  )

  private val announced = new java.util.concurrent.atomic.AtomicBoolean(false)

  lazy val checkJsRuntime = taskKey[Unit]("Fails unless the JS runtime of the Scala.js tests (Bun or Node.js) can be started")

  /** A `NodeJSEnv` on [[runtime]], created when the first run starts: `jsEnv` is a setting, so building the env itself must not probe anything. */
  private final class RuntimeJSEnv(env: Map[String, String]) extends JSEnv {
    private lazy val underlying: NodeJSEnv = runtime match {
      case Some((exe, _)) => new NodeJSEnv(NodeJSEnv.Config().withExecutable(exe).withArgs(List("-")).withEnv(env))
      case None           => throw noRuntime
    }
    val name: String                                                                               = "Bun or Node.js"
    def start(input: Seq[Input], runConfig: RunConfig): JSRun                                      = underlying.start(input, runConfig)
    def startWithCom(input: Seq[Input], runConfig: RunConfig, onMessage: String => Unit): JSComRun = underlying.startWithCom(input, runConfig, onMessage)
  }

  def runtimeEnv(env: Map[String, String] = Map.empty): JSEnv = new RuntimeJSEnv(env)

  def settings: Seq[Setting[_]] = Seq(
    jsEnv          := runtimeEnv(),
    checkJsRuntime := {
      val log = streams.value.log
      runtime match {
        case Some((exe, v)) =>
          // once per sbt session at info, so a test log says which runtime ran it
          val how = if (nonEmptyEnv("SCALAJS_RUNTIME").isDefined) "from SCALAJS_RUNTIME" else "set SCALAJS_RUNTIME to change it"
          if (announced.compareAndSet(false, true)) log.info(s"JS runtime: $exe $v ($how)") else log.debug(s"JS runtime: $exe $v")
        case None => throw noRuntime
      }
    },
    // every task that starts the JS runtime (test, testOnly, testQuick) loads the test frameworks first
    Test / loadedTestFrameworks := (Test / loadedTestFrameworks).dependsOn(checkJsRuntime).value,
    Compile / run               := (Compile / run).dependsOn(checkJsRuntime).evaluated
  )
}
