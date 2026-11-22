package com.github.andyglow.jsonschema.platform

import java.nio.charset.StandardCharsets.UTF_8

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray.{Int8Array, Uint8Array}

/** File access of the test harnesses, the same on the JVM and on Scala.js. Paths are strings, relative to the working directory (the build's root, which sbt gives the Node.js process too) or
  * absolute. This is the Scala.js implementation: Node's `fs`, `path` and `os`.
  */
object TestFiles {

  @js.native
  @JSImport("fs", JSImport.Namespace)
  private object fs extends js.Object {
    def existsSync(path: String): Boolean                            = js.native
    def statSync(path: String): js.Dynamic                           = js.native
    def readFileSync(path: String): Uint8Array                       = js.native
    def writeFileSync(path: String, data: String, enc: String): Unit = js.native
    def mkdirSync(path: String, options: js.Object): js.Any          = js.native
    def mkdtempSync(prefix: String): String                          = js.native
  }

  @js.native
  @JSImport("path", JSImport.Namespace)
  private object path extends js.Object {
    def join(a: String, b: String): String = js.native
    def dirname(p: String): String         = js.native
    def resolve(p: String): String         = js.native
    def normalize(p: String): String       = js.native
    val delimiter: String                  = js.native
  }

  @js.native
  @JSImport("os", JSImport.Namespace)
  private object os extends js.Object {
    def tmpdir(): String = js.native
  }

  def exists(p: String): Boolean = fs.existsSync(p)

  def isDirectory(p: String): Boolean = fs.existsSync(p) && fs.statSync(p).isDirectory().asInstanceOf[Boolean]

  def resolve(dir: String, name: String): String = path.normalize(path.join(dir, name))

  def resolveSibling(p: String, name: String): String = path.normalize(path.join(path.dirname(p), name))

  def absolute(p: String): String = path.resolve(p)

  private def readBytes(p: String): Array[Byte] = {
    val u = fs.readFileSync(p)
    new Int8Array(u.buffer, u.byteOffset, u.length).toArray
  }

  /** The file's text (UTF-8). */
  def read(p: String): String = new String(readBytes(p), UTF_8)

  /** Writes `text` (UTF-8) to `p`, creating its parent directories. */
  def write(p: String, text: String): Unit = {
    fs.mkdirSync(path.dirname(p), js.Dynamic.literal(recursive = true))
    fs.writeFileSync(p, text, "utf8")
  }

  def createTempDirectory(prefix: String): String = fs.mkdtempSync(path.join(os.tmpdir(), prefix))

  /** The test resource directories of the module under test: sbt passes them in `SCALAJS_TEST_RESOURCES` (see `jsSettings` in build.sbt). */
  private lazy val resourceDirs: List[String] = {
    val v = js.Dynamic.global.process.env.SCALAJS_TEST_RESOURCES
    if (js.isUndefined(v)) Nil else v.asInstanceOf[String].split(java.util.regex.Pattern.quote(path.delimiter)).toList.filter(_.nonEmpty)
  }

  /** A test resource. On the JVM it's looked up with the class loader of `anchor`; on Scala.js, which has no class path, in the test resource directories of the module under test. */
  def resource(anchor: Class[_], name: String): Array[Byte] =
    resourceDirs.map(path.join(_, name)).find(fs.existsSync) match {
      case Some(file) => readBytes(file)
      case None       => throw new IllegalStateException(s"resource not found: $name (looked in ${resourceDirs.mkString(", ")})")
    }

  /** A test resource as text (UTF-8). */
  def resourceText(anchor: Class[_], name: String): String = new String(resource(anchor, name), UTF_8)
}
