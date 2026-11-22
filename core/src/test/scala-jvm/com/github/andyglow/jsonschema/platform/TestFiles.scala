package com.github.andyglow.jsonschema.platform

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

/** File access of the test harnesses, the same on the JVM and on Scala.js (where it goes through Node's `fs`). Paths are strings, relative to the working directory (the build's root) or absolute.
  * This is the JVM implementation: java.nio.
  */
object TestFiles {

  def exists(path: String): Boolean = Files.exists(Paths.get(path))

  def isDirectory(path: String): Boolean = Files.isDirectory(Paths.get(path))

  def resolve(dir: String, name: String): String = Paths.get(dir).resolve(name).toString

  def resolveSibling(path: String, name: String): String = Paths.get(path).resolveSibling(name).toString

  def absolute(path: String): String = Paths.get(path).toAbsolutePath.toString

  /** The file's text (UTF-8). */
  def read(path: String): String = new String(Files.readAllBytes(Paths.get(path)), UTF_8)

  /** Writes `text` (UTF-8) to `path`, creating its parent directories. */
  def write(path: String, text: String): Unit = {
    val p = Paths.get(path)
    Option(p.getParent) foreach { d => Files.createDirectories(d) }
    Files.write(p, text.getBytes(UTF_8))
    ()
  }

  def createTempDirectory(prefix: String): String = Files.createTempDirectory(prefix).toString

  /** A test resource. On the JVM it's looked up with the class loader of `anchor`; on Scala.js, which has no class path, in the test resource directories of the module under test. */
  def resource(anchor: Class[_], name: String): Array[Byte] = {
    val is = anchor.getClassLoader.getResourceAsStream(name)
    if (is == null) throw new IllegalStateException(s"resource not found: $name")
    try {
      val out = new java.io.ByteArrayOutputStream
      val buf = new Array[Byte](8192)
      var n   = is.read(buf)
      while (n != -1) { out.write(buf, 0, n); n = is.read(buf) }
      out.toByteArray
    } finally is.close()
  }

  /** A test resource as text (UTF-8). */
  def resourceText(anchor: Class[_], name: String): String = new String(resource(anchor, name), UTF_8)
}
