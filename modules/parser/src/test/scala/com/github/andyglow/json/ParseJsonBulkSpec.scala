package com.github.andyglow.json

import java.io.InputStream

import scala.io.Source
import scala.util.{Failure, Success}
import org.scalatest.funsuite.AnyFunSuite

/** Runs the cases of https://github.com/nst/JSONTestSuite against [[ParseJson]].
  *
  *   - `y_*` must be accepted
  *   - `n_*` must be rejected
  *   - `i_*` are implementation-defined: either outcome is fine, but parsing must not blow up
  */
class ParseJsonBulkSpec extends AnyFunSuite {

  private def resource(path: String): InputStream = {
    val is = classOf[ParseJsonBulkSpec].getResourceAsStream(path)
    if (is == null) throw new IllegalStateException(s"resource not found: $path")
    is
  }

  private def bytes(name: String): Array[Byte] = {
    val is = resource("/" + name)
    try Iterator.continually(is.read()).takeWhile(_ != -1).map(_.toByte).toArray
    finally is.close()
  }

  private val names: List[String] = {
    val src = Source.fromInputStream(resource("/examples"), "UTF-8")
    try src.getLines().map(_.trim).filter(_.nonEmpty).toList
    finally src.close()
  }

  names foreach { name =>
    def parse() = ParseJson(bytes(name))

    name.take(2) match {
      case "y_" =>
        test(s"$name should succeed") {
          parse() match {
            case Failure(err) => fail(s"$name should have succeeded", err)
            case Success(_)   =>
          }
        }
      case "n_" =>
        test(s"$name should fail") {
          parse() match {
            case Success(x) => fail(s"$name should have failed, but got: $x")
            case Failure(_) =>
          }
        }
      case "i_" =>
        test(s"$name may succeed or fail") {
          parse()
        }
      case _ =>
        test(s"$name is a known test case") {
          fail(s"unexpected test case name: $name")
        }
    }
  }
}
