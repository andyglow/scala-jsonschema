package com.github.andyglow.json

import scala.io.Source
import scala.util.{Failure, Success}
import org.scalatest.funsuite.AnyFunSuite


class ParseJsonBulkSpec extends AnyFunSuite {

  private lazy val clazz = classOf[ParseJsonBulkSpec]

  // for running in parallel in drone
  private def open(resourcePath: String, attempt: Int = 0): Source = {
    Thread.sleep(attempt * 1000)
    try {
      Source.fromInputStream(clazz.getResourceAsStream(resourcePath))
    } catch {
      case _: Throwable if attempt < 5 =>
        open(resourcePath, attempt + 1)
    }
  }

  lazy val examples = open("/examples")

  examples.getLines() foreach { name =>
    def run(expect: Boolean) = {
      val json = open("/" + name).mkString
      ParseJson(json) match {
        case Success(x) if !expect => fail(json + " should have fail, but: " + x)
        case Failure(x) if expect  => fail(json + " should have succeed", x)
        case Success(_)            =>
        case Failure(_)            =>
      }
    }

    if (name.startsWith("i_") || name.startsWith("y_"))
      ignore(name + " should succeed") { run(true) }
    else
      ignore(name + " should fail") { run(false) }
  }
}
