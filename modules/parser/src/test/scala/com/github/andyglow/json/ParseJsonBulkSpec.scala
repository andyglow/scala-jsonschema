package com.github.andyglow.json

import scala.io.Source
import scala.util.{Failure, Success}
import org.scalatest.funsuite.AnyFunSuite


class ParseJsonBulkSpec extends AnyFunSuite {

  lazy val examples = Source.fromInputStream(classOf[ParseJsonBulkSpec].getResourceAsStream("/examples"))

  examples.getLines().foreach { name =>
    def run(expect: Boolean) = {
      val is = classOf[ParseJsonBulkSpec].getResourceAsStream("/" + name)
      val json = Source.fromInputStream(is).mkString
      ParseJson(json) match {
        case Success(_) if expect  =>
        case Success(x) if !expect => fail(json + " should have fail, but: " + x)
        case Failure(_) if !expect =>
        case Failure(x) if expect  => fail(json + " should have succeed", x)
      }
    }

    if (name.startsWith("i_") || name.startsWith("y_"))
      ignore(name + " should succeed") { run(true) }
    else
      ignore(name + " should fail") { run(false) }
  }
}
