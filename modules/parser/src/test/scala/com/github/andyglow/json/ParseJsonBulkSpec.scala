package com.github.andyglow.json

import scala.io.Source
import scala.util.{Failure, Success}
import org.scalatest.funsuite.AnyFunSuite

class ParseJsonBulkSpec extends AnyFunSuite {

  private lazy val clazz = classOf[ParseJsonBulkSpec]

  private lazy val CI_WS_PATH = {
    def env[T](x: String)(fn: String => T): Option[T] = {
      sys.env
        .get(x)
        .flatMap { x =>
          try Some(fn(x))
          catch { case _: Throwable => None }
        }
    }
    def CI    = env("CI")(_.toBoolean) getOrElse false
    def DRONE = env("DRONE")(_.toBoolean) getOrElse false
    def DRONE_WORKSPACE_PATH = env("DRONE_WORKSPACE_PATH")(
      identity
    ) // custom env var according to https://docs.drone.io/pipeline/environment/reference/

    if (CI & DRONE) {
      DRONE_WORKSPACE_PATH orElse Some("/drone/src")
    } else None
  }

  // for running in parallel in drone
  private def open(resourcePath: String, attempt: Int = 0): Source = {
    Thread.sleep(attempt * 1000L)
    try {
      CI_WS_PATH
        .map(_ + "/modules/parser/src/test/resources" + resourcePath)
        .map(Source.fromFile)
        .getOrElse(Source.fromInputStream(clazz.getResourceAsStream(resourcePath)))
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
