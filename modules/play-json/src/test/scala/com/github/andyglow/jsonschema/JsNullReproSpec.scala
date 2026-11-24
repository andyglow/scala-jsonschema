package com.github.andyglow.jsonschema

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

object JsNullReproSpec {
  case class Patch(name: play.api.libs.json.JsNull.type)
}

class JsNullReproSpec extends AnyFunSuite {
  import JsNullReproSpec._
  test("repro: JsNull without a Predef") { assertCompiles("json.Json.schema[Patch]") }
  test("repro: JsNull with the documented Predef") {
    implicit val jsNull: json.schema.Predef[play.api.libs.json.JsNull.type] = json.schema.Predef(json.Schema.`null`)
    json.Json.schema[Patch].stringify(json.schema.Version.Draft12("x")) should include(""""type":"null"""")
  }
}
