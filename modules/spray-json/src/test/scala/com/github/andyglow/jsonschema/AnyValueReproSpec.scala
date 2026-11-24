package com.github.andyglow.jsonschema

import json.schema.Version.Draft07
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._
import petshop.JsonValidator

/** E2 repro: spray-json's AST types compile, but derive as a family of spray's case classes (`JsObject(fields)` must be `{"fields": ...}`), so the schema rejects the JSON spray writes. */
class AnyValueReproSpec extends AnyFunSuite {

  private def accepts(schema: String, doc: String): Boolean = JsonValidator.errors(Draft07("x"), schema, doc).isEmpty

  test("JsValue accepts a JSON string") { assert(accepts(json.Json.schema[spray.json.JsValue].draft07("x"), "\"abc\"")) }
  test("JsObject accepts a JSON object") { assert(accepts(json.Json.schema[spray.json.JsObject].draft07("x"), """{"a": 1}""")) }
  test("a JsValue field accepts a JSON number") {
    accepts(json.Json.schema[AnyValueReproSpec.Payload].draft07("x"), """{"id": "1", "data": 5}""") shouldBe true
  }
}

object AnyValueReproSpec {
  case class Payload(id: String, data: spray.json.JsValue)
}
