package com.github.andyglow.jsonschema

import json.schema.Version.Draft07
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._
import petshop.JsonValidator

/** E2 repro: json4s' AST types compile, but derive as a family of json4s' case classes (`JString(s)` must be `{"s": ...}`), so the schema rejects the JSON json4s writes. */
class AnyValueReproSpec extends AnyFunSuite {

  private def accepts(schema: String, doc: String): Boolean = JsonValidator.errors(Draft07("x"), schema, doc).isEmpty

  test("JValue accepts a JSON string") { assert(accepts(json.Json.schema[org.json4s.JValue].draft07("x"), "\"abc\"")) }
  test("JObject accepts a JSON object") { assert(accepts(json.Json.schema[org.json4s.JObject].draft07("x"), """{"a": 1}""")) }
  test("a JValue field accepts a JSON number") {
    accepts(json.Json.schema[AnyValueReproSpec.Payload].draft07("x"), """{"id": "1", "data": 5}""") shouldBe true
  }
}

object AnyValueReproSpec {
  case class Payload(id: String, data: org.json4s.JValue)
}
