package com.github.andyglow.jsonschema

import org.scalatest.funsuite.AnyFunSuite

/** E2 repro: a schema for spray-json's own AST types. Fails today (compile errors). */
class AnyValueReproSpec extends AnyFunSuite {

  test("JsValue") { assertCompiles("json.Json.schema[spray.json.JsValue]") }
  test("JsObject") { assertCompiles("json.Json.schema[spray.json.JsObject]") }
  test("a JsValue field") {
    assertCompiles("""case class Payload(id: String, data: spray.json.JsValue); json.Json.schema[Payload]""")
  }
}
