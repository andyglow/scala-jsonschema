package com.github.andyglow.jsonschema

import org.scalatest.funsuite.AnyFunSuite

/** E2 repro: a schema for play-json's own AST types. Fails today (compile errors; Scala 2 crashes the macro on `JsObject`). */
class AnyValueReproSpec extends AnyFunSuite {

  test("JsValue") { assertCompiles("json.Json.schema[play.api.libs.json.JsValue]") }
  test("JsObject") { assertCompiles("json.Json.schema[play.api.libs.json.JsObject]") }
  test("JsArray") { assertCompiles("json.Json.schema[play.api.libs.json.JsArray]") }
  test("a JsValue field") {
    assertCompiles("""case class Payload(id: String, data: play.api.libs.json.JsValue); json.Json.schema[Payload]""")
  }
}
