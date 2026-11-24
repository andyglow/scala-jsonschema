package com.github.andyglow.jsonschema

import org.scalatest.funsuite.AnyFunSuite

/** E2 repro: a schema for json4s' own AST types. Fails today (compile errors). */
class AnyValueReproSpec extends AnyFunSuite {

  test("JValue") { assertCompiles("json.Json.schema[org.json4s.JValue]") }
  test("JObject") { assertCompiles("json.Json.schema[org.json4s.JObject]") }
  test("a JValue field") {
    assertCompiles("""case class Payload(id: String, data: org.json4s.JValue); json.Json.schema[Payload]""")
  }
}
