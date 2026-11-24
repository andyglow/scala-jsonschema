package com.github.andyglow.jsonschema

import org.scalatest.funsuite.AnyFunSuite

/** E2 repro: an "any JSON value" schema. There is no such node (`json.Schema.any`), and our own AST has no schema. Fails today (compile errors). */
class AnyValueReproSpec extends AnyFunSuite {

  test("json.Schema has an any-value node") { assertCompiles("json.Schema.`any`[String]()") }
  test("com.github.andyglow.json.Value") { assertCompiles("json.Json.schema[com.github.andyglow.json.Value]") }
  test("a Value field") {
    assertCompiles("""case class Payload(id: String, data: com.github.andyglow.json.Value); json.Json.schema[Payload]""")
  }
}
