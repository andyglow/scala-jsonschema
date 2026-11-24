package com.github.andyglow.jsonschema

import org.scalatest.funsuite.AnyFunSuite

/** E2 repro: a schema for ujson's own AST types. Fails today (compile errors). */
class AnyValueReproSpec extends AnyFunSuite {

  test("ujson.Value") { assertCompiles("json.Json.schema[ujson.Value]") }
  test("ujson.Obj") { assertCompiles("json.Json.schema[ujson.Obj]") }
  test("ujson.Arr") { assertCompiles("json.Json.schema[ujson.Arr]") }
  test("a ujson.Value field") {
    assertCompiles("""case class Payload(id: String, data: ujson.Value); json.Json.schema[Payload]""")
  }
}
