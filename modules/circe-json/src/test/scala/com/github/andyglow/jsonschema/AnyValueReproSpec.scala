package com.github.andyglow.jsonschema

import org.scalatest.funsuite.AnyFunSuite

/** E2 repro: a schema for circe's own AST types. Fails today (compile errors). */
class AnyValueReproSpec extends AnyFunSuite {

  test("Json") { assertCompiles("json.Json.schema[io.circe.Json]") }
  test("JsonObject") { assertCompiles("json.Json.schema[io.circe.JsonObject]") }
  test("a Json field") {
    assertCompiles("""case class Payload(id: String, data: io.circe.Json); json.Json.schema[Payload]""")
  }
}
