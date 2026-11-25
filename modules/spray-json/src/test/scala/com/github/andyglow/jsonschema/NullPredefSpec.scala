package com.github.andyglow.jsonschema

import json.Schema
import json.Schema.`object`.Field
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._
import spray.json.JsNull

object NullPredefSpec {
  case class Patch(name: JsNull.type)
}

/** #288: a schema for spray-json's `JsNull`, opt-in with `import AsSpray.predef._`. */
class NullPredefSpec extends AnyFunSuite {
  import NullPredefSpec._

  test("JsNull is {\"type\": \"null\"} with AsSpray.predef") {
    import AsSpray.predef._
    json.Json.schema[JsNull.type] shouldBe Schema.`null`
    json.Json.schema[Patch] shouldBe Schema.`object`[Patch](Field("name", Schema.`null`))
  }
}
