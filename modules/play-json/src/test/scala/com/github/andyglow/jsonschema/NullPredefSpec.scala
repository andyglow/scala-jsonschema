package com.github.andyglow.jsonschema

import json.Schema
import json.Schema.`object`.Field
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._
import play.api.libs.json.JsNull

object NullPredefSpec {
  case class Patch(name: JsNull.type)
}

/** #288: a schema for play-json's `JsNull`, opt-in with `import AsPlay.predef._`. */
class NullPredefSpec extends AnyFunSuite {
  import NullPredefSpec._

  test("JsNull is {\"type\": \"null\"} with AsPlay.predef") {
    import AsPlay.predef._
    json.Json.schema[JsNull.type] shouldBe Schema.`null`
    json.Json.schema[Patch] shouldBe Schema.`object`[Patch](Field("name", Schema.`null`))
  }

  test("a user's own Predef[JsNull.type] works without the import") {
    implicit val own: json.schema.Predef[JsNull.type] = json.schema.Predef(Schema.`string`[JsNull.type])
    json.Json.schema[Patch] shouldBe Schema.`object`[Patch](Field("name", Schema.`string`[JsNull.type]))
  }
}
