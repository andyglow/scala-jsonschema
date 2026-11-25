package com.github.andyglow.jsonschema

import json.Schema
import json.Schema.`object`.Field
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

object NullPredefSpec {
  case class Patch(name: ujson.Null.type)
}

/** #288: a schema for ujson's `Null`, opt-in with `import AsU.predef._`. */
class NullPredefSpec extends AnyFunSuite {
  import NullPredefSpec._

  test("ujson.Null is {\"type\": \"null\"} with AsU.predef") {
    import AsU.predef._
    json.Json.schema[ujson.Null.type] shouldBe Schema.`null`
    json.Json.schema[Patch] shouldBe Schema.`object`[Patch](Field("name", Schema.`null`))
  }
}
