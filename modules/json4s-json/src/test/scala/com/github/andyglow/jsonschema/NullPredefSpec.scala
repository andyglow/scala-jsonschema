package com.github.andyglow.jsonschema

import json.Schema
import json.Schema.`object`.Field
import org.json4s.JsonAST.JNull
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

object NullPredefSpec {
  case class Patch(name: JNull.type)
}

/** #288: a schema for json4s' `JNull`, opt-in with `import AsJson4s.predef._`. */
class NullPredefSpec extends AnyFunSuite {
  import NullPredefSpec._

  test("JNull is {\"type\": \"null\"} with AsJson4s.predef") {
    import AsJson4s.predef._
    json.Json.schema[JNull.type] shouldBe Schema.`null`
    json.Json.schema[Patch] shouldBe Schema.`object`[Patch](Field("name", Schema.`null`))
  }
}
