package com.github.andyglow.jsonschema

import com.github.andyglow.json.Value._
import json.Schema._
import json.schema.Version.Draft06
import matchers.should.Matchers._
import org.scalatest._
import JsonMatchers._
import org.scalatest.matchers
import org.scalatest.wordspec.AnyWordSpec

class AsDraft06Spec extends AnyWordSpec {

  "AsValue.schema" should {

    "emit Object" in {
      import `object`.Field

      val o = `object`(
        Field("foo", `string`[String](None, None)),
        Field("bar", `integer`, required = false),
        Field("baz", `ref`[Boolean]("scala.Boolean", `boolean`("my-bool"))))

      AsValue.schema(o, Draft06(id = "http://example.com/foobarbaz.json")) should containJson(obj(
      f"$$schema" -> "http://json-schema.org/draft-06/schema#",
      f"$$id" -> "http://example.com/foobarbaz.json",
      "type" -> "object",
      "additionalProperties" -> false,
      "required" -> arr("foo", "baz"),
      "properties" -> obj(
        "foo" -> obj("type" -> "string"),
        "bar" -> obj("type" -> "integer"),
        "baz" -> obj(f"$$ref" -> "#my-bool")),
      "definitions" -> obj(
        "my-bool" -> obj(
          f"$$id" -> "#my-bool",
          "type" -> "boolean"))))
    }
  }
}
