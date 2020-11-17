package com.github.andyglow.jsonschema

import com.github.andyglow.json.Value._
import com.github.andyglow.jsonschema.JsonMatchers._
import json.Schema._
import json.schema.Version._
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec


class AsDraft07Spec extends AnyWordSpec {

  "AsValue.schema" should {

    "emit Object" in {
      import `object`.Field

      val o = `object`(
        Field("foo", `string`[String]()),
        Field("bar", `integer`, required = false),
        Field("baz", `ref`[Boolean]("scala.Boolean", `boolean`("my-bool"))))

      AsValue.schema(o, Draft07(id = "http://example.com/foobarbaz.json")) should containJson(obj(
      f"$$schema" -> "http://json-schema.org/draft-07/schema#",
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
