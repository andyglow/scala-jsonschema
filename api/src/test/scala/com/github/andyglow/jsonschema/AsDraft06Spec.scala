package com.github.andyglow.jsonschema

import com.github.andyglow.json.Value._
import json.Schema._
import json.schema.Version.{Draft06, Draft09}
import JsonMatchers._
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

class AsDraft06Spec extends AnyWordSpec {

  "AsValue.schema" should {

    "emit const" in {

      val a = `oneof`.of(
        `const`("foo").withTitle("Foo").withDescription("f-o-o"),
        `const`("bar").withTitle("Bar").withDescription("b-a-r")
      )

      val e = obj(
        f"$$schema" -> "https://json-schema.org/draft/2019-09/schema",
        f"$$id" -> "http://example.com/foobarbaz.json",
        "oneOf" -> arr(
          obj("const" -> "foo", "title" -> "Foo", "description" -> "f-o-o"),
          obj("const" -> "bar", "title" -> "Bar", "description" -> "b-a-r")
        )
      )

      AsValue.schema(a, Draft09(id = "http://example.com/foobarbaz.json")) should beStructurallyEqualTo(e)
    }

    "emit Object" in {
      import `object`.Field

      val o = `object`(
        Field("foo", `string`[String]),
        Field("bar", `integer`, required = false),
        Field("baz", `def`[Boolean]("my-bool", `boolean`)))

      AsValue.schema(o, Draft06(id = "http://example.com/foobarbaz.json")) should beStructurallyEqualTo(obj(
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
