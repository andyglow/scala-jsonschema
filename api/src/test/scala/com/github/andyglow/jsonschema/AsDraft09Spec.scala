package com.github.andyglow.jsonschema

import com.github.andyglow.json.Value._
import com.github.andyglow.jsonschema.JsonMatchers._
import json.Schema._
import json.schema.Version._
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

class AsDraft09Spec extends AnyWordSpec {

  "AsValue.schema" should {

    "emit const" in {

      val a = `oneof`.of(
        `const`("foo").withTitle("Foo").withDescription("f-o-o"),
        `const`("bar").withTitle("Bar").withDescription("b-a-r")
      )

      val e = obj(
        f"$$schema" -> "https://json-schema.org/draft/2019-09/schema",
        f"$$id"     -> "http://example.com/foobarbaz.json",
        "oneOf" -> arr(
          obj("const" -> "foo", "title" -> "Foo", "description" -> "f-o-o"),
          obj("const" -> "bar", "title" -> "Bar", "description" -> "b-a-r")
        )
      )

      AsValue.schema(
        a,
        Draft09(id = "http://example.com/foobarbaz.json")
      ) should beStructurallyEqualTo(e)
    }

    "emit Object" in {
      import `object`.Field

      val a = `object`(
        Field("foo", `string`[String]),
        Field("uuid", `string`(`string`.Format.`uuid`)),
        Field("bar", `integer`, required = false),
        Field("baz", `def`[Boolean]("my-bool", `boolean`))
      )

      val e = obj(
        f"$$schema"            -> "https://json-schema.org/draft/2019-09/schema",
        f"$$id"                -> "http://example.com/foobarbaz.json",
        "type"                 -> "object",
        "additionalProperties" -> false,
        "required"             -> arr("foo", "baz", "uuid"),
        "properties" -> obj(
          "foo"  -> obj("type" -> "string"),
          "uuid" -> obj("type" -> "string", "format" -> "uuid"),
          "bar"  -> obj("type" -> "integer"),
          "baz"  -> obj(f"$$ref" -> "#my-bool")
        ),
        "$defs" -> obj("my-bool" -> obj(f"$$anchor" -> "my-bool", "type" -> "boolean"))
      )

      AsValue.schema(
        a,
        Draft09(id = "http://example.com/foobarbaz.json")
      ) should beStructurallyEqualTo(e)
    }
  }
}
