package com.github.andyglow.jsonschema

import com.github.andyglow.json.Value._
import com.github.andyglow.jsonschema.JsonMatchers._
import json.Schema._
import json.schema.Version._
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

class AsDraft07Spec extends AnyWordSpec {
  import AsDraft07Spec._

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

      val o = `object`(
        Field("foo", `string`[String]),
        Field("bar", `integer`, required = false),
        Field("baz", `def`[Boolean]("my-bool", `boolean`)),
        Field(
          "mapObject",
          `dictionary`[String, FooX, Map](`def`[FooX]("FooX", `object`(Field("a", `integer`))))
        ),
        Field(
          "listObject",
          `array`[FooY, List](`def`[FooY]("FooY", `object`(Field("b", `integer`))))
        )
      )

      val expected = obj(
        f"$$schema"            -> "http://json-schema.org/draft-07/schema#",
        f"$$id"                -> "http://example.com/foobarbaz.json",
        "type"                 -> "object",
        "additionalProperties" -> false,
        "required"             -> arr("listObject", "baz", "mapObject", "foo"),
        "properties" -> obj(
          "foo" -> obj("type" -> "string"),
          "bar" -> obj("type" -> "integer"),
          "baz" -> obj(f"$$ref" -> "#my-bool"),
          "mapObject" -> obj(
            "type"              -> "object",
            "patternProperties" -> obj("^.*$" -> obj(f"$$ref" -> "#FooX"))
          ),
          "listObject" -> obj(
            "type"  -> "array",
            "items" -> obj(f"$$ref" -> "#FooY")
          )
        ),
        "definitions" -> obj(
          "my-bool" -> obj(f"$$id" -> "#my-bool", "type" -> "boolean"),
          "FooX" -> obj(
            f"$$id"                -> "#FooX",
            "type"                 -> "object",
            "additionalProperties" -> false,
            "properties"           -> obj("a" -> obj("type" -> "integer")),
            "required"             -> arr("a")
          ),
          "FooY" -> obj(
            f"$$id"                -> "#FooY",
            "type"                 -> "object",
            "additionalProperties" -> false,
            "properties"           -> obj("b" -> obj("type" -> "integer")),
            "required"             -> arr("b")
          )
        )
      )

      AsValue.schema(
        o,
        Draft07(id = "http://example.com/foobarbaz.json")
      ) should beStructurallyEqualTo(expected)
    }
  }
}

object AsDraft07Spec {
  case class FooX(a: Int)
  case class FooY(b: Int)
}
