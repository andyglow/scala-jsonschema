package com.github.andyglow.jsonschema

import com.github.andyglow.json.{ParseJson, Value}
import json.Schema
import com.github.andyglow.testsupport._
import com.github.andyglow.scalamigration._
import scala.util.Try
import org.scalatest.matchers.should.Matchers._
import org.scalatest.funsuite.AnyFunSuite

class ParseJsonSchemaSpec extends AnyFunSuite {
  import Schema._

  test("string") {
    parseType {
      """{
        | "type": "string"
        |}
      """.stripMargin
    }.value shouldBe `string`
  }

  test("title") {
    parseType {
      """{
        | "type": "string",
        | "title": "testing title"
        |}
      """.stripMargin
    }.value.title shouldBe Some("testing title")
  }

  test("string: date") {
    import `string`._
    import Format._

    parseType {
      """{
        | "type": "string",
        | "format": "date"
        |}
      """.stripMargin
    }.value shouldBe `string`(`date`)
  }

  test("string: time") {
    import `string`._
    import Format._

    parseType {
      """{
        | "type": "string",
        | "format": "time"
        |}
      """.stripMargin
    }.value shouldBe `string`(`time`)
  }

  test("string: date-time") {
    import `string`._
    import Format._

    parseType {
      """{
        | "type": "string",
        | "format": "date-time"
        |}
      """.stripMargin
    }.value shouldBe `string`(`date-time`)
  }

  test("string: uri") {
    import `string`._
    import Format._

    parseType {
      """{
        | "type": "string",
        | "format": "uri"
        |}
      """.stripMargin
    }.value shouldBe `string`(`uri`)
  }

  test("string: email") {
    import `string`._
    import Format._

    parseType {
      """{
        | "type": "string",
        | "format": "email"
        |}
      """.stripMargin
    }.value shouldBe `string`(`email`)
  }

  test("string: hostname") {
    import `string`._
    import Format._

    parseType {
      """{
        | "type": "string",
        | "format": "hostname"
        |}
      """.stripMargin
    }.value shouldBe `string`(`hostname`)
  }

  test("string: ipv4") {
    import `string`._
    import Format._

    parseType {
      """{
        | "type": "string",
        | "format": "ipv4"
        |}
      """.stripMargin
    }.value shouldBe `string`(`ipv4`)
  }

  test("string: ipv6") {
    import `string`._
    import Format._

    parseType {
      """{
        | "type": "string",
        | "format": "ipv6"
        |}
      """.stripMargin
    }.value shouldBe `string`(`ipv6`)
  }

  test("integer") {
    parseType {
      """{
        | "type": "integer"
        |}
      """.stripMargin
    }.value shouldBe `integer`
  }

  test("number") {
    parseType {
      """{
        | "type": "number"
        |}
      """.stripMargin
    }.value shouldBe `number`[Int]
  }

  test("boolean") {
    parseType {
      """{
        | "type": "boolean"
        |}
      """.stripMargin
    }.value shouldBe `boolean`
  }

  test("array") {
    parseType {
      """{
        | "type": "array",
        | "items": {
        |   "type": "string"
        | }
        |}
      """.stripMargin
    }.value shouldBe `array`(`string`)
  }

  test("object: properties keep their order, required is respected") {
    parseType {
      """{
        | "type": "object",
        | "properties": {
        |   "b": { "type": "string" },
        |   "a": { "type": "integer" },
        |   "c": { "type": "boolean" }
        | },
        | "required": ["a"]
        |}
      """.stripMargin
    }.value match {
      case o: `object`[_] => o.fields.map(f => (f.name, f.required)) shouldBe List("b" -> false, "a" -> true, "c" -> false)
      case other          => fail(s"expected object, got $other")
    }
  }

  test("object: invalid property schema is reported, not thrown") {
    parseType {
      """{
        | "type": "object",
        | "properties": { "a": { "title": "no type" } }
        |}
      """.stripMargin
    }.isFailure shouldBe true
  }

  test("unsupported type is a failure") {
    parseType("""{ "type": "foo" }""").isFailure shouldBe true
    parseType("""{ "type": ["string", "integer"] }""").isFailure shouldBe true
    parseType("""{ "type": ["null", "null"] }""").isFailure shouldBe true
  }

  test("null, and X or null (how an Option renders)") {
    parseType("""{ "type": "null" }""").value shouldBe Schema.`null`
    parseType("""{ "type": ["string", "null"] }""").value shouldBe `oneof`.of(`string`, Schema.`null`)
    parseType("""{ "type": ["null", "integer"] }""").value shouldBe `oneof`.of(`integer`, Schema.`null`)
    parseType {
      """{
        | "type": "object",
        | "properties": { "a": { "type": ["array", "null"], "items": { "type": "string" } } }
        |}
      """.stripMargin
    }.value shouldBe `object`(`object`.Field("a", `oneof`.of(`array`[String, List](`string`), Schema.`null`), required = false))
  }

  test("apply: accepts a draft-04 schema") {
    ParseJsonSchema {
      """{
        | "$schema": "http://json-schema.org/draft-04/schema#",
        | "type": "string"
        |}
      """.stripMargin
    }.value shouldBe `string`
  }

  test("apply: accepts the draft-04 uri without fragment") {
    ParseJsonSchema("""{ "$schema": "http://json-schema.org/draft-04/schema", "type": "string" }""").value shouldBe `string`
  }

  test("apply: rejects input without $schema unless the check is disabled") {
    ParseJsonSchema("""{ "type": "string" }""").isFailure shouldBe true
    ParseJson("""{ "type": "string" }""").flatMap(ParseJsonSchema(_, checkSchema = false)).value shouldBe `string`
  }

  test("apply: rejects invalid json") {
    ParseJsonSchema("""{ "type": "string", }""").isFailure shouldBe true
  }

  test("enum: values keep the order of the input array") {
    val parsed = parseType("""{ "type": "string", "enum": ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"] }""").value
    parsed match {
      case e: `enum`[_] => e.values.toList shouldBe List("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun").map(Value.str(_))
      case other        => fail(other.toString)
    }
  }

  test("patternProperties: parsed back and rendered, ^.*$ (any key, before #466) isn't enforced, another pattern is") {
    def rendered(pattern: String): Value.obj = {
      val s = parseType(s"""{ "type": "object", "patternProperties": { "$pattern": { "type": "string" } } }""").value
      s shouldBe `dictionary`[Any, Any, Map](`string`).withValidation(json.schema.validation.Instance.`patternProperties` := pattern)
      AsValue.schema(s, json.schema.Version.Draft07("x"))
    }
    val any = rendered("^.*$").value
    any.contains("propertyNames") shouldBe false
    any.contains("additionalProperties") shouldBe false
    rendered("^-?[0-9]+$").value("propertyNames") shouldBe Value.obj("pattern" -> "^-?[0-9]+$")
  }

  def parseType(x: String): Try[Schema[_]] =
    ParseJson(x) find { case e: Value.obj => e } flatMap ParseJsonSchema.makeType
}
