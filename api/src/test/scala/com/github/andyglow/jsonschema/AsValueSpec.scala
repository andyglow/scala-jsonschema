package com.github.andyglow.jsonschema

import java.net.URI

import com.github.andyglow.json.Value._
import json.Json
import json.Schema._
import org.scalatest.Matchers._
import org.scalatest._
import JsonMatchers._


class AsValueSpec extends WordSpec {

  "Schema.asJson" should {

    "emit String" in {
      AsValue(`string`[String](None, None)) shouldEqual obj("type" -> "string")
    }

    "emit String with Built in Format" in {

      AsValue(`string`[URI](Some(`string`.Format.`uri`), None)) shouldEqual obj("type" -> "string", "format" -> "uri")

      AsValue(`string`[java.util.Date](Some(`string`.Format.`date-time`), None)) shouldEqual obj("type" -> "string", "format" -> "date-time")

      AsValue(`string`[java.sql.Date](Some(`string`.Format.`date`), None)) shouldEqual obj("type" -> "string", "format" -> "date")

      AsValue(`string`[java.sql.Time](Some(`string`.Format.`time`), None)) shouldEqual obj("type" -> "string", "format" -> "time")

      AsValue(`string`[String](Some(`string`.Format.`email`), None)) shouldEqual obj("type" -> "string", "format" -> "email")

      AsValue(`string`[String](Some(`string`.Format.`hostname`), None)) shouldEqual obj("type" -> "string", "format" -> "hostname")

      AsValue(`string`[String](Some(`string`.Format.`ipv4`), None)) shouldEqual obj("type" -> "string", "format" -> "ipv4")

      AsValue(`string`[String](Some(`string`.Format.`ipv6`), None)) shouldEqual obj("type" -> "string", "format" -> "ipv6")
    }

    "emit String with Custom Format" in {

      case object `fancy-string-format` extends `string`.Format

      AsValue(`string`[String](Some(`fancy-string-format`), None)) shouldEqual obj("type" -> "string", "format" -> "fancy-string-format")
    }

    "emit Number" in {
      AsValue(`number`[Double]()) shouldEqual obj("type" -> "number")
    }

    "emit Integer" in {
      AsValue(`boolean`) shouldEqual obj("type" -> "boolean")
    }

    "emit Boolean" in {
      AsValue(`boolean`) shouldEqual obj("type" -> "boolean")
    }

    "emit Set" in {
      AsValue(`set`(`string`[String](None, None))) shouldEqual obj(
        "type"        -> "array",
        "items"       -> obj("type" -> "string"),
        "uniqueItems" -> true)
    }

    "emit Array" in {

      // simple

      AsValue(`array`(`string`[String](None, None))) shouldEqual obj("type" -> "array", "items" -> obj("type" -> "string"))

      AsValue(`array`(`integer`)) shouldEqual obj("type" -> "array", "items" -> obj("type" -> "integer"))

      AsValue(`array`(`number`[Int]())) shouldEqual obj("type" -> "array", "items" -> obj("type" -> "number"))

      AsValue(`array`(`boolean`)) shouldEqual obj("type" -> "array", "items" -> obj("type" -> "boolean"))

      // complex

      // array of formatted strings
      AsValue(`array`(`string`[String](Some(`string`.Format.`email`), None))) shouldEqual obj("type" -> "array", "items" -> obj("type" -> "string", "format" -> "email"))

      // array of array
      AsValue(`array`(`array`(`string`[String](None, None)))) shouldEqual obj("type" -> "array", "items" -> obj("type" -> "array", "items" -> obj("type" -> "string")))
    }

    "emit Enum" in {
      AsValue(`enum`(Set("Red", "Blue", "Green"))) shouldEqual obj(
        "type" -> "string",
        "enum" -> arr("Red", "Blue", "Green"))
    }

    "emit OneOf for classes" in {
      import `object`.Field

      AsValue(`oneof`(Set(
        `object`(
          Field("foo", `string`[String](None, None)),
          Field("bar", `integer`, required = false)
        ),
        `object`(
          Field("foo", `string`[String](None, None))
        )
      ))) shouldEqual obj(
        "oneOf" -> arr(
          obj(
            "type" -> "object",
            "additionalProperties" -> false,
            "required" -> arr("foo"),
            "properties" -> obj(
              "foo" -> obj("type" -> "string"),
              "bar" -> obj("type" -> "integer"))),
          obj(
            "type" -> "object",
            "additionalProperties" -> false,
            "required" -> arr("foo"),
            "properties" -> obj(
              "foo" -> obj("type" -> "string")))))
    }

    "emit OneOf for value classes" in {

      AsValue(`oneof`(Set(
        `string`[String](None, None),
        `integer`))
      ) shouldEqual obj(
        "oneOf" -> arr(
          obj("type" -> "string"),
          obj("type" -> "integer")))
    }

    "emit Map[String, _]" in {
      AsValue(`string-map`(`string`[String](None, None))) shouldEqual obj(
        "type" -> "object",
        "patternProperties" -> obj(
          "^.*$" -> obj(
            "type" -> "string")))
    }

    "emit Map[Int, _]" in {
      AsValue(`int-map`(`string`[String](None, None))) shouldEqual obj(
        "type" -> "object",
        "patternProperties" -> obj(
          "^[0-9]*$" -> obj(
            "type" -> "string")))
    }

    "emit Object" in {
      import `object`.Field

      AsValue(`object`(
        Field("foo", `string`[String](None, None)),
        Field("bar", `integer`, required = false),
        Field("baz", `boolean`, required = false)
      )) shouldEqual obj(
        "type" -> "object",
        "additionalProperties" -> false,
        "required" -> arr("foo"),
        "properties" -> obj(
          "foo" -> obj("type" -> "string"),
          "bar" -> obj("type" -> "integer"),
          "baz" -> obj("type" -> "boolean")
        ))
    }

    "consider Ref if defined" in {
      AsValue(
        `ref`[Boolean](
          "scala.Boolean",
          `boolean`("my-bool"))) shouldEqual obj(s"$$ref" -> "#/definitions/my-bool")
    }

    "handle validations" when {
      import json.Validation._

      "string" in {
        val schema1 = Json.schema[String] withValidation (
          `maxLength` := 20,
          `minLength` := 15,
          `pattern` := "[a-z]+")

        AsValue(schema1) shouldEqual obj("type" -> "string", "minLength" -> 15, "maxLength" -> 20, "pattern" -> "[a-z]+")
      }

      def numCase[T: Numeric](schema: json.Schema[T], t: String = "number")(implicit bound: ValidationBound[T, Number]): Unit = {
        AsValue {
          schema withValidation (
            `maximum` := 20,
            `minimum` := 15,
            `exclusiveMinimum` := 3,
            `exclusiveMaximum` := 18,
            `multipleOf` := 3)
        } shouldEqual obj(
          "type" -> t,
          "minimum" -> 15,
          "maximum" -> 20,
          "exclusiveMinimum" -> 3,
          "exclusiveMaximum" -> 18,
          "multipleOf" -> 3)
      }

      "byte" in numCase(Json.schema[Byte])
      "short" in numCase(Json.schema[Short])
      "int" in numCase(Json.schema[Int], "integer")
      "long" in numCase(Json.schema[Long])
      "float" in numCase(Json.schema[Float])
      "double" in numCase(Json.schema[Double])
      "bigInt" in numCase(Json.schema[BigInt])
      "bigDec" in numCase(Json.schema[BigDecimal])

      def arrCase[T](schema: json.Schema[T])(implicit bound: ValidationBound[T, Iterable[_]]): Unit = {
        AsValue {
          schema withValidation (
            `maxItems` := 20,
            `minItems` := 15)
        } should containJson(
          obj(
            "type" -> "array",
            "items" -> obj("type" -> "string"),
            "minItems" -> 15,
            "maxItems" -> 20))
      }

      "array" in arrCase(Json.schema[Array[String]])
      "iterable" in arrCase(Json.schema[Iterable[String]])
      "seq" in arrCase(Json.schema[Seq[String]])
      "list" in arrCase(Json.schema[List[String]])
      "vector" in arrCase(Json.schema[Vector[String]])
      "set" in arrCase(Json.schema[Set[String]])

      "map" in {
        AsValue {
          Json.schema[Map[String, String]] withValidation (
            `patternProperties` := "^[a-z]*$",
            `maxProperties`     := 10,
            `minProperties`     := 4)
        } shouldBe obj(
          "type" -> "object",
          "maxProperties" -> 10,
          "minProperties" -> 4,
          "patternProperties" -> obj(
            "^[a-z]*$" -> obj("type" -> "string"))
        )
      }
    }
  }
}
