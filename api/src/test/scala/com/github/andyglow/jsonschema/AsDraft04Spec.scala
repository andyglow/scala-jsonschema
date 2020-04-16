package com.github.andyglow.jsonschema

import java.net.URI

import com.github.andyglow.json.Value._
import com.github.andyglow.jsonschema.JsonMatchers._
import json.Json
import json.Schema._
import json.schema.Version.Draft04
import org.scalatest.Matchers._
import org.scalatest._


class AsDraft04Spec extends WordSpec {
  import AsDraft04Spec._

  "AsValue.schema" should {

    "emit Object" in {
      import `object`.Field

      AsValue.schema(`object`(
        Field("foo", `string`[String](None, None)),
        Field("bar", `integer`, required = false),
        Field("baz", `ref`[Boolean]("scala.Boolean", `boolean`("my-bool")))
      )("foo.bar.baz.Obj"), Draft04()) shouldEqual obj(
      f"$$schema" -> "http://json-schema.org/draft-04/schema#",
      "type" -> "object",
      "additionalProperties" -> false,
      "required" -> arr("foo", "baz"),
      "properties" -> obj(
        "foo" -> obj("type" -> "string"),
        "bar" -> obj("type" -> "integer"),
        "baz" -> obj(f"$$ref" -> "#/definitions/my-bool")),
      "definitions" -> obj(
        "my-bool" -> obj(
          "type" -> "boolean")))
    }
  }

  "AsValue.apply" should {

    val asDraft04 = new AsDraft04(Draft04())
    
    "emit String" in {
      asDraft04(`string`[String](None, None)) shouldEqual obj("type" -> "string")
    }

    "emit String with Built in Format" in {

      asDraft04(`string`[URI](Some(`string`.Format.`uri`), None)) shouldEqual obj("type" -> "string", "format" -> "uri")

      asDraft04(`string`[java.util.Date](Some(`string`.Format.`date-time`), None)) shouldEqual obj("type" -> "string", "format" -> "date-time")

      asDraft04(`string`[java.sql.Date](Some(`string`.Format.`date`), None)) shouldEqual obj("type" -> "string", "format" -> "date")

      asDraft04(`string`[java.sql.Time](Some(`string`.Format.`time`), None)) shouldEqual obj("type" -> "string", "format" -> "time")

      asDraft04(`string`[String](Some(`string`.Format.`email`), None)) shouldEqual obj("type" -> "string", "format" -> "email")

      asDraft04(`string`[String](Some(`string`.Format.`hostname`), None)) shouldEqual obj("type" -> "string", "format" -> "hostname")

      asDraft04(`string`[String](Some(`string`.Format.`ipv4`), None)) shouldEqual obj("type" -> "string", "format" -> "ipv4")

      asDraft04(`string`[String](Some(`string`.Format.`ipv6`), None)) shouldEqual obj("type" -> "string", "format" -> "ipv6")
    }

    "emit String with Custom Format" in {

      case object `fancy-string-format` extends `string`.Format

      asDraft04(`string`[String](Some(`fancy-string-format`), None)) shouldEqual obj("type" -> "string", "format" -> "fancy-string-format")
    }

    "emit Number" in {
      asDraft04(`number`[Double]()) shouldEqual obj("type" -> "number")
    }

    "emit Integer" in {
      asDraft04(`boolean`) shouldEqual obj("type" -> "boolean")
    }

    "emit Boolean" in {
      asDraft04(`boolean`) shouldEqual obj("type" -> "boolean")
    }

    "emit Set" in {
      asDraft04(`set`[String, Set](`string`[String](None, None))) shouldEqual obj(
        "type"        -> "array",
        "items"       -> obj("type" -> "string"),
        "uniqueItems" -> true)
    }

    "emit Array" in {

      // simple

      asDraft04(`array`(`string`[String](None, None))) shouldEqual obj("type" -> "array", "items" -> obj("type" -> "string"))

      asDraft04(`array`(`integer`)) shouldEqual obj("type" -> "array", "items" -> obj("type" -> "integer"))

      asDraft04(`array`(`number`[Int]())) shouldEqual obj("type" -> "array", "items" -> obj("type" -> "number"))

      asDraft04(`array`(`boolean`)) shouldEqual obj("type" -> "array", "items" -> obj("type" -> "boolean"))

      // complex

      // array of formatted strings
      asDraft04(`array`(`string`[String](Some(`string`.Format.`email`), None))) shouldEqual obj("type" -> "array", "items" -> obj("type" -> "string", "format" -> "email"))

      // array of array
      asDraft04(`array`(`array`(`string`[String](None, None)))) shouldEqual obj("type" -> "array", "items" -> obj("type" -> "array", "items" -> obj("type" -> "string")))
    }

    "emit Enum" in {
      asDraft04(`enum`(Set("Red", "Blue", "Green"))) shouldEqual obj(
        "type" -> "string",
        "enum" -> arr("Red", "Blue", "Green"))
    }

    "emit OneOf for classes" in {
      import `object`.Field

      asDraft04(`oneof`(Set(
        `object`(
          Field("foo", `string`[String](None, None)),
          Field("bar", `integer`, required = false)),
        `object`(
          Field("foo", `string`[String](None, None)))))) shouldEqual obj(
        "type" -> "object",
        "oneOf" -> arr(
          obj(
            "additionalProperties" -> false,
            "required" -> arr("foo"),
            "properties" -> obj(
              "foo" -> obj("type" -> "string"),
              "bar" -> obj("type" -> "integer"))),
          obj(
            "additionalProperties" -> false,
            "required" -> arr("foo"),
            "properties" -> obj(
              "foo" -> obj("type" -> "string")))))
    }

    "emit OneOf for value classes" in {

      asDraft04(`oneof`(Set(
        `string`[String](None, None),
        `integer`))
      ) shouldEqual obj(
        "oneOf" -> arr(
          obj("type" -> "string"),
          obj("type" -> "integer")))
    }

    "emit Map[String, _]" in {
      asDraft04(`string-map`(`string`[String](None, None))) shouldEqual obj(
        "type" -> "object",
        "patternProperties" -> obj(
          "^.*$" -> obj(
            "type" -> "string")))
    }

    "emit Map[Int, _]" in {
      asDraft04(`int-map`(`string`[String](None, None))) shouldEqual obj(
        "type" -> "object",
        "patternProperties" -> obj(
          "^[0-9]*$" -> obj(
            "type" -> "string")))
    }

    "emit Object" in {
      import `object`.Field

      asDraft04(`object`(
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
      asDraft04(
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
          `pattern`   := "[a-z]+")

        asDraft04(schema1) shouldEqual obj("type" -> "string", "minLength" -> 15, "maxLength" -> 20, "pattern" -> "[a-z]+")
      }

      def numCase[T: Numeric](schema: json.Schema[T], t: String = "number")(implicit bound: ValidationBound[T, Number]): Unit = {
        asDraft04 {
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
        asDraft04 {
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

      "array" in arrCase[Array[String]](Json.schema[Array[String]])
      // "iterable" in arrCase(Json.schema[Iterable[String]])
      // "seq" in arrCase(Json.schema[Seq[String]])
      "list" in arrCase[List[String]](Json.schema[List[String]])
      "vector" in arrCase[Vector[String]](Json.schema[Vector[String]])
      "set" in arrCase[Set[String]](Json.schema[Set[String]])

      "map" in {
        asDraft04 {
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

      "value class" in {
        asDraft04 {
          implicit val vb = ValidationBound.mk[ValueClass, String]
          Json.schema[ValueClass] withValidation (
            `pattern` := "^[a-z]*$")
        } shouldBe obj(
          "type" -> "string",
          "pattern" -> "^[a-z]*$")
      }
    }
  }
}

object AsDraft04Spec {

  case class ValueClass(value: String) extends AnyVal
}