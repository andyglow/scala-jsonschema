package com.github.andyglow.jsonschema

import java.net.URI

import com.github.andyglow.json.Value._
import com.github.andyglow.jsonschema.JsonMatchers._
import json._
import json.Schema._
import json.Schema.`string`. { Format => F }
import json.schema.Version.Draft04
import json.schema.{ validation => V }, V.Instance._
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec


class AsDraft04Spec extends AnyWordSpec {
  import AsDraft04Spec._

  "AsValue.schema" should {

    "emit Object" in {
      import `object`.Field

      AsValue.schema(
        `object`(
          Field("foo" , `string`),
          Field("meta", `object`(Field("owner" , `string`)).free),
          Field("uuid", `string`(`string`.Format.`uuid`)),
          Field("bar" , `integer`, required = false),
          Field("baz" , `def`[Boolean]("my-bool", `boolean`))),
        Draft04()) should beStructurallyEqualTo(obj(
      f"$$schema" -> "http://json-schema.org/draft-04/schema#",
      "type" -> "object",
      "additionalProperties" -> false,
      "required" -> arr("foo", "meta", "baz", "uuid"),
      "properties" -> obj(
        "foo"  -> obj("type" -> "string"),
        "meta" -> obj("type" -> "object", "properties" -> obj("owner" -> obj("type" -> "string")), "required" -> arr("owner"), "additionalProperties" -> true),
        "uuid" -> obj("type" -> "string", "pattern" -> Constants.RegEx.uuid),
        "bar"  -> obj("type" -> "integer"),
        "baz"  -> obj(f"$$ref" -> "#/definitions/my-bool")),
      "definitions" -> obj(
        "my-bool" -> obj(
          "type" -> "boolean"))))
    }
  }

  "AsValue.apply" should {

    val asDraft04 = new AsDraft04(Draft04())
    
    "emit String" in {
      asDraft04(`string`[String]) shouldEqual obj("type" -> "string")
    }

    "emit String with Built in Format" in {

      asDraft04(`string`[URI](F.`uri`)) shouldEqual obj("type" -> "string", "format" -> "uri")

      asDraft04(`string`[java.util.Date](F.`date-time`)) shouldEqual obj("type" -> "string", "format" -> "date-time")

      asDraft04(`string`[java.sql.Date](F.`date`)) shouldEqual obj("type" -> "string", "format" -> "date")

      asDraft04(`string`[java.sql.Time](F.`time`)) shouldEqual obj("type" -> "string", "format" -> "time")

      asDraft04(`string`[String](F.`email`)) shouldEqual obj("type" -> "string", "format" -> "email")

      asDraft04(`string`[String](F.`hostname`)) shouldEqual obj("type" -> "string", "format" -> "hostname")

      asDraft04(`string`[String](F.`ipv4`)) shouldEqual obj("type" -> "string", "format" -> "ipv4")

      asDraft04(`string`[String](F.`ipv6`)) shouldEqual obj("type" -> "string", "format" -> "ipv6")
    }

    "emit String with Custom Format" in {

      case object `fancy-string-format` extends `string`.Format

      asDraft04(`string`[String](`fancy-string-format`)) shouldEqual obj("type" -> "string", "format" -> "fancy-string-format")
    }

    "emit Number" in {
      asDraft04(`number`[Double]) shouldEqual obj("type" -> "number")
    }

    "emit Integer" in {
      asDraft04(`boolean`) shouldEqual obj("type" -> "boolean")
    }

    "emit Boolean" in {
      asDraft04(`boolean`) shouldEqual obj("type" -> "boolean")
    }

    "emit Set" in {
      asDraft04(`array`[String, Set](`string`[String], unique = true)) shouldEqual obj(
        "type"        -> "array",
        "items"       -> obj("type" -> "string"),
        "uniqueItems" -> true)
    }

    "emit Array" in {

      // simple

      asDraft04(`array`(`string`[String])) shouldEqual obj("type" -> "array", "items" -> obj("type" -> "string"))

      asDraft04(`array`(`integer`)) shouldEqual obj("type" -> "array", "items" -> obj("type" -> "integer"))

      asDraft04(`array`(`number`[Int])) shouldEqual obj("type" -> "array", "items" -> obj("type" -> "number"))

      asDraft04(`array`(`boolean`)) shouldEqual obj("type" -> "array", "items" -> obj("type" -> "boolean"))

      // complex

      // array of formatted strings
      asDraft04(`array`(`string`(F.`email`))) shouldEqual obj("type" -> "array", "items" -> obj("type" -> "string", "format" -> "email"))

      // array of array
      asDraft04(`array`(`array`(`string`))) shouldEqual obj("type" -> "array", "items" -> obj("type" -> "array", "items" -> obj("type" -> "string")))
    }

    "emit Enum" in {
      asDraft04(`enum`.of("Red", "Blue", "Green")) shouldEqual obj(
        "type" -> "string",
        "enum" -> arr("Red", "Blue", "Green"))
    }

    "emit OneOf for classes" in {
      import `object`.Field

      asDraft04(`oneof`(Set(
        `object`(
          Field("foo", `string`),
          Field("bar", `integer`, required = false)),
        `object`(
          Field("foo", `string`))))) should containJson {
        obj(
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
    }

    "emit OneOf for sealed trait with value classes" in {

      asDraft04(`oneof`(Set(
        `string`[String],
        `integer`))
      ) shouldEqual obj(
        "oneOf" -> arr(
          obj("type" -> "string"),
          obj("type" -> "integer")))
    }

    "emit inner type for value class" in {
      // basic
      asDraft04(`value-class`(`integer`)) shouldEqual obj("type" -> "integer")

      // complex
      val value = asDraft04(`value-class`(`object`(
        `object`.Field("id", `integer`.withValidation(`minimum` := 20)),
        `object`.Field("name", `string`[String]))))

      value shouldEqual obj(
        "type" -> "object",
        "additionalProperties" -> false,
        "required" -> arr("id", "name"),
        "properties" -> obj(
          "id" -> obj("type" -> "integer", "minimum" -> 20),
          "name" -> obj("type" -> "string")))
    }

    "emit Map[String, _]" in {
      asDraft04(`dictionary`(`string`[String])) shouldEqual obj(
        "type" -> "object",
        "patternProperties" -> obj(
          "^.*$" -> obj(
            "type" -> "string")))
    }

    "emit Object" in {
      import `object`.Field

      asDraft04(`object`(
        Field("foo", `string`[String]),
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
        `def`[Boolean](
          "my-bool",
          `boolean`)) shouldEqual obj(s"$$ref" -> "#/definitions/my-bool")
    }

    "handle validations" when {

      "string" in {
        val schema1 = Json.schema[String].withValidation(
          `maxLength` := 20,
          `minLength` := 15,
          `pattern`   := "[a-z]+")

        asDraft04(schema1) shouldEqual obj("type" -> "string", "minLength" -> 15, "maxLength" -> 20, "pattern" -> "[a-z]+")
      }

      def numCase[T: Numeric](schema: json.Schema[T], t: String = "number")(implicit bound: V.Magnet[T, Number]): Unit = {
        asDraft04 {
          schema.withValidation(
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

        ()
      }

      "byte" in numCase(Json.schema[Byte])
      "short" in numCase(Json.schema[Short])
      "int" in numCase(Json.schema[Int], "integer")
      "long" in numCase(Json.schema[Long])
      "float" in numCase(Json.schema[Float])
      "double" in numCase(Json.schema[Double])
      "bigInt" in numCase(Json.schema[BigInt])
      "bigDec" in numCase(Json.schema[BigDecimal])

      def arrCase[T](schema: json.Schema[T])(implicit bound: V.Magnet[T, Iterable[_]]): Unit = {
        asDraft04 {
          schema.withValidation(
            `maxItems` := 20,
            `minItems` := 15)
        } should containJson(
          obj(
            "type" -> "array",
            "items" -> obj("type" -> "string"),
            "minItems" -> 15,
            "maxItems" -> 20))

        ()
      }

      "array" in arrCase[Array[String]](Json.schema[Array[String]])
      // "iterable" in arrCase(Json.schema[Iterable[String]])
      // "seq" in arrCase(Json.schema[Seq[String]])
      "list" in arrCase[List[String]](Json.schema[List[String]])
      "vector" in arrCase[Vector[String]](Json.schema[Vector[String]])
      "set" in arrCase[Set[String]](Json.schema[Set[String]])

      "map" in {
        asDraft04 {
          Json.schema[Map[String, String]].withValidation (
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
          implicit val vb = V.Magnet.mk[ValueClass, String]
          Json.schema[ValueClass].withValidation(
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