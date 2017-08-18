package json

import json.Type._
import json.Value._
import org.scalatest._
import org.scalatest.Matchers._

class AsJsonSpec extends WordSpec {

  "Schema.asJson" should {

    "emit String" in {
      Schema(`string`()).asJson shouldEqual obj("type" -> "string")
    }

    "emit String with Built in Format" in {

      Schema(`string`(`string`.Format.`uri`)).asJson shouldEqual obj("type" -> "string", "format" -> "uri")

      Schema(`string`(`string`.Format.`date-time`)).asJson shouldEqual obj("type" -> "string", "format" -> "date-time")

      Schema(`string`(`string`.Format.`date`)).asJson shouldEqual obj("type" -> "string", "format" -> "date")

      Schema(`string`(`string`.Format.`time`)).asJson shouldEqual obj("type" -> "string", "format" -> "time")

      Schema(`string`(`string`.Format.`email`)).asJson shouldEqual obj("type" -> "string", "format" -> "email")

      Schema(`string`(`string`.Format.`hostname`)).asJson shouldEqual obj("type" -> "string", "format" -> "hostname")

      Schema(`string`(`string`.Format.`ipv4`)).asJson shouldEqual obj("type" -> "string", "format" -> "ipv4")

      Schema(`string`(`string`.Format.`ipv6`)).asJson shouldEqual obj("type" -> "string", "format" -> "ipv6")
    }


    "emit String with Custom Format" in {

      case object `fancy-string-format` extends `string`.Format

      Schema(`string`(`fancy-string-format`)).asJson shouldEqual obj("type" -> "string", "format" -> "fancy-string-format")
    }

    "emit Number" in {
      Schema(`number`).asJson shouldEqual obj("type" -> "number")
    }

    "emit Integer" in {
      Schema(`boolean`).asJson shouldEqual obj("type" -> "boolean")
    }

    "emit Boolean" in {
      Schema(`boolean`).asJson shouldEqual obj("type" -> "boolean")
    }

    "emit Array" in {

      // simple

      Schema(`array`(Schema(`string`()))).asJson shouldEqual obj("type" -> "array", "items" -> obj("type" -> "string"))

      Schema(`array`(Schema(`integer`))).asJson shouldEqual obj("type" -> "array", "items" -> obj("type" -> "integer"))

      Schema(`array`(Schema(`number`))).asJson shouldEqual obj("type" -> "array", "items" -> obj("type" -> "number"))

      Schema(`array`(Schema(`boolean`))).asJson shouldEqual obj("type" -> "array", "items" -> obj("type" -> "boolean"))

      // complex

      // array of formatted strings
      Schema(`array`(Schema(`string`(`string`.Format.`email`)))).asJson shouldEqual obj("type" -> "array", "items" -> obj("type" -> "string", "format" -> "email"))

      // array of array
      Schema(`array`(Schema(`array`(Schema(`string`()))))).asJson shouldEqual obj("type" -> "array", "items" -> obj("type" -> "array", "items" -> obj("type" -> "string")))
    }

    "emit Object" in {
      import `object`.Field

      Schema(`object`(
        Field("foo", Schema(`string`())),
        Field("bar", Schema(`integer`), required = false)
      )).asJson shouldEqual obj(
        "type" -> "object",
        "required" -> arr("foo"),
        "properties" -> obj(
          "foo" -> obj("type" -> "string"),
          "bar" -> obj("type" -> "integer")
        ))
    }
  }
}
