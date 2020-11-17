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
    }.value shouldBe `string`()
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
        | "type": "integer",
        |}
      """.stripMargin
    }.value shouldBe `integer`
  }

  test("number") {
    parseType {
      """{
        | "type": "number",
        |}
      """.stripMargin
    }.value shouldBe `number`[Int]()
  }

  test("boolean") {
    parseType {
      """{
        | "type": "boolean",
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
    }.value shouldBe `array`(`string`())
  }

  def parseType(x: String): Try[Schema[_]] =
    ParseJson(x) find { case e: Value.obj => e } flatMap ParseJsonSchema.makeType
}
