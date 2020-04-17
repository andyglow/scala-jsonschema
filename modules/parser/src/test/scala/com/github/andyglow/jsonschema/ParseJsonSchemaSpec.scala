package com.github.andyglow.jsonschema

import com.github.andyglow.json.{ParseJson, Value}
import json.Schema
import org.scalatest._
import matchers.should.Matchers._
import com.github.andyglow.testsupport._
import com.github.andyglow.scalamigration._
import scala.util.Try
import org.scalatest.matchers
import org.scalatest.funsuite.AnyFunSuite


class ParseJsonSchemaSpec extends AnyFunSuite {
  import Schema._

  test("string") {
    parseType {
      """{
        | "type": "string"
        |}
      """.stripMargin
    }.value shouldBe `string`(None, None)
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
    }.value shouldBe `string`(Some(`date`), None)
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
    }.value shouldBe `string`(Some(`time`), None)
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
    }.value shouldBe `string`(Some(`date-time`), None)
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
    }.value shouldBe `string`(Some(`uri`), None)
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
    }.value shouldBe `string`(Some(`email`), None)
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
    }.value shouldBe `string`(Some(`hostname`), None)
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
    }.value shouldBe `string`(Some(`ipv4`), None)
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
    }.value shouldBe `string`(Some(`ipv6`), None)
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
    }.value shouldBe `array`(`string`(None, None))
  }

  def parseType(x: String): Try[Schema[_]] =
    ParseJson(x) find { case e: Value.obj => e } flatMap ParseJsonSchema.makeType
}
