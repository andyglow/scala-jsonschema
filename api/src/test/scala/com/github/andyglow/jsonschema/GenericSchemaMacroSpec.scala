package com.github.andyglow.jsonschema

import java.time.LocalDateTime

import json.Json
import json.Schema._
import json.Schema.`object`.Field
import org.scalatest._
import org.scalatest.Matchers._


class GenericSchemaMacroSpec extends FunSuite {

  test("generic case class") {
    Json.schema[GenericCC[Int]] shouldEqual `object`(
      Field("head", `integer`),
      Field("tail", `array`(`integer`)))
  }

  test("generic case class with generic nesting") {
    import `string`.Format._

    Json.schema[DoubleGenericCC[LocalDateTime]] shouldEqual `object`(
      Field("str", `string`()),
      Field("gen", `object`(
        Field("head", `string`(`date-time`)),
        Field("tail", `array`(`string`(`date-time`))))))
  }

  test("generic case class with generic nesting and defaults") {
    Json.schema[DoubleGenericCCWithDefaults[Long]] shouldEqual `object`(
      Field("str", `string`(), required = false, "abc"),
      Field("gen", `object`(
        Field("head", `number`[Long]),
        Field("tail", `array`(`number`[Long])))))
  }

  test("generic case class with generic nesting and defaults defined externally") {
    Json.schema[DoubleGenericCCWithPredefinedDefaults[Long]] shouldEqual `object`(
      Field("str", `string`(), required = false, "some-str"),
      Field("gen", `object`(
        Field("head", `number`[Long]),
        Field("tail", `array`(`number`[Long])))))
  }
}

case class GenericCC[T](head: T, tail: List[T])

case class DoubleGenericCC[T](str: String, gen: GenericCC[T])

case class DoubleGenericCCWithDefaults[T](str: String = "abc", gen: GenericCC[T])

case class DoubleGenericCCWithPredefinedDefaults[T](
  str: String = GenericSchemaMacroSpec.defaultStr1 + "-" + GenericSchemaMacroSpec.defaultStr2,
  gen: GenericCC[T])

object GenericSchemaMacroSpec {

  val defaultStr1 = "some"
  val defaultStr2= "str"
}