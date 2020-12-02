package com.github.andyglow.jsonschema

import json._
import json.Schema._
import json.Schema.`object`._
import org.scalatest.matchers.should.Matchers._
import org.scalatest.funsuite.AnyFunSuite
import com.github.andyglow.jsonschema.{ model => case0 }

object NoneForDefaultValueCases {

  final object case1 {
    case class Foo(value: Option[String] = None)
  }
}

object case2 {
  case class Foo(value: Option[String] = None)
}

class NoneForDefaultValueSpec extends AnyFunSuite {
  import NoneForDefaultValueCases._

  test("scala.None for default value") {

    // type id specified in external file
    Json.schema[case0.Foo] shouldBe `object`(
      Field("value", `string`, required = false))

    // type is specified in same file
    Json.schema[case2.Foo] shouldBe `object`(
      Field("value", `string`, required = false))

    // type is specified in same file, companion object
    Json.schema[case1.Foo] shouldBe `object`(
      Field("value", `string`, required = false))
  }
}
