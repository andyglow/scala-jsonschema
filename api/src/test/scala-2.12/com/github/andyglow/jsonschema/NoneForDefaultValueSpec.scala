package com.github.andyglow.jsonschema

import json._
import json.Schema._
import json.Schema.`object`._
import org.scalatest.matchers.should.Matchers._
import org.scalatest.funsuite.AnyFunSuite
import com.github.andyglow.jsonschema.{model => case0}

object NoneForDefaultValueCases {

  object case1 {
    case class Foo(value: Option[String] = None)
  }
}

object case2 {
  case class Foo(value: Option[String] = None)
}

object case3 {
  import case2._
  case class Bar(value: Option[Foo] = None)
}

class NoneForDefaultValueSpec extends AnyFunSuite {
  import NoneForDefaultValueCases._

  test("scala.None for default value") {

    // type id specified in external file
    Json.schema[case0.Foo] shouldBe `object`(Field("value", `string`, required = false))

    // type is specified in same file
    Json.schema[case2.Foo] shouldBe `object`(Field("value", `string`, required = false))

    // type is specified in same file, companion object
    Json.schema[case1.Foo] shouldBe `object`(Field("value", `string`, required = false))

    // type hierarchy
    // same source file
    Json.schema[case3.Bar] shouldBe `object`(Field("value", `object`(Field("value", `string`, required = false)), required = false))
    // external source file
    Json.schema[NoneForDefaultModelsCase4] shouldBe `object`(Field("value", `object`(Field("value", `string`, required = false)), required = false))
    // external module
    Json.schema[ExternalNoneForDefaultModelsCase5] shouldBe `object`(Field("value", `object`(Field("value", `string`, required = false)), required = false))
  }
}
