package com.github.andyglow.jsonschema

import json._
import json.Schema._
//import json.schema._
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec._

class SumTypeWithEnumsSpec extends AnyWordSpec {
  import SumTypeModels._

  "Macro" should {
    "generate schema for Sealed Trait subclasses defined inside of it's companion object and include sub-types annotations" in {
      import `object`.Field

      Json.schema[FooBarInsideCompanionWithAnnotations] shouldEqual `oneof`(
        Set(
          `object`(Field("val1", `number`[Double])).withTitle("t1").withDescription("d1"),
          `object`(Field("val2", `number`[Double])).withTitle("t2").withDescription("d2"),
          `value-class`(`string`).withTitle("t3").withDescription("d3"),
          `enum`.of("M4", "M5")
        )
      )
    }
  }
}
