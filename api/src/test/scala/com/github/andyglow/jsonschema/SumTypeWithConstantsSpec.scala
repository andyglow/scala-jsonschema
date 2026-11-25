package com.github.andyglow.jsonschema

import com.github.andyglow.jsonschema.harness.Families.as07
import json._
import json.Schema._
import json.schema._
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec._

class SumTypeWithConstantsSpec extends AnyWordSpec {
  import SumTypeModels._

  "Macro" should {
    "generate schema for Sealed Trait subclasses defined inside of it's companion object and include sub-types annotations, enum-as-oneof" in {
      import `object`.Field

      implicit val flag: Flag with Flag.EnumsAsOneOf = null

      // "M4" is a string too: an anyOf (B3, #461; a oneOf rejected "M4")
      as07(Json.schema[FooBarInsideCompanionWithAnnotations]) shouldEqual `anyof`(
        Set(
          `object`(Field("val1", `number`[Double])).withTitle("t1").withDescription("d1"),
          `object`(Field("val2", `number`[Double])).withTitle("t2").withDescription("d2"),
          `value-class`(`string`).withTitle("t3").withDescription("d3"),
          `const`("M4").withTitle("t4").withDescription("d4"),
          `const`("M5").withTitle("t5").withDescription("d5")
        )
      )
    }
  }
}
