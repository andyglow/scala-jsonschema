package com.github.andyglow.jsonschema

import json.Schema
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers._

class SchemaEqualitySpec extends AnyWordSpec {
  import SchemaEquality._, Schema._, Difference._
  private val F = Schema.`object`.Field

  "SchemaEquality" should {

    "result in Equal for equivalent schemas" when {

      "comparing scalar schemas" when {

        "string" in { compute(`string`, `string`) shouldBe Equal }
        "boolean" in { compute(`boolean`, `boolean`) shouldBe Equal }
        "integer" in { compute(`integer`, `integer`) shouldBe Equal }
        "number" in {
          compute(`number`[Byte], `number`[Byte]) shouldBe Equal
          compute(`number`[Short], `number`[Short]) shouldBe Equal
          compute(`number`[Int], `number`[Int]) shouldBe Equal
          compute(`number`[Long], `number`[Long]) shouldBe Equal
          compute(`number`[Float], `number`[Float]) shouldBe Equal
          compute(`number`[Double], `number`[Double]) shouldBe Equal
        }
      }

      "comparing composite schemas" when {

        "array" in {
          compute(
            `array`(`string`, unique = true),
            `array`(`string`, unique = true)
          ) shouldBe Equal
        }

        "object" in {
          compute(
            `object`(
              F("a", `boolean`, required = true),
              F("b", `string`, required = true),
              F("c", `integer`, required = false)
            ),
            `object`(
              F("a", `boolean`, required = true),
              F("b", `string`, required = true),
              F("c", `integer`, required = false)
            )
          ) shouldBe Equal
        }
      }
    }

    "result in UnEqual for equivalent schemas" when {

      "broken field order" in {
        compute(
          `object`(
            F("a", `boolean`, required = true),
            F("b", `string`, required = true),
            F("c", `integer`, required = false)
          ),
          `object`(
            F("a", `boolean`, required = true),
            F("c", `integer`, required = false),
            F("b", `string`, required = true)
          )
        ) shouldBe UnEqual(NotMatchingFieldOrder(Nil, List("a", "b", "c"), List("a", "c", "b")))
      }

      "missed fields" in {
        compute(
          `object`(
            F("a", `boolean`, required = true),
            F("b", `string`, required = true),
            F("cA", `integer`, required = false)
          ),
          `object`(
            F("a", `boolean`, required = true),
            F("b", `string`, required = true),
            F("cB", `integer`, required = false)
          )
        ) shouldBe UnEqual(MissingFields(Nil, Set(F("cB", `integer`, required = false)), Set(F("cA", `integer`, required = false))))
      }

      "field type mismatch" in {
        compute(
          `object`(
            F("a", `boolean`, required = true),
            F("b", `string`, required = true)
          ),
          `object`(
            F("a", `boolean`, required = true),
            F("b", `integer`, required = true)
          )
        ) shouldBe UnEqual(ClashingSchemas(Segment.Field("b") :: Nil, `string`, `integer`))
      }
    }
  }
}
