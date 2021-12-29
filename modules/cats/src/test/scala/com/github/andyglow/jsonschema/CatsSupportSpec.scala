package com.github.andyglow.jsonschema

import cats.data._
import json.Json
import json.Schema
import json.Schema.`object`._
import json.Schema._
import json.schema.validation.Instance._
import CatsSupport._
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

class CatsSupportSpec extends AnyWordSpec {
  import CatsSupportSpec._

  "CatsSupportSpec" when {

    "NonEmptyList" should {

      "be exposed as object" in {
        Json.schema[NonEmptyList[String]] shouldBe `array`[String, NonEmptyList](`string`)
          .withValidation(`minItems` := 1)

        nelEventSchema shouldBe `object`(
          Field("id", `string`),
          Field("arr", `array`[String, NonEmptyList](`string`).withValidation(`minItems` := 1))
        )
      }
    }

    "NonEmptyVector" should {

      "be exposed as object" in {
        Json.schema[NonEmptyVector[String]] shouldBe `array`[String, NonEmptyVector](`string`)
          .withValidation(`minItems` := 1)

        nevEventSchema shouldBe `object`(
          Field("id", `string`),
          Field("arr", `array`[String, NonEmptyVector](`string`).withValidation(`minItems` := 1))
        )
      }
    }

    "NonEmptySet" should {

      "be exposed as object" in {
        Json.schema[NonEmptySet[String]] shouldBe `array`[String, NonEmptySet](`string`)
          .withValidation(`minItems` := 1, `uniqueItems` := true)

        nesEventSchema shouldBe `object`(
          Field("id", `string`),
          Field(
            "arr",
            `array`[String, NonEmptySet](`string`)
              .withValidation(`minItems` := 1, `uniqueItems` := true)
          )
        )
      }
    }

    "NonEmptyChain" should {

      "be exposed as object" in {
        Json.schema[NonEmptyChain[String]] shouldBe `array`[String, NonEmptyChain](`string`)
          .withValidation(`minItems` := 1)

        necEventSchema shouldBe `object`(
          Field("id", `string`),
          Field("arr", `array`[String, NonEmptyChain](`string`).withValidation(`minItems` := 1))
        )
      }
    }

    "NonEmptyMap[String, _]" should {

      "be exposed as object" in {
        Json.schema[NonEmptyMap[String, String]] shouldBe `dictionary`[String, String, NonEmptyMap](
          `string`
        ).withValidation(`minProperties` := 1)

        nesmEventSchema shouldBe `object`(
          Field("id", `string`),
          Field(
            "data",
            `dictionary`[String, String, NonEmptyMap](`string`).withValidation(`minProperties` := 1)
          )
        )
      }
    }

    "NonEmptyMap[Int, _]" should {

      "be exposed as object" in {
        Json.schema[NonEmptyMap[Int, String]] shouldBe `dictionary`[Int, String, NonEmptyMap](
          `string`
        ).withValidation(`minProperties` := 1, `patternProperties` := "^[0-9]+$")

        neimEventSchema shouldBe `object`(
          Field("id", `string`),
          Field(
            "data",
            `dictionary`[Int, String, NonEmptyMap](`string`)
              .withValidation(`minProperties` := 1, `patternProperties` := "^[0-9]+$")
          )
        )
      }
    }

    "OneAnd[List, _]" should {

      "be exposed as object" in {
        Json.schema[OneAnd[List, Int]] shouldBe `array`[Int, ({ type Z[T] = OneAnd[List, T] })#Z](
          `integer`
        ).withValidation(`minItems` := 1)

        oneAndListEventSchema shouldBe `object`(
          Field(
            "x",
            `array`[Int, ({ type Z[T] = OneAnd[List, T] })#Z](`integer`)
              .withValidation(`minItems` := 1)
          )
        )
      }
    }
  }
}

object CatsSupportSpec {

  case class NonEmptyListEvent(id: String, arr: NonEmptyList[String])
  lazy val nelEventSchema: Schema[NonEmptyListEvent] = Json.schema[NonEmptyListEvent]

  case class NonEmptyVectorEvent(id: String, arr: NonEmptyVector[String])
  lazy val nevEventSchema: Schema[NonEmptyVectorEvent] = Json.schema[NonEmptyVectorEvent]

  case class NonEmptySetEvent(id: String, arr: NonEmptySet[String])
  lazy val nesEventSchema: Schema[NonEmptySetEvent] = Json.schema[NonEmptySetEvent]

  case class NonEmptyChainEvent(id: String, arr: NonEmptyChain[String])
  lazy val necEventSchema: Schema[NonEmptyChainEvent] = Json.schema[NonEmptyChainEvent]

  case class NonEmptyStringMapEvent(id: String, data: NonEmptyMap[String, String])
  lazy val nesmEventSchema: Schema[NonEmptyStringMapEvent] = Json.schema[NonEmptyStringMapEvent]

  case class NonEmptyIntMapEvent(id: String, data: NonEmptyMap[Int, String])
  lazy val neimEventSchema: Schema[NonEmptyIntMapEvent] = Json.schema[NonEmptyIntMapEvent]

  case class OneAndListEvent(x: OneAnd[List, Int])
  lazy val oneAndListEventSchema: Schema[OneAndListEvent] = Json.schema[OneAndListEvent]
}
