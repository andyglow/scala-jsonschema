package com.github.andyglow.jsonschema

import cats.data._
import json.Schema
import json.Schema.`object`._
import json.Schema._
import json.Validation._
import org.scalatest._
import org.scalatest.Matchers._
import CatsSupport._


class CatsSupportSpec extends WordSpec {
  import CatsSupportSpec._

  "CatsSupportSpec" when {

    "NonEmptyList" should {

      "be exposed as object" in {
        nelEventSchema shouldBe `object`(
          Field("id", `string`()),
          Field("arr", `array`[String, NonEmptyList](`string`()).withValidation(`minItems` := 1)))
      }
    }

    "NonEmptyVector" should {

      "be exposed as object" in {
        nevEventSchema shouldBe `object`(
          Field("id", `string`()),
          Field("arr", `array`[String, NonEmptyVector](`string`()).withValidation(`minItems` := 1)))
      }
    }

    "NonEmptySet" should {

      "be exposed as object" in {
        nesEventSchema shouldBe `object`(
          Field("id", `string`()),
          Field("arr", `array`[String, NonEmptySet](`string`()).withValidation(`minItems` := 1)))
      }
    }

    "NonEmptyChain" should {

      "be exposed as object" in {
        necEventSchema shouldBe `object`(
          Field("id", `string`()),
          Field("arr", `array`[String, NonEmptyChain](`string`()).withValidation(`minItems` := 1)))
      }
    }

    "NonEmptyMap[String, _]" should {

      "be exposed as object" in {
        nesmEventSchema shouldBe `object`(
          Field("id", `string`()),
          Field("data" , `string-map`[String, NonEmptyMap](`string`()).withValidation(`minProperties` := 1)))
      }
    }

    "NonEmptyMap[Int, _]" should {

      "be exposed as object" in {
        neimEventSchema shouldBe `object`(
          Field("id", `string`()),
          Field("data", `int-map`[String, NonEmptyMap](`string`()).withValidation(`minProperties` := 1)))
      }
    }

    "OneAnd[List, _]" should {

      "be exposed as object" in {
        oneAndListEventSchema shouldBe `object`(
          Field("x", `array`[Int, ({type Z[T] = OneAnd[List, T]})#Z](`integer`).withValidation(`minItems` := 1)))
      }
    }
  }
}

object CatsSupportSpec {

  case class NonEmptyListEvent(id: String, arr: NonEmptyList[String])
  lazy val nelEventSchema: Schema[NonEmptyListEvent] = json.Json.schema[NonEmptyListEvent]

  case class NonEmptyVectorEvent(id: String, arr: NonEmptyVector[String])
  lazy val nevEventSchema: Schema[NonEmptyVectorEvent] = json.Json.schema[NonEmptyVectorEvent]

  case class NonEmptySetEvent(id: String, arr: NonEmptySet[String])
  lazy val nesEventSchema: Schema[NonEmptySetEvent] = json.Json.schema[NonEmptySetEvent]

  case class NonEmptyChainEvent(id: String, arr: NonEmptyChain[String])
  lazy val necEventSchema: Schema[NonEmptyChainEvent] = json.Json.schema[NonEmptyChainEvent]

  case class NonEmptyStringMapEvent(id: String, data: NonEmptyMap[String, String])
  lazy val nesmEventSchema: Schema[NonEmptyStringMapEvent] = json.Json.schema[NonEmptyStringMapEvent]

  case class NonEmptyIntMapEvent(id: String, data: NonEmptyMap[Int, String])
  lazy val neimEventSchema: Schema[NonEmptyIntMapEvent] = json.Json.schema[NonEmptyIntMapEvent]

  case class OneAndListEvent(x: OneAnd[List, Int])
  lazy val oneAndListEventSchema: Schema[OneAndListEvent] = json.Json.schema[OneAndListEvent]
}