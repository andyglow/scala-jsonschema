package com.github.andyglow.jsonschema

import cats.data._
import com.github.andyglow.json.ToValue
import json.Schema
import json.Schema.ValidationBound.mk
import json.Schema.`object`._
import json.Schema._
import json.Validation.`minItems`
import json.schema.Predef
import org.scalatest._
import org.scalatest.Matchers._

import scala.languageFeature.implicitConversions


class CatsSupportSpec extends WordSpec {
  import CatsSupportSpec._

  "CatsSupportSpec" when {

    "NonEmptyList" should {

      "be exposed as object" in {
        nelEventSchema shouldBe `object`(
          Field("id", `string`(None, None)),
          Field("arr", `array`(`string`(None, None))))
      }
    }

    "NonEmptyVector" should {

      "be exposed as object" in {
        nevEventSchema shouldBe `object`(
          Field("id", `string`(None, None)),
          Field("arr", `array`(`string`(None, None))))
      }
    }

    "NonEmptySet" should {

      "be exposed as object" in {
        nesEventSchema shouldBe `object`(
          Field("id", `string`(None, None)),
          Field("arr", `array`(`string`(None, None))))
      }
    }

    "NonEmptyChain" should {

      "be exposed as object" in {
        necEventSchema shouldBe `object`(
          Field("id", `string`(None, None)),
          Field("arr", `array`(`string`(None, None))))
      }
    }

    "NonEmptyStream" should {

      "be exposed as object" in {
        nestEventSchema shouldBe `object`(
          Field("id", `string`(None, None)),
          Field("arr", `array`(`string`(None, None))))
      }
    }

    "NonEmptyMap[String, _]" should {

      "be exposed as object" in {
        nesmEventSchema shouldBe `object`(
          Field("id", `string`(None, None)),
          Field("data", `string-map`(`string`(None, None))))
      }
    }

    "NonEmptyMap[Int, _]" should {

      "be exposed as object" in {
        neimEventSchema shouldBe `object`(
          Field("id", `string`(None, None)),
          Field("data", `int-map`(`string`(None, None))))
      }
    }

    "OneAnd[List, _]" should {

      "be exposed as object" in {
        oneAndListEventSchema shouldBe `object`(
          Field("x", `array`(`integer`)))
      }
    }
  }
}

object CatsSupportSpec {
  import CatsSupport._

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

  case class NonEmptyStreamEvent(id: String, arr: NonEmptyStream[String])
  lazy val nestEventSchema: Schema[NonEmptyStreamEvent] = json.Json.schema[NonEmptyStreamEvent]

  case class OneAndListEvent(x: OneAnd[List, Int])
  lazy val oneAndListEventSchema: Schema[OneAndListEvent] = json.Json.schema[OneAndListEvent]
}