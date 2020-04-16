package com.github.andyglow.jsonschema

import cats.data._
import json.Schema
import json.Schema.`object`._
import json.Schema._
import json.Validation._
import org.scalatest._
import org.scalatest.Matchers._
import CatsSupport._


class ScalaVersionSpecificCatsSupportSpec extends WordSpec {
  import ScalaVersionSpecificCatsSupportSpec._

  "CatsSupportSpec" when {

    "NonEmptyStream" should {

      "be exposed as object" in {
        nestEventSchema shouldBe `object`(
          Field("id", `string`()),
          Field("arr", `array`[String, NonEmptyStream](`string`()).withValidation(`minItems` := 1)))
      }
    }
  }
}

object ScalaVersionSpecificCatsSupportSpec {

  case class NonEmptyStreamEvent(id: String, arr: NonEmptyStream[String])
  lazy val nestEventSchema: Schema[NonEmptyStreamEvent] = json.Json.schema[NonEmptyStreamEvent]
}