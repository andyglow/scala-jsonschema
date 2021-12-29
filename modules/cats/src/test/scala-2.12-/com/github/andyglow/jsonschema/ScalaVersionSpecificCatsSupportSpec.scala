package com.github.andyglow.jsonschema

import cats.data._
import json.Schema
import json.Schema.`object`._
import json.Schema._
import json.schema.validation.Instance._
import CatsSupport._
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

class ScalaVersionSpecificCatsSupportSpec extends AnyWordSpec {
  import ScalaVersionSpecificCatsSupportSpec._

  "CatsSupportSpec" when {

    "NonEmptyStream" should {

      "be exposed as object" in {
        nestEventSchema shouldBe `object`(
          Field("id", `string`),
          Field("arr", `array`[String, NonEmptyStream](`string`).withValidation(`minItems` := 1))
        )
      }
    }
  }
}

object ScalaVersionSpecificCatsSupportSpec {

  case class NonEmptyStreamEvent(id: String, arr: NonEmptyStream[String])
  lazy val nestEventSchema: Schema[NonEmptyStreamEvent] = json.Json.schema[NonEmptyStreamEvent]
}
