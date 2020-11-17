package com.github.andyglow.jsonschema

import cats.data._
import json.Schema
import json.Schema.`object`._
import json.Schema._
import json.schema.validation.Instance._
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec
import CatsSupport._


class ScalaVersionSpecificCatsSupportSpec extends AnyWordSpec {
  import ScalaVersionSpecificCatsSupportSpec._

  "CatsSupportSpec" when {

    "NonEmptyLazyList" should {

      "be exposed as object" in {
        nellEventSchema shouldBe `object`(
          Field("id", `string`()),
          Field("arr", `array`[String, NonEmptyLazyList](`string`()).withValidation(`minItems` := 1)))
      }
    }
  }
}

object ScalaVersionSpecificCatsSupportSpec {

  case class NonEmptyLazyListEvent(id: String, arr: NonEmptyLazyList[String])
  lazy val nellEventSchema: Schema[NonEmptyLazyListEvent] = json.Json.schema[NonEmptyLazyListEvent]
}