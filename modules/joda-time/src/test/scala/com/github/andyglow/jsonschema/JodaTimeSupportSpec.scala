package com.github.andyglow.jsonschema

import json.Schema._
import json.Schema.`string`.Format._
import json.Schema.`object`.Field
import org.joda.time._
import matchers.should.Matchers._
import org.scalatest.{time => _, _}
import json._
import org.scalatest.matchers
import org.scalatest.wordspec.AnyWordSpec


class JodaTimeSupportSpec extends AnyWordSpec {
  import JodaTimeSupportSpec._

  "JodaTimeSupport" when {

    "DateTime" should {

      "be exposed as string/date-time" in {
        dateTimeEventSchema shouldBe `object`(
          Field("id", `string`(None, None)),
          Field("timestamp", ref("org.joda.time.DateTime", `string`(Some(`date-time`), None))))
      }
    }

    "Instant" should {

      "be exposed as string/date-time" in {
        instantEventSchema shouldBe `object`(
          Field("id", `string`(None, None)),
          Field("timestamp", ref("org.joda.time.Instant", `string`(Some(`date-time`), None))))
      }
    }

    "LocalDateTime" should {

      "be exposed as string/date-time" in {
        localDateTimeEventSchema shouldBe `object`(
          Field("id", `string`(None, None)),
          Field("timestamp", ref("org.joda.time.LocalDateTime", `string`(Some(`date-time`), None))))
      }
    }

    "LocalDate" should {

      "be exposed as string/date" in {
        localDateEventSchema shouldBe `object`(
          Field("id", `string`(None, None)),
          Field("timestamp", ref("org.joda.time.LocalDate", `string`(Some(`date`), None))))
      }
    }

    "LocalTime" should {

      "be exposed as string/time" in {
        localTimeEventSchema shouldBe `object`(
          Field("id", `string`(None, None)),
          Field("timestamp", ref("org.joda.time.LocalTime", `string`(Some(`time`), None))))
      }
    }
  }
}

object JodaTimeSupportSpec {
  import JodaTimeSupport._

  case class InstantEvent(id: String, timestamp: Instant)
  val instantEventSchema: Schema[InstantEvent] = json.Json.schema[InstantEvent]

  case class DateTimeEvent(id: String, timestamp: DateTime)
  val dateTimeEventSchema: Schema[DateTimeEvent] = json.Json.schema[DateTimeEvent]

  case class LocalDateTimeEvent(id: String, timestamp: LocalDateTime)
  val localDateTimeEventSchema: Schema[LocalDateTimeEvent] = json.Json.schema[LocalDateTimeEvent]

  case class LocalDateEvent(id: String, timestamp: LocalDate)
  val localDateEventSchema: Schema[LocalDateEvent] = json.Json.schema[LocalDateEvent]

  case class LocalTimeEvent(id: String, timestamp: LocalTime)
  val localTimeEventSchema: Schema[LocalTimeEvent] = json.Json.schema[LocalTimeEvent]
}