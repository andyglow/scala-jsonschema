package com.github.andyglow.jsonschema

import json.Schema._
import json.Schema.`string`.Format._
import json.Schema.`object`.Field
import org.joda.time._
import json._
import json.schema.validation.{Instance => V, Magnet}
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

class JodaTimeSupportSpec extends AnyWordSpec {
  import JodaTimeSupportSpec._

  "JodaTimeSupport" when {

    "DateTime" should {

      "be exposed as string/date-time" in {
        dateTimeEventSchema shouldBe `object`(
          Field("id", `string`),
          Field("timestamp", `def`("org.joda.time.DateTime", `string`(`date-time`)))
        )
      }
    }

    "Instant" should {

      "be exposed as string/date-time" in {
        instantEventSchema shouldBe `object`(
          Field("id", `string`),
          Field("timestamp", `def`("org.joda.time.Instant", `string`(`date-time`)))
        )
      }
    }

    "LocalDateTime" should {

      "be exposed as string/date-time" in {
        localDateTimeEventSchema shouldBe `object`(
          Field("id", `string`),
          Field("timestamp", `def`("org.joda.time.LocalDateTime", `string`(`date-time`)))
        )
      }
    }

    "LocalDate" should {

      "be exposed as string/date" in {
        localDateEventSchema shouldBe `object`(
          Field("id", `string`),
          Field("timestamp", `def`("org.joda.time.LocalDate", `string`(`date`)))
        )
      }
    }

    "LocalTime" should {

      "be exposed as string/time" in {
        localTimeEventSchema shouldBe `object`(
          Field("id", `string`),
          Field("timestamp", `def`("org.joda.time.LocalTime", `string`(`time`)))
        )
      }
    }

    "Period" should {

      "be exposed as string with a pattern that accepts what joda writes" in {
        val pattern = JodaTimeSupport.jtPeriodSchema.validations.collectFirst { case d if d.validation == V.`pattern` => d.value.toString }.get
        periodEventSchema shouldBe `object`(
          Field("id", `string`),
          Field("p", `def`("org.joda.time.Period", `string`[Period].withValidation(V.`pattern` := pattern)(Magnet.mk[Period, String])))
        )
        val re      = java.util.regex.Pattern.compile(pattern)
        val written = List(
          Period.ZERO,
          new Period(1, 2, 3, 4, 5, 6, 7, 8),
          Period.weeks(2),
          Period.days(-1).plusHours(-2),
          Period.days(-3).plusHours(4),
          Period.weeks(1).plusDays(2).plusMillis(8).plusHours(-3),
          Period.millis(-1500),
          Period.years(Int.MaxValue),
          Period.millis(Int.MinValue)
        ).map(_.toString)
        written foreach { s => assert(re.matcher(s).matches, s) }
        List("P", "PT", "P1DT", "1D", "P1.5D", "PT1H2D") foreach { s => assert(!re.matcher(s).matches, s) }
      }
    }

    "DateTimeZone" should {

      "be exposed as string" in {
        import JodaTimeSupport._
        Json.schema[DateTimeZone] shouldBe `def`("org.joda.time.DateTimeZone", `string`[DateTimeZone])
      }
    }

    "Interval" should {

      "be exposed as string" in {
        import JodaTimeSupport._
        Json.schema[Interval] shouldBe `def`("org.joda.time.Interval", `string`[Interval])
      }
    }

    "predef" should {

      "inline Period, DateTimeZone and Interval" in {
        import JodaTimeSupport.predef._
        Json.schema[DateTimeZone] shouldBe `string`[DateTimeZone]
        Json.schema[Interval] shouldBe `string`[Interval]
        Json.schema[Period] shouldBe JodaTimeSupport.jtPeriodSchema
      }
    }
  }
}

object JodaTimeSupportSpec {
  import JodaTimeSupport._

  case class PeriodEvent(id: String, p: Period)
  val periodEventSchema: Schema[PeriodEvent] = json.Json.schema[PeriodEvent]

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
