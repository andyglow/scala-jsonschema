package com.github.andyglow.jsonschema

import java.time._
import java.time.format.DateTimeFormatter

import com.github.andyglow.json.Value._
import json._
import json.Schema._
import json.Schema.`string`.Format
import json.schema.{JsonSchemaProfile, Version}
import json.schema.validation.Instance._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._
import petshop.Acceptance

import scala.concurrent.duration.{Duration => SDuration, FiniteDuration}

/** The java.time Predefs added by #474 (design doc `docs/design/474-java-time.md`): `OffsetDateTime`, `OffsetTime`, `Period`, `Year`, `YearMonth`, `MonthDay`, `ZoneOffset`, `ZoneId`, and
  * `scala.concurrent.duration.FiniteDuration`. Each is a core Predef, so it is inlined (never a definition), also as a field. The tree and rendering tests run on both platforms; the validating ones use
  * the acceptance harness' JSON Schema validator (JVM; canceled on Scala.js) on the text java.time and the codecs write.
  */
class JavaTimeSchemaSpec extends AnyFunSuite {
  import JavaTimeSchemaSpec._
  import ProfileSpec.{body, drafts, prop, render}

  private val profiles = List(JsonSchemaProfile.Neutral, JsonSchemaProfile.Legacy)

  test("the trees: a string with a format or a pattern") {
    Json.schema[OffsetDateTime] shouldBe `string`[OffsetDateTime](Format.`date-time`)
    Json.schema[OffsetTime] shouldBe `string`[OffsetTime](Format.`time`)
    Json.schema[Period] shouldBe `string`[Period].withValidation(`pattern` := PeriodPattern)
    Json.schema[Year] shouldBe `string`[Year].withValidation(`pattern` := YearPattern)
    Json.schema[YearMonth] shouldBe `string`[YearMonth].withValidation(`pattern` := YearMonthPattern)
    Json.schema[MonthDay] shouldBe `string`[MonthDay].withValidation(`pattern` := MonthDayPattern)
    Json.schema[ZoneOffset] shouldBe `string`[ZoneOffset].withValidation(`pattern` := ZoneOffsetPattern)
    Json.schema[ZoneId] shouldBe `string`[ZoneId]
    Json.schema[FiniteDuration] shouldBe `string`[FiniteDuration](Format.`duration`)
  }

  test("FiniteDuration is the same as Duration, also as a field") {
    Json.schema[FiniteDuration] shouldBe Json.schema[SDuration]
    Json.schema[Timeouts] shouldBe `object`(
      `object`.Field("connect", `string`[FiniteDuration](Format.`duration`)),
      `object`.Field("idle", `string`[SDuration](Format.`duration`))
    )
  }

  test("as fields: inlined, never a definition") {
    val s = Json.schema[Times]
    s shouldBe `object`(
      `object`.Field("odt", `string`[OffsetDateTime](Format.`date-time`)),
      `object`.Field("ot", `string`[OffsetTime](Format.`time`)),
      `object`.Field("p", `string`[Period].withValidation(`pattern` := PeriodPattern)),
      `object`.Field("y", `string`[Year].withValidation(`pattern` := YearPattern)),
      `object`.Field("ym", `string`[YearMonth].withValidation(`pattern` := YearMonthPattern)),
      `object`.Field("md", `string`[MonthDay].withValidation(`pattern` := MonthDayPattern)),
      `object`.Field("zo", `string`[ZoneOffset].withValidation(`pattern` := ZoneOffsetPattern)),
      `object`.Field("zid", `string`[ZoneId]),
      `object`.Field("fd", `string`[FiniteDuration](Format.`duration`))
    )
  }

  for (p <- profiles) {
    implicit val profile: JsonSchemaProfile = p

    test(s"${p.name}: rendered on every draft (duration only from 2019-09)") {
      drafts foreach { v =>
        val dur = v match {
          case _: Version.Draft09 | _: Version.Draft12 => obj("type" -> "string", "format" -> "duration")
          case _                                       => obj("type" -> "string")
        }
        body(render(Json.schema[OffsetDateTime], v)) shouldBe obj("type" -> "string", "format" -> "date-time")
        body(render(Json.schema[OffsetTime], v)) shouldBe obj("type" -> "string", "format" -> "time")
        body(render(Json.schema[Period], v)) shouldBe obj("type" -> "string", "pattern" -> PeriodPattern)
        body(render(Json.schema[Year], v)) shouldBe obj("type" -> "string", "pattern" -> YearPattern)
        body(render(Json.schema[YearMonth], v)) shouldBe obj("type" -> "string", "pattern" -> YearMonthPattern)
        body(render(Json.schema[MonthDay], v)) shouldBe obj("type" -> "string", "pattern" -> MonthDayPattern)
        body(render(Json.schema[ZoneOffset], v)) shouldBe obj("type" -> "string", "pattern" -> ZoneOffsetPattern)
        body(render(Json.schema[ZoneId], v)) shouldBe obj("type" -> "string")
        body(render(Json.schema[FiniteDuration], v)) shouldBe dur
        prop(render(Json.schema[Timeouts], v), "connect") shouldBe dur
        prop(render(Json.schema[Times], v), "zid") shouldBe obj("type" -> "string")
      }
    }

    // the patterns hold on every draft
    for ((name, schema, valid, invalid) <- patterned) {
      test(s"${p.name}: $name accepts what java.time and the codecs write") {
        Acceptance.assertValid(Acceptance.rendered(schema), valid.map(quoted))
      }
      test(s"${p.name}: $name rejects other text") {
        Acceptance.assertInvalid(Acceptance.rendered(schema), invalid.map(quoted) :+ "1")
      }
    }

    // a format is asserted on draft-04 / draft-07 only (networknt treats it as an annotation on 2020-12)
    for ((name, schema, valid, invalid) <- formatted) {
      test(s"${p.name}: $name accepts what the codecs write") {
        Acceptance.assertValid(Acceptance.rendered(schema), valid.map(quoted))
      }
      test(s"${p.name}: $name rejects text without an offset (where the format is asserted)") {
        val render = Acceptance.rendered(schema)
        Acceptance.drafts.filterNot(_.isInstanceOf[Version.Draft12]) foreach { d =>
          invalid.map(quoted) foreach { doc => Acceptance.errors(d, render(d), doc) should not be empty }
        }
      }
    }
  }

  test("the sample texts are what java.time writes (toString, and the ISO formatters circe and play-json use)") {
    OffsetDateTime.of(2024, 1, 1, 10, 0, 0, 0, ZoneOffset.ofHours(2)).format(DateTimeFormatter.ISO_OFFSET_DATE_TIME) shouldBe "2024-01-01T10:00:00+02:00"
    OffsetTime.of(10, 15, 0, 0, ZoneOffset.UTC).format(DateTimeFormatter.ISO_OFFSET_TIME) shouldBe "10:15:00Z"
    Period.of(1, -2, 3).toString shouldBe "P1Y-2M3D"
    Period.ZERO.toString shouldBe "P0D"
    Period.ofWeeks(2).toString shouldBe "P14D"
    Year.of(5).toString shouldBe "5"
    Year.of(5).format(DateTimeFormatter.ofPattern("uuuu")) shouldBe "0005"
    Year.of(12345).toString shouldBe "12345"
    YearMonth.of(2024, 3).toString shouldBe "2024-03"
    YearMonth.of(-5, 1).toString shouldBe "-0005-01"
    YearMonth.of(12345, 12).toString shouldBe "+12345-12"
    MonthDay.of(12, 3).toString shouldBe "--12-03"
    ZoneOffset.UTC.toString shouldBe "Z"
    ZoneOffset.ofHoursMinutes(-5, -30).toString shouldBe "-05:30"
    ZoneOffset.ofHoursMinutesSeconds(1, 0, 30).toString shouldBe "+01:00:30"
  }
}

object JavaTimeSchemaSpec {
  case class Timeouts(connect: FiniteDuration, idle: SDuration)

  case class Times(odt: OffsetDateTime, ot: OffsetTime, p: Period, y: Year, ym: YearMonth, md: MonthDay, zo: ZoneOffset, zid: ZoneId, fd: FiniteDuration)

  val PeriodPattern     = "^P(?!$)(?:-?[0-9]+Y)?(?:-?[0-9]+M)?(?:-?[0-9]+D)?$"
  val YearPattern       = "^[-+]?[0-9]+$"
  val YearMonthPattern  = "^[-+]?[0-9]{4,}-(?:0[1-9]|1[0-2])$"
  val MonthDayPattern   = "^--(?:0[1-9]|1[0-2])-(?:0[1-9]|[12][0-9]|3[01])$"
  val ZoneOffsetPattern = "^(?:Z|[-+][0-9]{2}:[0-9]{2}(?::[0-9]{2})?)$"

  def quoted(s: String): String = "\"" + s + "\""

  /** name, schema, texts java.time / the codecs write, other texts. */
  val patterned: List[(String, Schema[_], List[String], List[String])] = List(
    ("Period", Json.schema[Period], List("P0D", "P1Y2M3D", "P1Y-2M3D", "P-1Y", "P14D", "P10M"), List("P", "PT1S", "1Y", "P1D2M", "")),
    ("Year", Json.schema[Year], List("5", "0005", "-5", "-0005", "2024", "12345", "+12345"), List("", "20a", "2024-01", "+")),
    ("YearMonth", Json.schema[YearMonth], List("2024-03", "-0005-01", "+12345-12", "0001-01"), List("2024-3", "2024-13", "24-03", "2024-00", "2024-03-01")),
    ("MonthDay", Json.schema[MonthDay], List("--12-03", "--02-29", "--01-31"), List("12-03", "--13-01", "--12-32", "--1-1", "--00-10")),
    ("ZoneOffset", Json.schema[ZoneOffset], List("Z", "+01:00", "-05:30", "+01:00:30", "+18:00"), List("+1", "UTC", "+0100", "z", "+01:00:3")),
    ("ZoneId", Json.schema[ZoneId], List("Europe/Paris", "Z", "+01:00", "UTC+01:00", "America/Argentina/Buenos_Aires"), Nil)
  )

  val formatted: List[(String, Schema[_], List[String], List[String])] = List(
    ("OffsetDateTime", Json.schema[OffsetDateTime], List("2024-01-01T10:00:00+02:00", "2024-01-01T10:00:00.123456789Z"), List("2024-01-01T10:00:00", "x")),
    ("OffsetTime", Json.schema[OffsetTime], List("10:15:00Z", "10:15:30.5+01:00"), List("10:15:30", "x"))
  )
}
