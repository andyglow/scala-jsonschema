package com.github.andyglow.jsonschema

import json.Schema
import json.Schema._
import json.schema.Predef
import json.schema.validation.Instance.`pattern`
import json.schema.validation.Magnet
import org.joda.time._

object JodaTimeSupport {

  implicit val jtDateTimeSchema: Schema[DateTime] = `string`[DateTime](`string`.Format.`date-time`)

  implicit val jtInstantSchema: Schema[Instant] = `string`[Instant](`string`.Format.`date-time`)

  implicit val jtLocalDateTimeSchema: Schema[LocalDateTime] =
    `string`[LocalDateTime](`string`.Format.`date-time`)

  implicit val jtLocalDateSchema: Schema[LocalDate] = `string`[LocalDate](`string`.Format.`date`)

  implicit val jtLocalTimeSchema: Schema[LocalTime] = `string`[LocalTime](`string`.Format.`time`)

  implicit val jtDurationSchema: Schema[Duration] = `string`[Duration](`string`.Format.`duration`)

  /** `Period.toString` (what Jackson and json4s-ext write): `P1Y2M3W4DT5H6M7.008S`, `P-1DT-2H`, `PT0S`. Each field has its own sign, weeks mix with the other units and seconds have a fraction, so not
    * `format: duration` (RFC 3339 allows none of those).
    */
  implicit val jtPeriodSchema: Schema[Period] = `string`[Period].withValidation(
    `pattern` := "^P(?=[-0-9T])(?:-?[0-9]+Y)?(?:-?[0-9]+M)?(?:-?[0-9]+W)?(?:-?[0-9]+D)?(?:T(?=[-0-9])(?:-?[0-9]+H)?(?:-?[0-9]+M)?(?:-?[0-9]+(?:\\.[0-9]+)?S)?)?$"
  )(Magnet.mk[Period, String])

  /** The zone's id: `Europe/Paris`, `UTC`, `+01:00`. */
  implicit val jtDateTimeZoneSchema: Schema[DateTimeZone] = `string`[DateTimeZone]

  /** A plain string: `toString` writes `start/end` (ISO date-times), Jackson `startMillis-endMillis`. */
  implicit val jtIntervalSchema: Schema[Interval] = `string`[Interval]

  object predef {

    implicit val jtDateTimePredef: Predef[DateTime] = Predef(jtDateTimeSchema)

    implicit val jtInstantPredef: Predef[Instant] = Predef(jtInstantSchema)

    implicit val jtLocalDateTimePredef: Predef[LocalDateTime] = Predef(jtLocalDateTimeSchema)

    implicit val jtLocalDatePredef: Predef[LocalDate] = Predef(jtLocalDateSchema)

    implicit val jtLocalTimePredef: Predef[LocalTime] = Predef(jtLocalTimeSchema)

    implicit val jtDurationPredef: Predef[Duration] = Predef(jtDurationSchema)

    implicit val jtPeriodPredef: Predef[Period] = Predef(jtPeriodSchema)

    implicit val jtDateTimeZonePredef: Predef[DateTimeZone] = Predef(jtDateTimeZoneSchema)

    implicit val jtIntervalPredef: Predef[Interval] = Predef(jtIntervalSchema)
  }
}
