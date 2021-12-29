package com.github.andyglow.jsonschema

import json.Schema
import json.Schema._
import json.schema.Predef
import org.joda.time._

object JodaTimeSupport {

  implicit val jtDateTimeSchema: Schema[DateTime] = `string`[DateTime](`string`.Format.`date-time`)

  implicit val jtInstantSchema: Schema[Instant] = `string`[Instant](`string`.Format.`date-time`)

  implicit val jtLocalDateTimeSchema: Schema[LocalDateTime] =
    `string`[LocalDateTime](`string`.Format.`date-time`)

  implicit val jtLocalDateSchema: Schema[LocalDate] = `string`[LocalDate](`string`.Format.`date`)

  implicit val jtLocalTimeSchema: Schema[LocalTime] = `string`[LocalTime](`string`.Format.`time`)

  implicit val jtDurationSchema: Schema[Duration] = `string`[Duration](`string`.Format.`duration`)

  object predef {

    implicit val jtDateTimePredef: Predef[DateTime] = Predef(jtDateTimeSchema)

    implicit val jtInstantPredef: Predef[Instant] = Predef(jtInstantSchema)

    implicit val jtLocalDateTimePredef: Predef[LocalDateTime] = Predef(jtLocalDateTimeSchema)

    implicit val jtLocalDatePredef: Predef[LocalDate] = Predef(jtLocalDateSchema)

    implicit val jtLocalTimePredef: Predef[LocalTime] = Predef(jtLocalTimeSchema)

    implicit val jtDurationPredef: Predef[Duration] = Predef(jtDurationSchema)
  }
}
