package com.github.andyglow.jsonschema

import json.Schema
import json.Schema._
import org.joda.time._


object JodaTimeSupport {

  implicit val jtDateTimeSchema: Schema[DateTime] = `string`[DateTime](Some(`string`.Format.`date-time`), None)

  implicit val jtInstantSchema: Schema[Instant] = `string`[Instant](Some(`string`.Format.`date-time`), None)

  implicit val jtLocalDateTimeSchema: Schema[LocalDateTime] = `string`[LocalDateTime](Some(`string`.Format.`date-time`), None)

  implicit val jtLocalDateSchema: Schema[LocalDate] = `string`[LocalDate](Some(`string`.Format.`date`), None)

  implicit val jtLocalTimeSchema: Schema[LocalTime] = `string`[LocalTime](Some(`string`.Format.`time`), None)
}
