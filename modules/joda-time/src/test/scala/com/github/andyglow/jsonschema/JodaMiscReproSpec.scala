package com.github.andyglow.jsonschema

import org.scalatest.funsuite.AnyFunSuite

class JodaMiscReproSpec extends AnyFunSuite {
  import JodaTimeSupport._
  test("repro: joda Period") { assertCompiles("json.Json.schema[org.joda.time.Period]") }
  test("repro: joda DateTimeZone") { assertCompiles("json.Json.schema[org.joda.time.DateTimeZone]") }
  test("repro: joda Interval") { assertCompiles("json.Json.schema[org.joda.time.Interval]") }
}
