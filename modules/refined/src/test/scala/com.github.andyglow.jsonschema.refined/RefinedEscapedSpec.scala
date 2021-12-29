package com.github.andyglow.jsonschema.refined

import com.github.andyglow.json.JsonFormatter
import com.github.andyglow.jsonschema.AsValue
import eu.timepit.refined.{string => _, _}
import eu.timepit.refined.api._
import eu.timepit.refined.string._
import json.Json.schema
import json.schema.validation.Instance._
import json.Schema._
import json.schema.Version.Draft04
import org.scalatest.matchers.should.Matchers._
import org.scalatest.funsuite._

import com.github.andyglow.jsonschema.RefinedSupport._

class RefinedEscapedSpec extends AnyFunSuite {

  test("JsonFormatter escapes regex specified by matchesRegexp ") {
    val res = schema[String Refined MatchesRegex[W.`"""\\d{3}[- ]\\d{3}[- ]\\d{4}"""`.T]]
    def d04 = JsonFormatter.format(AsValue.schema(res, Draft04()))

    res shouldBe `string`.withValidation(`pattern` := "\\d{3}[- ]\\d{3}[- ]\\d{4}")

    d04 shouldBe
      s"""{
         |  "$$schema": "http://json-schema.org/draft-04/schema#",
         |  "type": "string",
         |  "pattern": "\\\\d{3}[- ]\\\\d{3}[- ]\\\\d{4}"
         |}""".stripMargin
  }
}
