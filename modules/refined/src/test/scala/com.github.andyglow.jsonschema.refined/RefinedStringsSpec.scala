package com.github.andyglow.jsonschema.refined

import eu.timepit.refined.{string => _, _}
import eu.timepit.refined.api._
import eu.timepit.refined.string._
import eu.timepit.refined.collection._
import json.Json.schema
import json.Schema._
import json.schema.validation.Instance._
import org.scalatest.matchers.should.Matchers._
import org.scalatest.funsuite._

import com.github.andyglow.jsonschema.RefinedSupport._


class RefinedStringsSpec extends AnyFunSuite {
  import `string`._

  test("non-empty") {
    schema[String Refined NonEmpty] shouldBe `string`.withValidation( `minLength` := 1 )
  }

  test("trimmed") {
    schema[String Refined Trimmed] shouldBe `string`.withValidation(`pattern` := """^(?!\s)[\S ]*(?<!\s)$""")
  }

  test("uuid") {
    schema[String Refined Uuid] shouldBe `string`.withValidation(`pattern` := "^[a-fA-F0-9]{8}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{12}$")
  }

  test("uri") {
    schema[String Refined Uri] shouldBe `string`(Format.`uri`)
  }

  test("url") {
    schema[String Refined Url] shouldBe `string`(Format.`uri`)
  }

  test("startsWith") {
    schema[String Refined StartsWith[W.`"Foo"`.T]] shouldBe `string`.withValidation(`pattern` := "^Foo.*$")
  }

  test("endsWith") {
    schema[String Refined EndsWith[W.`"Bar"`.T]] shouldBe `string`.withValidation(`pattern` := "^.*Bar$")
  }

  test("matchesRegexp") {
    schema[String Refined MatchesRegex[W.`"[a-z]*"`.T]] shouldBe `string`.withValidation(`pattern` := "[a-z]*")
  }

  test("ipv4") {
    schema[String Refined IPv4] shouldBe `string`(Format.`ipv4`)
  }

  test("ipv6") {
    schema[String Refined IPv6] shouldBe `string`(Format.`ipv6`)
  }

  test("xml") {
    schema[String Refined Xml] shouldBe `string`.withValidation( `contentMediaType` := "text/xml", `contentEncoding` := "utf8")
  }

  test("size") {
    schema[String Refined Size[W.`6`.T]] shouldBe `string`.withValidation( `maxLength` := 6, `minLength` := 6 )
  }

  test("minSize") {
    schema[String Refined MinSize[W.`6`.T]] shouldBe `string`.withValidation( `minLength` := 6 )
  }

  test("maxSize") {
    schema[String Refined MaxSize[W.`6`.T]] shouldBe `string`.withValidation( `maxLength` := 6 )
  }
}
