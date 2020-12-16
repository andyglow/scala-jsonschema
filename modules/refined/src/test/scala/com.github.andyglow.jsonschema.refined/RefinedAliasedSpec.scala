package com.github.andyglow.jsonschema.refined

import eu.timepit.refined.api.Refined
import eu.timepit.refined.boolean.Or
import eu.timepit.refined.{string => S}
import json.Json.schema
import com.github.andyglow.jsonschema.RefinedSupport._
import org.scalatest.matchers.should.Matchers._
import org.scalatest.funsuite._
import json.Schema._
import json.Schema.`string`.Format


class RefinedAliasedSpec extends AnyFunSuite {

  test("partially aliased") {
    type IP = S.IPv4 Or S.IPv6

    schema[Refined[String, IP]] shouldBe `oneof`.of(
      `string`(Format.`ipv4`),
      `string`(Format.`ipv6`))
  }

  test("fully aliased") {
    type IPFormat = S.IPv4 Or S.IPv6
    type IP = String Refined IPFormat

    schema[IP] shouldBe `oneof`.of(
      `string`(Format.`ipv4`),
      `string`(Format.`ipv6`))
  }
}
