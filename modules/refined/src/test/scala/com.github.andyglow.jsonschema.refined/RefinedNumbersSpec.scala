package com.github.andyglow.jsonschema.refined

import org.scalatest._
import org.scalatest.Matchers._
import json.Json.schema
import json.Schema._
import json.Validation._
import com.github.andyglow.jsonschema.RefinedSupport._
import eu.timepit.refined._
import eu.timepit.refined.api._
import eu.timepit.refined.numeric._


class RefinedNumbersSpec extends FunSuite {

  test("int positive") {
      schema[Int Refined Positive] shouldBe `number`[Int].withValidation(`exclusiveMinimum` := 0)
  }

  test("int negative") {
      schema[Int Refined Negative] shouldBe `number`[Int].withValidation(`exclusiveMaximum` := 0)
  }

  test("int non positive") {
      schema[Int Refined NonPositive] shouldBe `number`[Int].withValidation(`maximum` := 0)
  }

  test("int non negative") {
      schema[Int Refined NonNegative] shouldBe `number`[Int].withValidation(`minimum` := 0)
  }

  test("int > n") {
      schema[Int Refined Greater[W.`20`.T]] shouldBe `number`[Int].withValidation(`exclusiveMinimum` := 20)
  }

  test("int < n") {
      schema[Int Refined Less[W.`20`.T]] shouldBe `number`[Int].withValidation(`exclusiveMaximum` := 20)
  }

  test("int <= n") {
      schema[Int Refined GreaterEqual[W.`20`.T]] shouldBe `number`[Int].withValidation(`minimum` := 20)
  }

  test("int >= n") {
      schema[Int Refined LessEqual[W.`20`.T]] shouldBe `number`[Int].withValidation(`maximum` := 20)
  }

  test("int mod n") {
      schema[Int Refined Divisible[W.`20`.T]] shouldBe `number`[Int].withValidation(`multipleOf` := 20)
  }
}
