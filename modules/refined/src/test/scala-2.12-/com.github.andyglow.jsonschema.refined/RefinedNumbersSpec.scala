package com.github.andyglow.jsonschema.refined

import org.scalatest.matchers.should.Matchers._
import org.scalatest.funsuite._
import json.Json.schema
import json.Schema._
import json.schema.validation.Instance._
import com.github.andyglow.jsonschema.RefinedSupport._
import eu.timepit.refined._
import eu.timepit.refined.api._
import eu.timepit.refined.numeric._

class RefinedNumbersSpec extends AnyFunSuite {

  test("int positive") {
    schema[Int Refined Positive] shouldBe `integer`.withValidation(`exclusiveMinimum` := 0)
  }

  test("int negative") {
    schema[Int Refined Negative] shouldBe `integer`.withValidation(`exclusiveMaximum` := 0)
  }

  test("int non positive") {
    schema[Int Refined NonPositive] shouldBe `integer`.withValidation(`maximum` := 0)
  }

  test("int non negative") {
    schema[Int Refined NonNegative] shouldBe `integer`.withValidation(`minimum` := 0)
  }

  test("int > n") {
    schema[Int Refined Greater[W.`20`.T]] shouldBe `integer`.withValidation(
      `exclusiveMinimum` := 20
    )
  }

  test("int < n") {
    schema[Int Refined Less[W.`20`.T]] shouldBe `integer`.withValidation(`exclusiveMaximum` := 20)
  }

  test("int <= n") {
    schema[Int Refined GreaterEqual[W.`20`.T]] shouldBe `integer`.withValidation(`minimum` := 20)
  }

  test("int >= n") {
    schema[Int Refined LessEqual[W.`20`.T]] shouldBe `integer`.withValidation(`maximum` := 20)
  }

  test("int mod n") {
    schema[Int Refined Divisible[W.`20`.T]] shouldBe `integer`.withValidation(`multipleOf` := 20)
  }

  test("long positive") {
    schema[Long Refined Positive] shouldBe `integer`.withValidation(`exclusiveMinimum` := 0)
  }

  test("long negative") {
    schema[Long Refined Negative] shouldBe `integer`.withValidation(`exclusiveMaximum` := 0)
  }

  test("long non positive") {
    schema[Long Refined NonPositive] shouldBe `integer`.withValidation(`maximum` := 0)
  }

  test("long non negative") {
    schema[Long Refined NonNegative] shouldBe `integer`.withValidation(`minimum` := 0)
  }

  test("long > n") {
    schema[Long Refined Greater[W.`20`.T]] shouldBe `integer`.withValidation(
      `exclusiveMinimum` := 20
    )
  }

  test("long < n") {
    schema[Long Refined Less[W.`20`.T]] shouldBe `integer`.withValidation(`exclusiveMaximum` := 20)
  }

  test("long <= n") {
    schema[Long Refined GreaterEqual[W.`20`.T]] shouldBe `integer`.withValidation(`minimum` := 20)
  }

  test("long >= n") {
    schema[Long Refined LessEqual[W.`20`.T]] shouldBe `integer`.withValidation(`maximum` := 20)
  }

  test("long mod n") {
    schema[Long Refined Divisible[W.`20`.T]] shouldBe `integer`.withValidation(`multipleOf` := 20)
  }

  test("double positive") {
    schema[Double Refined Positive] shouldBe `number`[Double].withValidation(
      `exclusiveMinimum` := 0
    )
  }

  test("double negative") {
    schema[Double Refined Negative] shouldBe `number`[Double].withValidation(
      `exclusiveMaximum` := 0
    )
  }

  test("double non positive") {
    schema[Double Refined NonPositive] shouldBe `number`[Double].withValidation(`maximum` := 0)
  }

  test("double non negative") {
    schema[Double Refined NonNegative] shouldBe `number`[Double].withValidation(`minimum` := 0)
  }

  test("double > n") {
    schema[Double Refined Greater[W.`20`.T]] shouldBe `number`[Double].withValidation(
      `exclusiveMinimum` := 20
    )
  }

  test("double < n") {
    schema[Double Refined Less[W.`20`.T]] shouldBe `number`[Double].withValidation(
      `exclusiveMaximum` := 20
    )
  }

  test("double <= n") {
    schema[Double Refined GreaterEqual[W.`20`.T]] shouldBe `number`[Double].withValidation(
      `minimum` := 20
    )
  }

  test("double >= n") {
    schema[Double Refined LessEqual[W.`20`.T]] shouldBe `number`[Double].withValidation(
      `maximum` := 20
    )
  }

  test("double mod n") {
    schema[Double Refined Divisible[W.`20`.T]] shouldBe `number`[Double].withValidation(
      `multipleOf` := 20
    )
  }
}
