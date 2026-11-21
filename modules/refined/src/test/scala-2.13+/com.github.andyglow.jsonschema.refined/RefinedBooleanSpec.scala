package com.github.andyglow.jsonschema.refined

import eu.timepit.refined.api._
import eu.timepit.refined.numeric._
import eu.timepit.refined.boolean._
import json.Json.schema
import json.Schema._
import json.schema.validation.Instance._
import org.scalatest.matchers.should.Matchers.{not => _, _}
import org.scalatest.funsuite._

import com.github.andyglow.jsonschema.RefinedSupport._

class RefinedBooleanSpec extends AnyFunSuite {

  test("not") {
    schema[Refined[Int, Not[GreaterEqual[2]]]] shouldBe `integer`.withValidation(
      `exclusiveMaximum` := 2
    )
    schema[Refined[Int, Not[Divisible[2]]]] shouldBe `not`[Int](
      `integer`.withValidation(`multipleOf` := 2)
    )
    schema[Refined[Int, Not[Not[Divisible[2]]]]] shouldBe `integer`.withValidation(
      `multipleOf` := 2
    )
  }

  test("and") {
    // normalized
    schema[Refined[Int, Greater[2] And Greater[7]]] shouldBe `integer`.withValidation(
      `exclusiveMinimum` := 7
    )
    schema[Refined[Int, Less[2] And Less[7]]] shouldBe `integer`.withValidation(
      `exclusiveMaximum` := 2
    )

    // bounded
    schema[Refined[Int, GreaterEqual[2] And LessEqual[7]]] shouldBe `integer`
      .withValidation(`minimum` := 2, `maximum` := 7)
    schema[Refined[Int, GreaterEqual[2] And Less[7]]] shouldBe `integer`.withValidation(
      `minimum`          := 2,
      `exclusiveMaximum` := 7
    )
    schema[Refined[Int, Greater[2] And LessEqual[7]]] shouldBe `integer`.withValidation(
      `exclusiveMinimum` := 2,
      `maximum`          := 7
    )
    schema[Refined[Int, Greater[2] And Less[7]]] shouldBe `integer`.withValidation(
      `exclusiveMinimum` := 2,
      `exclusiveMaximum` := 7
    )

    schema[Refined[Int, Less[7] And Greater[2]]] shouldBe `integer`.withValidation(
      `exclusiveMinimum` := 2,
      `exclusiveMaximum` := 7
    )

    // somehow scalatest doesn't catch a compile error in here, so commenting it out for now
    // "schema[Refined[Int, Less[2] And Greater[7]]]" shouldNot compile
  }

  test("or") {
    // normalized
    schema[Refined[Int, Greater[2] Or Greater[7]]] shouldBe `integer`.withValidation(
      `exclusiveMinimum` := 2
    )
    schema[Refined[Int, Less[2] Or Less[7]]] shouldBe `integer`.withValidation(
      `exclusiveMaximum` := 7
    )

    // bounded
    schema[Refined[Int, GreaterEqual[2] Or LessEqual[7]]] shouldBe `integer`
    schema[Refined[Int, GreaterEqual[2] Or Less[7]]] shouldBe `integer`
    schema[Refined[Int, Greater[2] Or LessEqual[7]]] shouldBe `integer`
    schema[Refined[Int, Greater[2] Or Less[7]]] shouldBe `integer`

    schema[Refined[Int, Less[2] Or Greater[7]]] shouldBe `not`(
      `integer`.withValidation(`exclusiveMinimum` := 2, `exclusiveMaximum` := 7)
    )
    schema[Refined[Int, Less[7] Or Greater[2]]] shouldBe `integer`
  }
}
