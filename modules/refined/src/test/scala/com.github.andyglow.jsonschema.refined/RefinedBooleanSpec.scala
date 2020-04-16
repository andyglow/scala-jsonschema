package com.github.andyglow.jsonschema.refined

import eu.timepit.refined._
import eu.timepit.refined.api._
import eu.timepit.refined.numeric._
import eu.timepit.refined.string._
import eu.timepit.refined.boolean._
import json.Json.schema
import json.Schema._
import json.Validation._
import org.scalatest.Matchers.{ not => _, _ }
import org.scalatest._

import com.github.andyglow.jsonschema.RefinedSupport._


class RefinedBooleanSpec extends FunSuite {

  test("not") {
    schema[Refined[Int, Not[GreaterEqual[W.`2`.T]]]] shouldBe `number`[Int].withValidation(`exclusiveMaximum` := 2)
    schema[Refined[Int, Not[Divisible[W.`2`.T]]]] shouldBe `not`[Int](`number`[Int].withValidation(`multipleOf` := 2))
    schema[Refined[Int, Not[Not[Divisible[W.`2`.T]]]]] shouldBe `number`[Int].withValidation(`multipleOf` := 2)
  }

  test("and") {
    // normalized
    schema[Refined[Int, Greater[W.`2`.T] And Greater[W.`7`.T]]] shouldBe `number`[Int].withValidation(`exclusiveMinimum` := 7)
    schema[Refined[Int, Less[W.`2`.T] And Less[W.`7`.T]]] shouldBe `number`[Int].withValidation(`exclusiveMaximum` := 2)

    // bounded
    schema[Refined[Int, GreaterEqual[W.`2`.T] And LessEqual[W.`7`.T]]] shouldBe `number`[Int].withValidation(`minimum` := 2, `maximum` := 7)
    schema[Refined[Int, GreaterEqual[W.`2`.T] And Less[W.`7`.T]]] shouldBe `number`[Int].withValidation(`minimum` := 2, `exclusiveMaximum` := 7)
    schema[Refined[Int, Greater[W.`2`.T] And LessEqual[W.`7`.T]]] shouldBe `number`[Int].withValidation(`exclusiveMinimum` := 2, `maximum` := 7)
    schema[Refined[Int, Greater[W.`2`.T] And Less[W.`7`.T]]] shouldBe `number`[Int].withValidation(`exclusiveMinimum` := 2, `exclusiveMaximum` := 7)

    schema[Refined[Int, Less[W.`7`.T] And Greater[W.`2`.T]]] shouldBe `number`[Int].withValidation(`exclusiveMinimum` := 2, `exclusiveMaximum` := 7)

    // somehow scalatest doesn't catch a compile error in here, so commenting it out for now
    // "schema[Refined[Int, Less[W.`2`.T] And Greater[W.`7`.T]]]" shouldNot compile
  }

  test("or") {
    // normalized
    schema[Refined[Int, Greater[W.`2`.T] Or Greater[W.`7`.T]]] shouldBe `number`[Int].withValidation(`exclusiveMinimum` := 2)
    schema[Refined[Int, Less[W.`2`.T] Or Less[W.`7`.T]]] shouldBe `number`[Int].withValidation(`exclusiveMaximum` := 7)

    // bounded
    schema[Refined[Int, GreaterEqual[W.`2`.T] Or LessEqual[W.`7`.T]]] shouldBe `number`[Int]
    schema[Refined[Int, GreaterEqual[W.`2`.T] Or Less[W.`7`.T]]] shouldBe `number`[Int]
    schema[Refined[Int, Greater[W.`2`.T] Or LessEqual[W.`7`.T]]] shouldBe `number`[Int]
    schema[Refined[Int, Greater[W.`2`.T] Or Less[W.`7`.T]]] shouldBe `number`[Int]

    schema[Refined[Int, Less[W.`2`.T] Or Greater[W.`7`.T]]] shouldBe `not`(`number`[Int].withValidation(`minimum` := 2, `maximum` := 7))
    schema[Refined[Int, Less[W.`7`.T] Or Greater[W.`2`.T]]] shouldBe `number`[Int]
  }
}
