package com.github.andyglow.jsonschema.refined

import eu.timepit.refined.api._
import eu.timepit.refined.boolean._
import eu.timepit.refined.collection._
import eu.timepit.refined.numeric._
import eu.timepit.refined.string._
import json.Json.schema
import json.Schema._
import json.schema.validation.Instance._
import org.scalatest.matchers.should.Matchers.{not => _, _}
import org.scalatest.funsuite._

import com.github.andyglow.jsonschema.RefinedSupport._

/** Pins the Scala 2 macro's behavior where the original specs don't (inventory appendix C, part 1.1: "Gaps" and "Normalization quirks").
  *
  * The expectations are Scala 2's output; the Scala 3 port reproduces them. Literal types, so 2.13 and 3 only.
  */
class RefinedQuirksSpec extends AnyFunSuite {

  test("NonDivisible") {
    schema[Int Refined NonDivisible[2]] shouldBe `not`[Int](`integer`.withValidation(`multipleOf` := 2))
  }

  test("intervals (reached by dealiasing to And)") {
    schema[Int Refined Interval.Closed[2, 7]] shouldBe `integer`.withValidation(`minimum` := 2, `maximum` := 7)
    schema[Int Refined Interval.Open[2, 7]] shouldBe `integer`.withValidation(`exclusiveMinimum` := 2, `exclusiveMaximum` := 7)
    schema[Int Refined Interval.OpenClosed[2, 7]] shouldBe `integer`.withValidation(`exclusiveMinimum` := 2, `maximum` := 7)
    schema[Int Refined Interval.ClosedOpen[2, 7]] shouldBe `integer`.withValidation(`minimum` := 2, `exclusiveMaximum` := 7)
  }

  test("Short, Byte, BigInt, BigDecimal are `number`") {
    schema[Short Refined Positive] shouldBe `number`[Short].withValidation(`exclusiveMinimum` := 0)
    schema[Byte Refined Negative] shouldBe `number`[Byte].withValidation(`exclusiveMaximum` := 0)
    schema[BigInt Refined NonNegative] shouldBe `number`[BigInt].withValidation(`minimum` := 0)
    schema[BigDecimal Refined NonPositive] shouldBe `number`[BigDecimal].withValidation(`maximum` := 0)
  }

  test("Double bound") {
    schema[Double Refined Greater[1.5]] shouldBe `number`[Double].withValidation(`exclusiveMinimum` := 1.5)
  }

  test("Size[P] ignores P's inclusiveness") {
    schema[String Refined Size[Greater[5]]] shouldBe `string`.withValidation(`minLength` := 5)
    schema[String Refined Size[LessEqual[3]]] shouldBe `string`.withValidation(`maxLength` := 3)
  }

  test("And keeps an inclusive bound when either side is inclusive") {
    schema[Int Refined (Greater[5] And GreaterEqual[3])] shouldBe `integer`.withValidation(`minimum` := 5)
  }

  test("And of equal bounds is an allof") {
    schema[Int Refined (GreaterEqual[5] And LessEqual[5])] shouldBe `allof`[Int](
      Set(`integer`.withValidation(`minimum` := 5), `integer`.withValidation(`maximum` := 5))
    )
  }

  test("And matches its raw operands: a nested Not isn't simplified") {
    schema[Int Refined (Not[Less[2]] And Less[7])] shouldBe `allof`[Int](
      Set(`not`[Int](`integer`.withValidation(`exclusiveMaximum` := 2)), `integer`.withValidation(`exclusiveMaximum` := 7))
    )
  }

  test("A And B And C is a nested allof") {
    schema[Int Refined (Divisible[2] And Divisible[3] And Divisible[5])] shouldBe `allof`[Int](
      Set(
        `allof`[Int](Set(`integer`.withValidation(`multipleOf` := 2), `integer`.withValidation(`multipleOf` := 3))),
        `integer`.withValidation(`multipleOf` := 5)
      )
    )
  }

  test("Or outside, lower bound first: the bounds come out swapped") {
    schema[Int Refined (Greater[7] Or Less[2])] shouldBe `not`[Int](`integer`.withValidation(`exclusiveMinimum` := 7, `exclusiveMaximum` := 2))
  }

  test("Or of string predicates is a oneof") {
    schema[String Refined (StartsWith["a"] Or EndsWith["b"])] shouldBe `oneof`[String](
      Set(`string`.withValidation(`pattern` := "^a.*$"), `string`.withValidation(`pattern` := "^.*b$"))
    )
  }

  test("StartsWith isn't regex-escaped") {
    schema[String Refined StartsWith["a.b"]] shouldBe `string`.withValidation(`pattern` := "^a.b.*$")
  }

  test("Not of a string predicate") {
    schema[String Refined Not[IPv4]] shouldBe `not`[String](`string`(`string`.Format.`ipv4`))
  }

  test("collections other than List") {
    schema[Vector[Int] Refined NonEmpty] shouldBe `array`[Int, Vector](`integer`).withValidation[Vector[Int], Iterable[_]](`minItems` := 1)
    schema[Set[String] Refined MaxSize[3]] shouldBe `array`[String, Set](`string`, unique = true).withValidation[Set[String], Iterable[_]](`maxItems` := 3)
  }

  test("refined[T], the explicit entry point") {
    refined[Int Refined Positive] shouldBe `integer`.withValidation(`exclusiveMinimum` := 0)
    refined[String Refined MatchesRegex["[0-9]+"]] shouldBe `string`.withValidation(`pattern` := "[0-9]+")
  }
}
