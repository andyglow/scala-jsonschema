package com.github.andyglow.jsonschema

import json._
import org.scalatest.funsuite.AnyFunSuite

object BoxedBigMathSpec {
  case class Boxed(
    i: java.lang.Integer,
    l: java.lang.Long,
    d: java.lang.Double,
    f: java.lang.Float,
    s: java.lang.Short,
    b: java.lang.Byte,
    z: java.lang.Boolean
  )
  case class BigMath(d: java.math.BigDecimal, i: java.math.BigInteger)
  case class BoxedDefault(i: java.lang.Integer = 5)
}

/** E4: boxed `java.lang` numbers/Boolean and `java.math.BigDecimal`/`BigInteger` (repro: none of them has a Predef). */
class BoxedBigMathSpec extends AnyFunSuite {

  test("repro: java.lang.Integer")(assertCompiles("Json.schema[java.lang.Integer]"))
  test("repro: java.lang.Long")(assertCompiles("Json.schema[java.lang.Long]"))
  test("repro: java.lang.Double")(assertCompiles("Json.schema[java.lang.Double]"))
  test("repro: java.lang.Float")(assertCompiles("Json.schema[java.lang.Float]"))
  test("repro: java.lang.Short")(assertCompiles("Json.schema[java.lang.Short]"))
  test("repro: java.lang.Byte")(assertCompiles("Json.schema[java.lang.Byte]"))
  test("repro: java.lang.Boolean")(assertCompiles("Json.schema[java.lang.Boolean]"))
  test("repro: java.math.BigDecimal")(assertCompiles("Json.schema[java.math.BigDecimal]"))
  test("repro: java.math.BigInteger")(assertCompiles("Json.schema[java.math.BigInteger]"))
  test("repro: boxed fields")(assertCompiles("Json.schema[BoxedBigMathSpec.Boxed]"))
  test("repro: java.math fields")(assertCompiles("Json.schema[BoxedBigMathSpec.BigMath]"))
  test("repro: a boxed field with a default")(assertCompiles("Json.schema[BoxedBigMathSpec.BoxedDefault]"))
  test("repro: Map with a boxed key")(assertCompiles("Json.schema[Map[java.lang.Integer, String]]"))
  test("repro: withValidation on a boxed number") {
    assertCompiles("Json.schema[java.lang.Integer].withValidation(json.schema.validation.Instance.`minimum` := 1)")
  }
}
