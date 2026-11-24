package com.github.andyglow.jsonschema.errors

import com.github.andyglow.jsonschema.harness.CompileErrors._
import json._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

object ErrorMessagesSpec {
  case object Lonely
  case class HasLonely(o: Lonely.type)
  class Plain(val x: Int)
  case class HasPlain(p: Plain)
  case class HasNull(x: Null)
  case class HasBoxed(x: java.lang.Integer)
  case class HasBoxedLong(x: java.lang.Long)
  sealed trait Shape
  case class Circle(r: Int) extends Shape
  class Odd                 extends Shape
}

/** Issue #471 / #291 (B11): the derivation's compile-error texts. Step 1 (repro): properties every fix must have, whatever the wording. */
class ErrorMessagesSpec extends AnyFunSuite {
  import ErrorMessagesSpec._

  private val M = "com.github.andyglow.jsonschema.errors.ErrorMessagesSpec."

  private def noCrash(err: String): Unit = {
    err should not include "exception during macro expansion"
    err should not include "Exception occurred while executing macro expansion"
  }

  test("D1: Null is not reported as a Map") {
    errorOf(assertCompiles("Json.schema[Null]")) should not include "Map"
    errorOf(assertCompiles("Json.schema[Option[Null]]")) should not include "Map"
  }

  test("D2: a field of type Null or Nothing is an error, not a crash") {
    noCrash(errorOf(assertCompiles("Json.schema[HasNull]")))
    noCrash(errorOf(assertCompiles("case class HasNothing(x: Nothing); Json.schema[HasNothing]")))
  }

  test("D2: an implicit Predef[Null] works for a field") {
    outcome(assertCompiles("implicit val np: json.schema.Predef[Null] = json.schema.Predef(json.Schema.`null`); Json.schema[HasNull]")) shouldBe None
  }

  test("D3: boxed Java types are named fully qualified") {
    errorOf(assertCompiles("Json.schema[HasBoxed]")) should include("schema for java.lang.Integer is not supported")
    errorOf(assertCompiles("Json.schema[HasBoxedLong]")) should include("schema for java.lang.Long is not supported")
  }

  test("D4: no trailing separator at the root") {
    errorOf(assertCompiles("Json.schema[Plain]")) should not endWith ", "
    errorOf(assertCompiles("Json.schema[java.lang.Integer]")) should not endWith ", "
  }

  test("D5: the sum-member error has no internal prefix and no `$`") {
    val err = errorOf(assertCompiles("Json.schema[Shape]"))
    err should not include "Supported Type"
    err should not include "$"
    err should include(s"schema for ${M}Shape is not supported")
    err should include(s"${M}Odd")
  }

  test("D6/D7: a singleton type is shown as X.type, with a hint (#291)") {
    errorOf(assertCompiles("Json.schema[Lonely.type]")) should include(s"schema for ${M}Lonely.type is not supported")
    val field = errorOf(assertCompiles("Json.schema[HasLonely]"))
    field should include(s"schema for ${M}Lonely.type is not supported")
    field should include("singleton type")
  }

  test("D8/D9: wildcard type arguments are an error, not a crash or a collection-shape error") {
    noCrash(errorOf(assertCompiles("Json.schema[Map[String, _]]")))
    noCrash(errorOf(assertCompiles("Json.schema[Map[_, Int]]")))
    errorOf(assertCompiles("Json.schema[Set[_]]")) should not include "a collection must take its element type"
  }
}
