package com.github.andyglow.jsonschema.refined

import eu.timepit.refined.api.Refined
import eu.timepit.refined.boolean._
import eu.timepit.refined.collection._
import eu.timepit.refined.numeric._
import json.Json
import json.schema.Predef
import org.scalatest.matchers.should.Matchers._
import org.scalatest.funsuite._
import scala.compiletime.testing.typeCheckErrors

import com.github.andyglow.jsonschema.RefinedSupport._

/** Compile errors of the Scala 3 refined macro (Scala 2's texts).
  *
  * `refined[T]` and `summon[Predef[T]]` report the macro's own message. Through the derivation (`Json.schema[T]`, a field) the derivation's implicit search drops it, so the error is the derivation's
  * `is not supported` (Scala 2 shows the refined macro's message there too).
  */
class Scala3RefinedErrorsSpec extends AnyFunSuite {

  private def errorsOf(errs: List[scala.compiletime.testing.Error]): String = errs.map(_.message).mkString("\n")

  test("an empty range") {
    errorsOf(typeCheckErrors("refined[Int Refined (Less[2] And Greater[7])]")) should include("There are no values inside of specified range [2.0 .. 7.0]")
  }

  test("an unsupported predicate") {
    errorsOf(typeCheckErrors("refined[Int Refined Modulo[2, 3]]")) should include("Can't infer Predicate out of eu.timepit.refined.numeric.Modulo[2, 3]")
    errorsOf(typeCheckErrors("summon[Predef[Int Refined Modulo[2, 3]]]")) should include("Can't infer Predicate out of eu.timepit.refined.numeric.Modulo[2, 3]")
  }

  test("not a refined type") {
    errorsOf(typeCheckErrors("refined[Int]")) should include("Can't infer Refined out of scala.Int")
  }

  test("a validation that doesn't match the base type") {
    errorsOf(typeCheckErrors("refined[Int Refined NonEmpty]")) should include("Implicit not found: validation.Magnet[Int, Iterable[")
  }

  test("through the derivation: `is not supported`") {
    errorsOf(typeCheckErrors("Json.schema[Int Refined Modulo[2, 3]]")) should include("is not supported")
    errorsOf(typeCheckErrors("Json.schema[Int Refined (Less[2] And Greater[7])]")) should include("is not supported")
  }
}
