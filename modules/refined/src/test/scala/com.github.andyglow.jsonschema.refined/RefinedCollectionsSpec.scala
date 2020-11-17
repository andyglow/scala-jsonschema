package com.github.andyglow.jsonschema.refined

import eu.timepit.refined.api._
import eu.timepit.refined.boolean._
import eu.timepit.refined.collection._
import eu.timepit.refined._
import json.Json.schema
import json.Schema._
import json.schema.validation.Instance._
import org.scalatest.matchers.should.Matchers._
import org.scalatest.funsuite._

import com.github.andyglow.jsonschema.RefinedSupport._


class RefinedCollectionsSpec extends AnyFunSuite {

  test("size") {
    schema[List[Int] Refined Size[W.`12`.T]] shouldBe `array`[Int, List](`integer`).withValidation( `minItems` := 12, `maxItems` := 12 )
  }

  test("minSize") {
    schema[List[Int] Refined MinSize[W.`12`.T]] shouldBe `array`[Int, List](`integer`).withValidation( `minItems` := 12 )
  }

  test("maxSize") {
    schema[List[Int] Refined MaxSize[W.`12`.T]] shouldBe `array`[Int, List](`integer`).withValidation( `maxItems` := 12 )
  }

  test("empty") {
    schema[List[Int] Refined Empty] shouldBe `array`[Int, List](`integer`).withValidation( `minItems` := 0, `maxItems` := 0 )
  }

  test("non-empty") {
    schema[List[Int] Refined Not[Empty]] shouldBe `array`[Int, List](`integer`).withValidation( `minItems` := 1 )
    schema[List[Int] Refined NonEmpty] shouldBe `array`[Int, List](`integer`).withValidation( `minItems` := 1 )
  }
}
