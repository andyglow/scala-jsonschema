package com.github.andyglow.jsonschema.refined

import eu.timepit.refined.api._
import eu.timepit.refined.boolean._
import eu.timepit.refined.collection._
import json.Json.schema
import json.Schema._
import json.schema.validation.Instance._
import org.scalatest.matchers.should.Matchers._
import org.scalatest.funsuite._

import com.github.andyglow.jsonschema.RefinedSupport._

// The expected values name the magnet's types (`withValidation[List[Int], Iterable[_]]`): on Scala 3 `listMagnet` and `iterableMagnet` are ambiguous otherwise
class RefinedCollectionsSpec extends AnyFunSuite {

  test("size") {
    schema[List[Int] Refined Size[12]] shouldBe `array`[Int, List](`integer`)
      .withValidation[List[Int], Iterable[_]](`minItems` := 12, `maxItems` := 12)
  }

  test("minSize") {
    schema[List[Int] Refined MinSize[12]] shouldBe `array`[Int, List](`integer`)
      .withValidation[List[Int], Iterable[_]](`minItems` := 12)
  }

  test("maxSize") {
    schema[List[Int] Refined MaxSize[12]] shouldBe `array`[Int, List](`integer`)
      .withValidation[List[Int], Iterable[_]](`maxItems` := 12)
  }

  test("empty") {
    schema[List[Int] Refined Empty] shouldBe `array`[Int, List](`integer`)
      .withValidation[List[Int], Iterable[_]](`minItems` := 0, `maxItems` := 0)
  }

  test("non-empty") {
    schema[List[Int] Refined Not[Empty]] shouldBe `array`[Int, List](`integer`).withValidation[List[Int], Iterable[_]](
      `minItems` := 1
    )
    schema[List[Int] Refined NonEmpty] shouldBe `array`[Int, List](`integer`).withValidation[List[Int], Iterable[_]](
      `minItems` := 1
    )
  }
}
