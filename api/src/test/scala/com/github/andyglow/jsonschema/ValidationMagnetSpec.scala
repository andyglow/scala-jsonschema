package com.github.andyglow.jsonschema

import json._
import json.schema.validation.Instance._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

/** `withValidation` on collection schemas, without type arguments, on every Scala version. Scala 3.3 used to reject List, Vector and Set ("Ambiguous given instances: listMagnet vs iterableMagnet"):
  * `withValidation`'s `TT >: T` is still open when the magnet is searched, so `iterableMagnet` matched too.
  */
class ValidationMagnetSpec extends AnyFunSuite {

  private val items = Seq(`minItems` := 1, `maxItems` := 3, `uniqueItems` := true)

  test("List") {
    Json.schema[List[String]].withValidation(`minItems` := 1).validations shouldEqual Seq(`minItems` := 1)
    Json.schema[List[String]].withValidation(`minItems` := 1, `maxItems` := 3, `uniqueItems` := true).validations shouldEqual items
  }

  test("Vector") {
    Json.schema[Vector[Int]].withValidation(`minItems` := 1, `maxItems` := 3, `uniqueItems` := true).validations shouldEqual items
  }

  test("Set") {
    Json.schema[Set[String]].withValidation(`minItems` := 1, `maxItems` := 3, `uniqueItems` := true).validations shouldEqual items
  }

  test("Seq") {
    Json.schema[Seq[String]].withValidation(`minItems` := 1, `maxItems` := 3, `uniqueItems` := true).validations shouldEqual items
  }

  test("Iterable") {
    Json.schema[Iterable[String]].withValidation(`minItems` := 1, `maxItems` := 3, `uniqueItems` := true).validations shouldEqual items
  }

  test("Array") {
    Json.schema[Array[String]].withValidation(`minItems` := 1, `maxItems` := 3, `uniqueItems` := true).validations shouldEqual items
  }

  test("Map") {
    val props = Seq(`minProperties` := 1, `maxProperties` := 3)
    Json.schema[Map[String, Int]].withValidation(`minProperties` := 1, `maxProperties` := 3).validations shouldEqual props
    // an Int key already has its key pattern; withValidation appends
    Json.schema[Map[Int, String]].withValidation(`minProperties` := 1, `maxProperties` := 3).validations shouldEqual (`patternProperties` := "^-?[0-9]+$") +: props
  }
}
