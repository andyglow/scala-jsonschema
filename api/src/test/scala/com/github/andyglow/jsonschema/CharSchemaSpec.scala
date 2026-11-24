package com.github.andyglow.jsonschema

import json._
import json.schema.JsonSchemaProfile
import org.scalatest.funsuite.AnyFunSuite
import petshop.Acceptance

/** `Char` and `java.lang.Character` (core Predefs): a string of exactly one character. Validated with the acceptance harness' JSON Schema validator on draft-04, draft-07 and 2020-12 (JVM; canceled on
  * Scala.js), under Neutral and Legacy.
  */
class CharSchemaSpec extends AnyFunSuite {
  import CharSchemaSpec._

  private val profiles = List(JsonSchemaProfile.Neutral, JsonSchemaProfile.Legacy)

  /** Every character a `Char` can hold as a JSON document, including whitespace, a line break and a non-ASCII letter. */
  private val chars = List("\"a\"", "\"Z\"", "\"0\"", "\" \"", "\"\\t\"", "\"\\n\"", "\".\"", "\"é\"")

  /** Not one character. */
  private val notChars = List("\"\"", "\"ab\"", "1", "null")

  for (p <- profiles) {
    implicit val profile: JsonSchemaProfile = p

    test(s"${p.name}: Char accepts any single character") {
      Acceptance.assertValid(Acceptance.rendered(Json.schema[Char]), chars)
    }

    test(s"${p.name}: java.lang.Character accepts any single character") {
      Acceptance.assertValid(Acceptance.rendered(Json.schema[Character]), chars)
    }

    test(s"${p.name}: Char rejects an empty string, two characters and a non-string") {
      Acceptance.assertInvalid(Acceptance.rendered(Json.schema[Char]), notChars)
    }

    test(s"${p.name}: a Char field accepts any single character") {
      Acceptance.assertValid(Acceptance.rendered(Json.schema[HasChar]), chars.map(c => s"""{"c":$c,"jc":$c}"""))
    }
  }
}

object CharSchemaSpec {
  case class HasChar(c: Char, jc: Character)
}
