package com.github.andyglow.jsonschema

import com.github.andyglow.json.Value._
import json._
import json.Schema._
import json.schema.JsonSchemaProfile
import json.schema.validation.Instance._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._
import petshop.Acceptance

/** `Char` and `java.lang.Character` (core Predefs): a string of exactly one character, `minLength` and `maxLength` 1 and no `pattern` (#463, design doc `docs/design/463-char-pattern.md`). The tree
  * and rendering tests run on both platforms; the validating ones use the acceptance harness' JSON Schema validator on draft-04, draft-07 and 2020-12 (JVM; canceled on Scala.js), under Neutral and
  * Legacy.
  */
class CharSchemaSpec extends AnyFunSuite {
  import CharSchemaSpec._
  import ProfileSpec.{body, drafts, prop, render}

  private val profiles = List(JsonSchemaProfile.Neutral, JsonSchemaProfile.Legacy)

  /** Characters a `Char` can hold, as JSON documents: letters, digits, whitespace, a line break, a dot, non-ASCII letters (raw and escaped) and a lone surrogate. */
  private val chars = List("\"a\"", "\"Z\"", "\"0\"", "\" \"", "\"\\t\"", "\"\\n\"", "\"\\r\"", "\".\"", "\"é\"", "\"\\u00e9\"", "\"\\ud83d\"")

  /** Not one character. */
  private val notChars = List("\"\"", "\"ab\"", "\"a \"", "1", "null")

  /** U+1F600, outside the BMP: two UTF-16 code units, so no `Char` holds it, but one code point, which is what JSON Schema lengths count. */
  private val nonBmp = "\"\\ud83d\\ude00\""

  private val oneChar = `string`.withValidation(`minLength` := 1, `maxLength` := 1)

  test("the Char and java.lang.Character Predefs: minLength 1, maxLength 1, no pattern") {
    Json.schema[Char] shouldBe oneChar
    Json.schema[Character] shouldBe oneChar
    Json.schema[HasChar] shouldBe `object`(`object`.Field("c", oneChar), `object`.Field("jc", oneChar))
  }

  for (p <- profiles) {
    implicit val profile: JsonSchemaProfile = p

    test(s"${p.name}: rendered on every draft as a string of length 1") {
      val expected = obj("type" -> "string", "minLength" -> 1, "maxLength" -> 1)
      drafts foreach { v =>
        body(render(Json.schema[Char], v)) shouldBe expected
        body(render(Json.schema[Character], v)) shouldBe expected
        prop(render(Json.schema[HasChar], v), "c") shouldBe expected
        prop(render(Json.schema[HasChar], v), "jc") shouldBe expected
      }
    }

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

    test(s"${p.name}: a non-BMP character is accepted too (one code point; documented trade-off of #463)") {
      Acceptance.assertValid(Acceptance.rendered(Json.schema[Char]), List(nonBmp))
    }
  }
}

object CharSchemaSpec {
  case class HasChar(c: Char, jc: Character)
}
