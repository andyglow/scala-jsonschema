package com.github.andyglow.jsonschema

import java.util.UUID

import com.github.andyglow.json.Value._
import json._
import json.Schema._
import json.Schema.`dictionary`.KeyPattern
import json.schema.{JsonSchemaProfile, Predef, Version}
import json.schema.validation.Instance._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._
import petshop.Acceptance

/** `Map[K, V]` keys (item B6): the key pattern must accept every key a JSON library writes for `K` (`toString`: negative numbers included) and reject the rest. The validating tests use the acceptance
  * harness' JSON Schema validator on draft-04, draft-07 and 2020-12 (JVM; canceled on Scala.js), under Neutral and Legacy.
  */
class MapKeySchemaSpec extends AnyFunSuite {
  import MapKeySchemaSpec._
  import ProfileSpec.{body, drafts, render}

  private val profiles = List(JsonSchemaProfile.Neutral, JsonSchemaProfile.Legacy)

  for (p <- profiles) {
    implicit val profile: JsonSchemaProfile = p

    test(s"${p.name}: integral keys accept negative numbers") {
      Acceptance.assertValid(Acceptance.rendered(Json.schema[Map[Int, String]]), List("""{"0":"a","42":"b","-1":"c","-2147483648":"d"}"""))
      Acceptance.assertValid(Acceptance.rendered(Json.schema[Map[Long, String]]), List("""{"-9223372036854775808":"a"}"""))
      Acceptance.assertValid(Acceptance.rendered(Json.schema[Map[Short, String]]), List("""{"-1":"a"}"""))
      Acceptance.assertValid(Acceptance.rendered(Json.schema[Map[Byte, String]]), List("""{"-128":"a"}"""))
      Acceptance.assertValid(Acceptance.rendered(Json.schema[IntIdKeys]), List("""{"m":{"-1":"a"}}"""))
    }

    // today a negative key is accepted only because it matches no pattern, so its value isn't checked at all
    test(s"${p.name}: the value under a negative key is validated") {
      Acceptance.assertInvalid(Acceptance.rendered(Json.schema[Map[Int, String]]), List("""{"-1":5}"""))
      Acceptance.assertInvalid(Acceptance.rendered(Json.schema[Map[Long, String]]), List("""{"-1":5}"""))
    }

    test(s"${p.name}: integral keys reject keys that aren't numbers") {
      Acceptance.assertInvalid(Acceptance.rendered(Json.schema[Map[Int, String]]), List("""{"abc":"a"}""", """{"1.5":"a"}""", """{"":"a"}""", """{"-":"a"}"""))
      Acceptance.assertInvalid(Acceptance.rendered(Json.schema[IntIdKeys]), List("""{"m":{"abc":"a"}}"""))
    }

    test(s"${p.name}: Char keys accept any single character, a line break included") {
      Acceptance.assertValid(Acceptance.rendered(Json.schema[Map[Char, String]]), List("""{"a":"x","\n":"y"," ":"z","\r":"w"}"""))
    }

    test(s"${p.name}: the value under a line-break Char key is validated") {
      Acceptance.assertInvalid(Acceptance.rendered(Json.schema[Map[Char, String]]), List("""{"\n":5}"""))
    }

    test(s"${p.name}: Char keys reject keys of other lengths") {
      Acceptance.assertInvalid(Acceptance.rendered(Json.schema[Map[Char, String]]), List("""{"ab":"x"}""", """{"":"x"}"""))
    }

    test(s"${p.name}: enum keys reject a name that isn't a member") {
      Acceptance.assertValid(Acceptance.rendered(Json.schema[Map[Color, Int]]), List("""{"Red":1,"Green":2}"""))
      Acceptance.assertInvalid(Acceptance.rendered(Json.schema[Map[Color, Int]]), List("""{"Blue":1}"""))
    }

    test(s"${p.name}: String keys accept any key") {
      Acceptance.assertValid(Acceptance.rendered(Json.schema[Map[String, Int]]), List("""{"":1,"a":2,"-1":3,"a\nb":4}"""))
      Acceptance.assertValid(Acceptance.rendered(Json.schema[Map[java.lang.String, Int]]), List("""{"":1,"a":2,"-1":3,"a\nb":4}"""))
    }

    test(s"${p.name}: the value under a String key with a line break is validated") {
      Acceptance.assertInvalid(Acceptance.rendered(Json.schema[Map[String, Int]]), List("""{"a\nb":"x"}"""))
      Acceptance.assertInvalid(Acceptance.rendered(Json.schema[Map[java.lang.String, Int]]), List("""{"a\nb":"x"}"""))
    }

    test(s"${p.name}: UUID keys, either case") {
      Acceptance.assertValid(Acceptance.rendered(Json.schema[Map[UUID, Int]]), List(s"""{"$uuid":1,"${uuid.toUpperCase}":2}"""))
      Acceptance.assertValid(Acceptance.rendered(Json.schema[UuidIdKeys]), List(s"""{"m":{"$uuid":1}}"""))
      Acceptance.assertInvalid(Acceptance.rendered(Json.schema[Map[UUID, Int]]), List("""{"abc":1}""", s"""{"${uuid.replace('-', 'x')}":1}""", s"""{"$uuid":"x"}"""))
      Acceptance.assertInvalid(Acceptance.rendered(Json.schema[UuidIdKeys]), List("""{"m":{"abc":1}}"""))
    }

    test(s"${p.name}: Boolean keys") {
      Acceptance.assertValid(Acceptance.rendered(Json.schema[Map[Boolean, Int]]), List("""{"true":1,"false":2}"""))
      Acceptance.assertInvalid(Acceptance.rendered(Json.schema[Map[Boolean, Int]]), List("""{"yes":1}""", """{"True":1}""", """{"true":"x"}"""))
    }

    test(s"${p.name}: enum keys whose names are regex syntax match themselves only") {
      Acceptance.assertValid(Acceptance.rendered(Json.schema[Map[Size, Int]]), List("""{"N/A":1,"1+":2,"(none)":3,"a.b":4}"""))
      Acceptance.assertInvalid(Acceptance.rendered(Json.schema[Map[Size, Int]]), List("""{"1":1}""", """{"11":1}""", """{"axb":1}""", """{"none":1}"""))
    }
  }

  test("the built-in key patterns") {
    Json.schema[Map[Int, String]] shouldBe `dictionary`[Int, String, Map](`string`).withValidation(`patternProperties` := "^-?[0-9]+$")
    Json.schema[Map[Long, String]] shouldBe `dictionary`[Long, String, Map](`string`).withValidation(`patternProperties` := "^-?[0-9]+$")
    Json.schema[Map[Char, String]] shouldBe `dictionary`[Char, String, Map](`string`).withValidation(`patternProperties` := "^[\\s\\S]$")
    // the macro's java.lang.String key (quirk 13); `Predef.dictS` leaves StringKeyPattern out, so use a derived value
    Json.schema[Map[java.lang.String, Color]].validations shouldBe Seq(`patternProperties` := "^[\\s\\S]*$")
    Json.schema[Map[Boolean, String]] shouldBe `dictionary`[Boolean, String, Map](`string`).withValidation(`patternProperties` := "^(?:true|false)$")
    Json.schema[Map[UUID, String]] shouldBe `dictionary`[UUID, String, Map](`string`).withValidation(
      `patternProperties` := "^[a-fA-F0-9]{8}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{12}$"
    )
  }

  test("enum key names are regex-escaped, in declaration order") {
    Json.schema[Map[Size, Int]] shouldBe `dictionary`[Size, Int, Map](`integer`).withValidation(`patternProperties` := "^(?:N/A|1\\+|\\(none\\)|a\\.b)$")
  }

  test("KeyPattern.escape and forEnum") {
    KeyPattern.escape("""a\b^c$d.e|f?g*h+i(j)k[l]m{n}o-p/q""") shouldBe """a\\b\^c\$d\.e\|f\?g\*h\+i\(j\)k\[l\]m\{n\}o-p/q"""
    KeyPattern.escape("Red") shouldBe "Red"
    KeyPattern.forEnum[Size](List("b+", "a.", "b+")).pattern shouldBe "^(?:a\\.|b\\+)$"
  }

  test("a KeyPattern in lexical scope takes priority over the built-in one") {
    implicit val mine: KeyPattern[UUID] = KeyPattern.mk[UUID]("^[0-9a-f-]{36}$")
    Json.schema[Map[UUID, String]] shouldBe `dictionary`[UUID, String, Map](`string`).withValidation(`patternProperties` := "^[0-9a-f-]{36}$")
    implicitly[Predef[Map[UUID, Int]]].schema shouldBe `dictionary`[UUID, Int, Map](`integer`).withValidation(`patternProperties` := "^[0-9a-f-]{36}$")
  }

  test("Predef: a Map of a Predef'd value with a UUID or Boolean key is a Predef") {
    implicitly[Predef[Map[UUID, Int]]].schema shouldBe Json.schema[Map[UUID, Int]]
    implicitly[Predef[Map[Boolean, Int]]].schema shouldBe Json.schema[Map[Boolean, Int]]
  }

  test("rendered: draft 04 enforces keys with additionalProperties: false, later drafts with propertyNames; any-string keys aren't enforced") {
    implicit val profile: JsonSchemaProfile = JsonSchemaProfile.Neutral
    val value                               = obj("type" -> "string")
    drafts foreach { v =>
      val restriction = if (v.isInstanceOf[Version.Draft04]) obj("additionalProperties" -> false) else obj("propertyNames" -> obj("pattern" -> "^-?[0-9]+$"))
      body(render(Json.schema[Map[Int, String]], v)) shouldBe obj("type" -> "object", "patternProperties" -> obj("^-?[0-9]+$" -> value)) ++ restriction
      body(render(Json.schema[Map[String, String]], v)) shouldBe obj("type" -> "object", "patternProperties" -> obj("^[\\s\\S]*$" -> value))
      body(render(Json.schema[Map[java.lang.String, String]], v)) shouldBe obj("type" -> "object", "patternProperties" -> obj("^[\\s\\S]*$" -> value))
      // ^.*$ from before #466 (a parsed schema, a user KeyPattern) still means any key
      val old = `dictionary`[String, String, Map](`string`).withValidation(`patternProperties` := "^.*$")
      body(render(old, v)) shouldBe obj("type" -> "object", "patternProperties" -> obj("^.*$" -> value))
    }
  }
}

object MapKeySchemaSpec {
  sealed trait Color
  object Color {
    case object Red   extends Color
    case object Green extends Color
  }

  sealed trait Size
  object Size {
    case object `N/A`    extends Size
    case object `1+`     extends Size
    case object `(none)` extends Size
    case object `a.b`    extends Size
  }

  val uuid = "123e4567-e89b-12d3-a456-426614174000"

  case class IntId(value: Int) extends AnyVal
  case class IntIdKeys(m: Map[IntId, String])

  case class UuidId(value: UUID) extends AnyVal
  case class UuidIdKeys(m: Map[UuidId, Int])
}
