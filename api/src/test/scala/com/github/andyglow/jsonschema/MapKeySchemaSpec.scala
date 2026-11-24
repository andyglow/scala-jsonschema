package com.github.andyglow.jsonschema

import json._
import json.schema.JsonSchemaProfile
import org.scalatest.funsuite.AnyFunSuite
import petshop.Acceptance

/** `Map[K, V]` keys (item B6): the key pattern must accept every key a JSON library writes for `K` (`toString`: negative numbers included) and reject the rest. The validating tests use the
  * acceptance harness' JSON Schema validator on draft-04, draft-07 and 2020-12 (JVM; canceled on Scala.js), under Neutral and Legacy.
  */
class MapKeySchemaSpec extends AnyFunSuite {
  import MapKeySchemaSpec._

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
  }

  test("UUID keys have a built-in KeyPattern") {
    assertCompiles("json.Json.schema[Map[java.util.UUID, String]]")
  }

  test("Boolean keys have a built-in KeyPattern") {
    assertCompiles("json.Json.schema[Map[Boolean, String]]")
  }
}

object MapKeySchemaSpec {
  sealed trait Color
  object Color {
    case object Red   extends Color
    case object Green extends Color
  }

  case class IntId(value: Int) extends AnyVal
  case class IntIdKeys(m: Map[IntId, String])
}
