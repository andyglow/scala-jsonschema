package com.github.andyglow.jsonschema

import com.github.andyglow.json.Value._
import com.github.andyglow.jsonschema.JsonMatchers._
import json.Schema._
import json.Schema.`object`.{Field => F}
import json.schema.{Version => v, _}
import json.Json
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec._


// moved on top because of
// - knownDirectSubclasses of Pet observed before subclass Cat registered
object RWSpec {

  case class Foo(
    @readOnly id: String,
    name: String,
    @writeOnly password: String)

}

class RWSpec extends AnyWordSpec {
  import RWSpec._

  "readOnly and writeOnly" should {

    "be exposed" in {

      val s = Json.schema[Foo]
      s shouldBe `object`(
        F("id", `string`, required = true).setReadOnly,
        F("name", `string`, required = true),
        F("password", `string`, required = true).setWriteOnly)

      AsValue.schema(s, v.Raw) should containJson {
        obj(
          "type"                 -> "object",
          "additionalProperties" -> false,
          "required"             -> arr("id", "name", "password"),
          "properties"           -> obj(
            "id"                   -> obj("type" -> "string", "readOnly" -> true),
            "name"                 -> obj("type" -> "string"),
            "password"             -> obj("type" -> "string", "writeOnly" -> true)))
      }
    }
  }
}

