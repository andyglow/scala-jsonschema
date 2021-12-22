package com.github.andyglow.jsonschema

import com.github.andyglow.json.JsonFormatter
import json.Profile
import json.Json
import json.Schema
import json.Schema._
import json.Schema.`object`.Field
import json.schema.Version
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec._


class ProfileSpec extends AnyWordSpec {
  import ProfileSpec._

  "Macros" should {

    "include profile settings into an account" when {

      "derives schema for object with optional fields" when {

        "default profile is enabled" in {
          val s = Json.schema[OptEx0]
          s shouldBe `object`(
            Field[String]("name", `string`, required = true),
            Field[String]("description", `string`, required = false))
        }

//        "option-as-array is enabled" in {
//          implicit val profile = new Profile with Profile.OptionAsArray
//
//          val s = Json.schema[OptEx0]
//
//          s shouldBe `object`(
//            Field[String]("name", `string`, required = true),
//            Field[String]("description", `string`, required = false))
//        }
      }
    }
  }
}

object ProfileSpec {

  case class OptEx0(name: String, description: Option[String])
}