package com.github.andyglow.jsonschema


import json._
import json.Schema._
import json.Schema.`object`._

import com.github.andyglow.json.Value._

import json.schema.Version
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec._

class DefinitionsSpec extends AnyWordSpec {
  import DefinitionsSpec._

  "definitions" should {

    "be exposed out of value-classes (type signatures)" in {
      implicit val idS   = Json.schema[UserId]
      implicit val nameS = Json.schema[UserName]

      val userS = Json.schema[User]

      userS shouldBe `object`(
        Field("id"  , `ref`[UserId]("com.github.andyglow.jsonschema.DefinitionsSpec.UserId", `integer`)),
        Field("name", `ref`[UserName]("com.github.andyglow.jsonschema.DefinitionsSpec.UserName", `string`())))

      AsValue.schema(userS, Version.Draft07(id = "users")) shouldBe obj(
        f"$$schema" -> "http://json-schema.org/draft-07/schema#",
        f"$$id" -> "users",
        "additionalProperties" -> false,
        "type" -> "object",
        "properties" -> obj(
          "id"   -> obj(f"$$ref" -> "#com.github.andyglow.jsonschema.DefinitionsSpec.UserId"),
          "name" -> obj(f"$$ref" -> "#com.github.andyglow.jsonschema.DefinitionsSpec.UserName")),
        "required" -> arr("id", "name"),
        "definitions" -> obj(
          "com.github.andyglow.jsonschema.DefinitionsSpec.UserId" -> obj(
            f"$$id" -> "#com.github.andyglow.jsonschema.DefinitionsSpec.UserId",
            "type" -> "integer"),
          "com.github.andyglow.jsonschema.DefinitionsSpec.UserName" -> obj(
            f"$$id" -> "#com.github.andyglow.jsonschema.DefinitionsSpec.UserName",
            "type" -> "string")))
    }

    "be exposed out of value-classes (custom names used)" in {
      implicit val idS   = Json.schema[UserId]("user-id")
      implicit val nameS = Json.schema[UserName]("user-name")

      val userS = Json.schema[User]

      userS shouldBe `object`(
        Field("id"  , `ref`[UserId]("user-id", `integer`)),
        Field("name", `ref`[UserName]("user-name", `string`())))

      AsValue.schema(userS, Version.Draft07(id = "users")) shouldBe obj(
        f"$$schema" -> "http://json-schema.org/draft-07/schema#",
        f"$$id" -> "users",
        "additionalProperties" -> false,
        "type" -> "object",
        "properties" -> obj(
          "id"   -> obj(f"$$ref" -> "#user-id"),
          "name" -> obj(f"$$ref" -> "#user-name")),
        "required" -> arr("id", "name"),
        "definitions" -> obj(
          "user-id" -> obj(
            f"$$id" -> "#user-id",
            "type" -> "integer"),
          "user-name" -> obj(
            f"$$id" -> "#user-name",
            "type" -> "string")))
    }

    "be exposed out of value-classes (type signatures, reverse order)" in {
      implicit val nameS = Json.schema[UserName]
      implicit val idS   = Json.schema[UserId]

      val userS = Json.schema[User]

      userS shouldBe `object`(
        Field("id"  , `ref`[UserId]("com.github.andyglow.jsonschema.DefinitionsSpec.UserId", `integer`)),
        Field("name", `ref`[UserName]("com.github.andyglow.jsonschema.DefinitionsSpec.UserName", `string`())))

      AsValue.schema(userS, Version.Draft07(id = "users")) shouldBe obj(
        f"$$schema" -> "http://json-schema.org/draft-07/schema#",
        f"$$id" -> "users",
        "additionalProperties" -> false,
        "type" -> "object",
        "properties" -> obj(
          "id"   -> obj(f"$$ref" -> "#com.github.andyglow.jsonschema.DefinitionsSpec.UserId"),
          "name" -> obj(f"$$ref" -> "#com.github.andyglow.jsonschema.DefinitionsSpec.UserName")),
        "required" -> arr("id", "name"),
        "definitions" -> obj(
          "com.github.andyglow.jsonschema.DefinitionsSpec.UserId" -> obj(
            f"$$id" -> "#com.github.andyglow.jsonschema.DefinitionsSpec.UserId",
            "type" -> "integer"),
          "com.github.andyglow.jsonschema.DefinitionsSpec.UserName" -> obj(
            f"$$id" -> "#com.github.andyglow.jsonschema.DefinitionsSpec.UserName",
            "type" -> "string")))
    }

    "be exposed out of value-classes (custom names used, reverse order)" in {
      implicit val nameS = Json.schema[UserName]("user-name")
      implicit val idS   = Json.schema[UserId]("user-id")

      val userS = Json.schema[User]

      userS shouldBe `object`(
        Field("id"  , `ref`[UserId]("user-id", `integer`)),
        Field("name", `ref`[UserName]("user-name", `string`())))

      AsValue.schema(userS, Version.Draft07(id = "users")) shouldBe obj(
        f"$$schema" -> "http://json-schema.org/draft-07/schema#",
        f"$$id" -> "users",
        "additionalProperties" -> false,
        "type" -> "object",
        "properties" -> obj(
          "id"   -> obj(f"$$ref" -> "#user-id"),
          "name" -> obj(f"$$ref" -> "#user-name")),
        "required" -> arr("id", "name"),
        "definitions" -> obj(
          "user-id" -> obj(
            f"$$id" -> "#user-id",
            "type" -> "integer"),
          "user-name" -> obj(
            f"$$id" -> "#user-name",
            "type" -> "string")))
    }
  }
}

object DefinitionsSpec {

  case class UserId(value: Int) extends AnyVal

  case class UserName(value: String) extends AnyVal

  case class User(id: UserId, name: UserName)
}