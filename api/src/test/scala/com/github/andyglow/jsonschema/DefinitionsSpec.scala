package com.github.andyglow.jsonschema


import json._
import json.Schema._
import json.Schema.`object`._
import com.github.andyglow.json.Value._
import json.schema.Version
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec._
import JsonMatchers._
import json.schema.validation.Magnet

// moved on top because of
// - knownDirectSubclasses of Pet observed before subclass Cat registered
object DefinitionsSpec {

  case class UserId(value: Int) extends AnyVal

  case class UserName(value: String) extends AnyVal

  case class User(id: UserId, name: UserName)

  sealed trait Pet
  object Pet {
    import json.schema._

    @definition final case class Cat(color: String) extends Pet

    @definition("alligator") final case class Alligator(toothLeft: Int) extends Pet
  }
}

class DefinitionsSpec extends AnyWordSpec {
  import DefinitionsSpec._

  "definitions" should {

    "be exposed if specified by annotations" in {
      val petS = Json.schema[Pet]
      petS shouldBe `oneof`.of[Pet](
        `def`[Pet.Cat]("com.github.andyglow.jsonschema.DefinitionsSpec.Pet.Cat", `object`(Field("color", `string`, required = true))),
        `def`[Pet.Alligator]("alligator", `object`(Field("toothLeft", `integer`, required = true))))

      AsValue.schema(petS, Version.Draft07(id = "pets")) should containJson {
        obj(
          f"$$schema" -> "http://json-schema.org/draft-07/schema#",
          f"$$id" -> "pets",
          "oneOf" -> arr(
            obj(f"$$ref" -> "#com.github.andyglow.jsonschema.DefinitionsSpec.Pet.Cat"),
            obj(f"$$ref" -> "#alligator")),
          "definitions" -> obj(
            "com.github.andyglow.jsonschema.DefinitionsSpec.Pet.Cat" -> obj(
              f"$$id" -> "#com.github.andyglow.jsonschema.DefinitionsSpec.Pet.Cat",
              "type" -> "object",
              "properties" -> obj(
                "color" -> obj("type" -> "string")),
              "additionalProperties" -> false,
              "required" -> arr("color")),
            "alligator" -> obj(
              f"$$id" -> "#alligator",
              "type" -> "object",
              "properties" -> obj(
                "toothLeft" -> obj("type" -> "integer")),
              "additionalProperties" -> false,
              "required" -> arr("toothLeft"))))
      }
    }

    "be exposed out of value-classes (type signatures)" in {
      implicit val idS   = Json.schema[UserId]
      implicit val nameS = Json.schema[UserName]

      val userS = Json.schema[User]

      userS shouldBe `object`(
        Field("id"  , `def`[UserId]("com.github.andyglow.jsonschema.DefinitionsSpec.UserId", `integer`)),
        Field("name", `def`[UserName]("com.github.andyglow.jsonschema.DefinitionsSpec.UserName", `string`)))

      AsValue.schema(userS, Version.Draft07(id = "users")) should containJson {
        obj(
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
    }

    "be exposed out of value-classes (custom names used)" in {
      import json.schema.validation.Instance._

      implicit val idS   = Json.schema[UserId] toDefinition "user-id"
      implicit val nameM = Magnet.mk[UserName, String]
      implicit val nameS = Json.schema[UserName] toDefinition "user-name" withValidation (
        `maxLength` := 100
      )

      val userS = Json.schema[User]

      userS shouldBe `object`(
        Field("id"  , `def`[UserId]("user-id", `integer`)),
        Field("name", `def`[UserName]("user-name", `string`.withValidation(`maxLength` := 100))))

      AsValue.schema(userS, Version.Draft07(id = "users")) should beStructurallyEqualTo {
        obj(
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
              "type" -> "string",
              "maxLength" -> 100)))
      }
    }

    "be exposed out of value-classes (type signatures, reverse order)" in {
      implicit val nameS = Json.schema[UserName]
      implicit val idS   = Json.schema[UserId]

      val userS = Json.schema[User]

      userS shouldBe `object`(
        Field("id"  , `def`[UserId]("com.github.andyglow.jsonschema.DefinitionsSpec.UserId", `integer`)),
        Field("name", `def`[UserName]("com.github.andyglow.jsonschema.DefinitionsSpec.UserName", `string`)))

      AsValue.schema(userS, Version.Draft07(id = "users")) should containJson {
        obj(
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
    }

    "be exposed out of value-classes (custom names used, reverse order)" in {
      implicit val nameS = Json.schema[UserName] toDefinition "user-name"
      implicit val idS   = Json.schema[UserId] toDefinition "user-id"

      val userS = Json.schema[User]

      userS shouldBe `object`(
        Field("id"  , `def`[UserId]("user-id", `integer`)),
        Field("name", `def`[UserName]("user-name", `string`)))

      AsValue.schema(userS, Version.Draft07(id = "users")) should containJson {
        obj(
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
}