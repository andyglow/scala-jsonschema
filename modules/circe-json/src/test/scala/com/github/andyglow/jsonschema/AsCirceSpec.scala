package com.github.andyglow.jsonschema

import org.scalatest.prop.TableDrivenPropertyChecks._
import com.github.andyglow.json.Value._
import com.github.andyglow.jsonschema.model.UserProfile
import io.circe._
import json.schema.Version.Draft04
import org.scalactic.Equality
import org.scalatest.matchers.should.Matchers._
import org.scalatest.propspec.AnyPropSpec

class AsCirceSpec extends AnyPropSpec {
  import AsCirceSpec._
  import UserProfileJson._

  private val examples = Table(
    ("json"                           , "Circe"),
    (`null`                           , Json.Null),
    (`true`                           , Json.True),
    (`false`                          , Json.False),
    (str("foo")                       , Json.fromString("foo")),
    (num(4)                           , Json.fromInt(4)),
    (num(4.78)                        , Json.fromDoubleOrNull(4.78)),
    (arr(1, 2, 3)                     , Json.arr(Json.fromInt(1), Json.fromInt(2), Json.fromInt(3))),
    (obj("foo" -> "foo", "bar" -> 15) , Json.obj("foo" -> Json.fromString("foo"), "bar" -> Json.fromInt(15)))
  )

  property("Check that AsCirce translates internal representation of json to Circe") {
    forAll(examples) { (internal, circe) => AsCirce(internal) shouldEqual circe }
  }

  property("AsCirce escapes") {
    AsCirce(obj(""""quoted-key"""" -> "\n\t'val\"")).noSpaces shouldBe """{"\"quoted-key\"":"\n\t'val\""}"""
  }

  property("Check Schema.asCirce") {
    import AsCirce._

    json.Json.schema[UserProfile].asCirce(Draft04()) shouldEqual Json.obj(
      f"$$schema"             -> Json.fromString("http://json-schema.org/draft-04/schema#"),
      "type"                  -> Json.fromString("object"),
      "additionalProperties"  -> Json.False,
      "properties"            -> Json.obj(
        "firstName"               -> Json.obj("type" -> Json.fromString("string")),
        "middleName"              -> Json.obj("type" -> Json.fromString("string")),
        "lastName"                -> Json.obj("type" -> Json.fromString("string")),
        "age"                     -> Json.obj("type" -> Json.fromString("integer")),
        "lastLoginMs"             -> Json.obj("type" -> Json.fromString("number")),
        "role"                    -> Json.obj("type" -> Json.fromString("string"), "default" -> Json.fromString("e-user"), "enum" -> Json.arr(Json.fromString("e-admin"), Json.fromString("e-manager"), Json.fromString("e-user"))),
        "active"                  -> Json.obj("type" -> Json.fromString("string"), "default" -> Json.fromString("On"), "enum" -> Json.arr(Json.fromString("On"), Json.fromString("Off"), Json.fromString("Suspended"))),
        "enabledFeatures"         -> Json.obj("type" -> Json.fromString("array"), "uniqueItems" -> Json.True, "default" -> Json.arr(Json.fromString("feature-0-name"), Json.fromString("feature-1-name")), "items" -> Json.obj("type" -> Json.fromString("string"), "enum" -> Json.arr(Json.fromString("feature-0-name"), Json.fromString("feature-1-name"), Json.fromString("feature-2-name")))),
        "credentials"             -> Json.obj("type" -> Json.fromString("object"),
                                              "additionalProperties" -> Json.False,
                                              "required"   -> Json.arr(Json.fromString("login"), Json.fromString("password")),
                                              "properties" -> Json.obj(
                                                "login"         -> Json.obj("type" -> Json.fromString("string")),
                                                "password"      -> Json.obj("type" -> Json.fromString("string"))),
                                              "default" -> Json.obj("login" -> Json.fromString("anonymous"), "password" -> Json.fromString("-"))),
        "notes"                  -> Json.obj("type" -> Json.fromString("object"),
                                              "additionalProperties" -> Json.False,
                                              "required"   -> Json.arr(Json.fromString("head"), Json.fromString("tail")),
                                              "properties" -> Json.obj(
                                                "head"          -> Json.obj("type" -> Json.fromString("string")),
                                                "tail"          -> Json.obj("type" -> Json.fromString("array"), "items" -> Json.obj("type" -> Json.fromString("string")))),
                                              "default" -> Json.obj("head" -> Json.fromString("initial note"), "tail" -> Json.arr()))),
      "required"              -> Json.arr(Json.fromString("age"), Json.fromString("lastName"), Json.fromString("firstName")))
  }
}

object AsCirceSpec {

  implicit val jsValEq: Equality[Json] = new Equality[Json] {
    override def areEqual(a: Json, b: Any): Boolean = a match {
      case Json.Null                 => b == Json.Null
      case Json.True                 => b == Json.True
      case Json.False                => b == Json.False
      case _ if b.isInstanceOf[Json] =>
        val bb = b.asInstanceOf[Json]
        if (a.isNumber && bb.isNumber) a.asNumber == bb.asNumber
        else if (a.isString && bb.isString) a.asString == bb.asString
        else if (a.isArray && bb.isArray) {
          val r = for {
            aa <- a.asArray
            bb <- bb.asArray
          } yield {
            aa forall { aa =>
              bb exists { bb =>
                jsValEq.areEqual(aa, bb)
              }
            }
          }

          r getOrElse false
        } else if (a.isObject && bb.isObject) {
          val r = for {
            aa <- a.asObject
            bb <- bb.asObject
          } yield {
            val am = aa.toMap
            val bm = bb.toMap
            val keys = am.keySet ++ bm.keySet
            keys.foldLeft(true) {
              case (true, k)  =>
                val r = for {
                  a <- am.get(k)
                  b <- bm.get(k)
                } yield {
                  jsValEq.areEqual(a, b)
                }

                r getOrElse false
              case (false, _) => false
            }
          }

          r getOrElse false
        } else
          false
      case _                         => false
    }
  }
}