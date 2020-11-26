package com.github.andyglow.jsonschema

import com.github.andyglow.json.Value
import org.scalatest.prop.TableDrivenPropertyChecks._
import com.github.andyglow.json.Value._
import com.github.andyglow.jsonschema.model.UserProfile
import json.schema.Version.Draft04
import org.scalactic.Equality
import play.api.libs.json._
import org.scalatest.matchers.should.Matchers._
import org.scalatest.propspec.AnyPropSpec

class AsPlaySpec extends AnyPropSpec{
  import AsPlaySpec._
  import UserProfileJson._

  private val examples = Table[Value, JsValue](
    ("json"                           , "PlayJson"),
    (`null`                           , JsNull),
    (`true`                           , JsTrue),
    (`false`                          , JsFalse),
    (str("foo")                       , JsString("foo")),
    (num(4)                           , JsNumber(4)),
    (num(4.78)                        , JsNumber(4.78)),
    (arr(1, 2, 3)                     , Json.arr(1, 2, 3)),
    (obj("foo" -> "foo", "bar" -> 15) , Json.obj("foo" -> "foo", "bar" -> 15))
  )

  property("Check that AsPlay translates internal representation of json to Play Json") {
    forAll(examples) { (internal, play) => AsPlay(internal) shouldEqual play }
  }

  property("AsPlay escapes") {
    val jobj = AsPlay(obj(""""quoted-key"""" -> "\n\t'val\""))
    Json.stringify(jobj) shouldBe """{"\"quoted-key\"":"\n\t'val\""}"""
  }

  property("Check Schema.asPlay") {
    import AsPlay._

    json.Json.schema[UserProfile].asPlay(Draft04()) shouldEqual Json.obj(
      f"$$schema"             -> "http://json-schema.org/draft-04/schema#",
      "type"                  -> "object",
      "additionalProperties"  -> false,
      "properties"            -> Json.obj(
        "firstName"               -> Json.obj("type" -> "string"),
        "middleName"              -> Json.obj("type" -> "string"),
        "lastName"                -> Json.obj("type" -> "string"),
        "age"                     -> Json.obj("type" -> "integer"),
        "lastLoginMs"             -> Json.obj("type" -> "number"),
        "role"                    -> Json.obj("type" -> "string", "default" -> "e-user", "enum" -> Json.arr("e-admin","e-manager","e-user")),
        "active"                  -> Json.obj("type" -> "string", "default" -> "On", "enum" -> Json.arr("On", "Off", "Suspended")),
        "enabledFeatures"         -> Json.obj("type" -> "array", "uniqueItems" -> true, "default" -> Json.arr("feature-0-name", "feature-1-name"), "items" -> Json.obj("type" -> "string", "enum" -> Json.arr("feature-0-name", "feature-1-name", "feature-2-name"))),
        "credentials"             -> Json.obj("type" -> "object",
                                              "additionalProperties" -> false,
                                              "required"   -> Json.arr("login", "password"),
                                              "properties" -> Json.obj(
                                                "login"         -> Json.obj("type" -> "string"),
                                                "password"      -> Json.obj("type" -> "string")),
                                              "default" -> Json.obj("login" -> "anonymous", "password" -> "-")),
        "notes"                   -> Json.obj("type" -> "object",
                                              "additionalProperties" -> false,
                                              "required"   -> Json.arr("head", "tail"),
                                              "properties" -> Json.obj(
                                                "head"         -> Json.obj("type" -> "string"),
                                                "tail"        -> Json.obj("type" -> "array", "items" -> Json.obj("type" -> "string"))),
                                              "default" -> Json.obj("head" -> "initial note", "tail" -> Json.arr()))),
      "required"              -> Json.arr("age", "lastName", "firstName"))
  }

  property("Check Schema.asPlay (+ free notation, as schema/definition") {
    import AsPlay._

    implicit val metaSchema: json.Schema[JsObject] = json.Schema.`object`.Free[JsObject]()

    json.Json.schema[Person].asPlay(Draft04()) shouldEqual Json.obj(
      f"$$schema"             -> "http://json-schema.org/draft-04/schema#",
      "type"                  -> "object",
      "additionalProperties"  -> false,
      "properties"            -> Json.obj(
        "id"       -> Json.obj("type" -> "string"),
        "name"     -> Json.obj("type" -> "string"),
        "metadata" -> Json.obj(f"$$ref" -> "#/definitions/play.api.libs.json.JsObject")),
     "required"   -> Json.arr("id", "name", "metadata"),
     "definitions"            -> Json.obj(
       "play.api.libs.json.JsObject" -> Json.obj(
         "type" -> "object",
         "additionalProperties"  -> true)))
  }

  property("Check Schema.asPlay (+ free notation, predef/inline") {
    import AsPlay._

    implicit val metaSchema: json.schema.Predef[JsObject] = json.schema.Predef(json.Schema.`object`.Free[JsObject]())

    json.Json.schema[Person].asPlay(Draft04()) shouldEqual Json.obj(
      f"$$schema"             -> "http://json-schema.org/draft-04/schema#",
      "type"                  -> "object",
      "additionalProperties"  -> false,
      "properties"            -> Json.obj(
        "id"       -> Json.obj("type" -> "string"),
        "name"     -> Json.obj("type" -> "string"),
        "metadata" -> Json.obj("type" -> "object", "additionalProperties"  -> true)),
     "required"   -> Json.arr("id", "name", "metadata"))
  }
}



object AsPlaySpec {

  implicit val jsValEq: Equality[JsValue] = new Equality[JsValue] {
    override def areEqual(a: JsValue, b: Any): Boolean = a match {
      case JsNull => b == JsNull
      case JsTrue => b == JsTrue
      case JsFalse => b == JsFalse
      case JsNumber(a) if b.isInstanceOf[JsNumber] => b.asInstanceOf[JsNumber].value == a
      case JsString(a) if b.isInstanceOf[JsString] => b.asInstanceOf[JsString].value == a
      case a: JsArray => jsArrEq.areEqual(a, b)
      case a: JsObject => jsObjEq.areEqual(a, b)
      case _ => false
    }
  }

  implicit val jsArrEq: Equality[JsArray] = new Equality[JsArray] {

    override def areEqual(a: JsArray, b: Any): Boolean = b match {
      case b: JsArray =>
        if (a.value.size == b.value.size) {
          a.value forall { aa =>
            b.value exists { bb =>
              jsValEq.areEqual(aa, bb)
            }
          }
        } else
          false
      case _ => false
    }
  }

  implicit val jsObjEq: Equality[JsObject] = new Equality[JsObject] {

    override def areEqual(a: JsObject, b: Any): Boolean = b match {
      case b: JsObject =>
        val keys = a.value.keySet ++ b.value.keySet
        keys.foldLeft(true) {
          case (true, k)  =>
            val r = for {
              a <- a.value.get(k)
              b <- b.value.get(k)
            } yield {
              jsValEq.areEqual(a, b)
            }

            r getOrElse false
          case (false, _) => false
        }
      case _ => false
    }
  }
}