package com.github.andyglow.jsonschema

import org.scalatest._
import org.scalatest.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks._
import com.github.andyglow.json.Value._
import spray.json._

class AsSpraySpec extends PropSpec {
  import AsSpraySpec._

  private val examples = Table(
    ("json"                           , "SprayJson"),
    (`null`                           , JsNull),
    (`true`                           , JsTrue),
    (`false`                          , JsFalse),
    (str("foo")                       , JsString("foo")),
    (num(4)                           , JsNumber(4)),
    (num(4.78)                        , JsNumber(4.78)),
    (arr(1, 2, 3)                     , JsArray(JsNumber(1), JsNumber(2), JsNumber(3))),
    (obj("foo" -> "foo", "bar" -> 15) , JsObject("foo" -> JsString("foo"), "bar" -> JsNumber(15)))
  )

  property("Check that AsSpray translates internal representation of json to Spray Json") {
    forAll(examples) { (internal, spray) => AsSpray(internal) shouldEqual spray }
  }

  property("Check Schema.asSpray") {
    import AsSpray._

    json.Json.schema[UserProfile].asSpray() shouldEqual JsObject(
      f"$$schema"             -> JsString("http://json-schema.org/draft-04/schema#"),
      "type"                  -> JsString("object"),
      "additionalProperties"  -> JsFalse,
      "properties"            -> JsObject(
        "firstName"               -> JsObject("type" -> JsString("string")),
        "middleName"              -> JsObject("type" -> JsString("string")),
        "lastName"                -> JsObject("type" -> JsString("string")),
        "age"                     -> JsObject("type" -> JsString("integer")),
        "active"                  -> JsObject("type" -> JsString("boolean"))),
      "required"              -> JsArray(JsString("age"), JsString("lastName"), JsString("firstName")))
  }
}

object AsSpraySpec {

  case class UserProfile(
    firstName: String,
    middleName: Option[String],
    lastName: String,
    age: Int,
    active: Boolean = true)
}