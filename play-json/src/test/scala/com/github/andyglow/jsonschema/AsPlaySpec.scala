package com.github.andyglow.jsonschema

import org.scalatest._
import org.scalatest.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks._
import com.github.andyglow.json.Value._
import play.api.libs.json._

class AsPlaySpec extends PropSpec{
  import AsPlaySpec._

  private val examples = Table(
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

  property("Check Schema.asPlay") {
    import AsPlay._

    json.Json.schema[UserProfile].asPlay() shouldEqual Json.obj(
      f"$$schema"             -> "http://json-schema.org/draft-04/schema#",
      "type"                  -> "object",
      "additionalProperties"  -> false,
      "properties"            -> Json.obj(
        "firstName"               -> Json.obj("type" -> "string"),
        "middleName"              -> Json.obj("type" -> "string"),
        "lastName"                -> Json.obj("type" -> "string"),
        "age"                     -> Json.obj("type" -> "integer"),
        "active"                  -> Json.obj("type" -> "boolean")),
      "required"              -> Json.arr("age", "lastName", "firstName"))
  }
}

object AsPlaySpec {

  case class UserProfile(
    firstName: String,
    middleName: Option[String],
    lastName: String,
    age: Int,
    active: Boolean = true)
}