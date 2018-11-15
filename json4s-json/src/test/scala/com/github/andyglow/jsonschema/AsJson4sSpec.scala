package com.github.andyglow.jsonschema

import org.scalatest._
import org.scalatest.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks._
import com.github.andyglow.json.Value._
import org.json4s.JsonAST._

class AsJson4sSpec extends PropSpec{
  import AsJson4sSpec._

  private val examples = Table(
    ("json"                           , "PlayJson"),
    (`null`                           , JNull),
    (`true`                           , JBool(true)),
    (`false`                          , JBool(false)),
    (str("foo")                       , JString("foo")),
    (num(4)                           , JDecimal(4)),
    (num(4.78)                        , JDecimal(4.78)),
    (arr(1, 2, 3)                     , JArray(List(JDecimal(1), JDecimal(2), JDecimal(3)))),
    (obj("foo" -> "foo", "bar" -> 15) , JObject("foo" -> JString("foo"), "bar" -> JDecimal(15)))
  )

  property("Check that AsJson4s translates internal representation of json to json4s Json") {
    forAll(examples) { (internal, play) => AsJson4s(internal) shouldEqual play }
  }

  property("Check Schema.asJson4s") {
    import AsJson4s._

    json.Json.schema[UserProfile].asJson4s() shouldEqual JObject(
      f"$$schema"             -> JString("http://json-schema.org/draft-04/schema#"),
      "type"                  -> JString("object"),
      "additionalProperties"  -> JBool(false),
      "properties"            -> JObject(
        "firstName"               -> JObject("type" -> JString("string")),
        "middleName"              -> JObject("type" -> JString("string")),
        "lastName"                -> JObject("type" -> JString("string")),
        "age"                     -> JObject("type" -> JString("integer")),
        "active"                  -> JObject("type" -> JString("boolean"))),
      "required"              -> JArray(List(JString("age"), JString("lastName"), JString("firstName"))))
  }
}

object AsJson4sSpec {

  case class UserProfile(
    firstName: String,
    middleName: Option[String],
    lastName: String,
    age: Int,
    active: Boolean = true)
}