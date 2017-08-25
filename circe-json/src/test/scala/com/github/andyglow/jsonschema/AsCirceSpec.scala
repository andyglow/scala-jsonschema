package com.github.andyglow.jsonschema

import org.scalatest._
import org.scalatest.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks._
import com.github.andyglow.json.Value._
import io.circe._

class AsCirceSpec extends PropSpec {
  import AsCirceSpec._

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

  property("Check Schema.asCirce") {
    import AsCirce._

    json.Json.schema[UserProfile].asCirce() shouldEqual Json.obj(
      f"$$schema"             -> Json.fromString("http://json-schema.org/draft-04/schema#"),
      "type"                  -> Json.fromString("object"),
      "additionalProperties"  -> Json.False,
      "properties"            -> Json.obj(
        "firstName"               -> Json.obj("type" -> Json.fromString("string")),
        "middleName"              -> Json.obj("type" -> Json.fromString("string")),
        "lastName"                -> Json.obj("type" -> Json.fromString("string")),
        "age"                     -> Json.obj("type" -> Json.fromString("integer")),
        "active"                  -> Json.obj("type" -> Json.fromString("boolean"))),
      "required"              -> Json.arr(Json.fromString("age"), Json.fromString("lastName"), Json.fromString("firstName")))
  }
}

object AsCirceSpec {

  case class UserProfile(
    firstName: String,
    middleName: Option[String],
    lastName: String,
    age: Int,
    active: Boolean = true)
}