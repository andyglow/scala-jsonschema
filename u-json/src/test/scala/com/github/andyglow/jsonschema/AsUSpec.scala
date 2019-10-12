package com.github.andyglow.jsonschema

import com.github.andyglow.json.Value
import org.scalatest._
import org.scalatest.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks._
import com.github.andyglow.json.Value._
import json.schema.Version._
import org.scalactic.Equality


class AsUSpec extends PropSpec {
  import AsUSpec._

  private val examples = Table[Value, ujson.Value](
    ("json"                           , "uJson"),
    (`null`                           , ujson.Null),
    (`true`                           , ujson.True),
    (`false`                          , ujson.False),
    (str("foo")                       , ujson.Str("foo")),
    (num(4)                           , ujson.Num(4)),
    (num(4.78)                        , ujson.Num(4.78)),
    (arr(1, 2, 3)                     , ujson.Arr(ujson.Num(1), ujson.Num(2), ujson.Num(3))),
    (obj("foo" -> "foo", "bar" -> 15) , ujson.Obj("foo" -> ujson.Str("foo"), "bar" -> ujson.Num(15)))
  )

  property("Check that AsU translates internal representation of json to U Json") {
    forAll(examples) { (internal, u) => AsU(internal) shouldEqual u }
  }

  property("Check Schema.asU draft-04") {
    import AsU._

    implicit val ratingSchema = json.Json.schema[Rating]
    ratingSchema.refName

    json.Json.schema[UserProfile].asU(Draft04()) shouldEqual ujson.Obj(
      f"$$schema"             -> ujson.Str("http://json-schema.org/draft-04/schema#"),
      "type"                  -> ujson.Str("object"),
      "additionalProperties"  -> ujson.False,
      "properties"            -> ujson.Obj(
        "rating"                  -> ujson.Obj(f"$$ref" -> ujson.Str("#/definitions/com.github.andyglow.jsonschema.AsUSpec.Rating")),
        "age"                     -> ujson.Obj("type" -> ujson.Str("integer")),
        "active"                  -> ujson.Obj("type" -> ujson.Str("boolean"), "default" -> ujson.True),
        "name"                    -> ujson.Obj(
          "type"                    -> ujson.Str("object"),
          "additionalProperties"    -> ujson.False,
          "properties"              -> ujson.Obj(
            "first"                   -> ujson.Obj("type" -> ujson.Str("string")),
            "middle"                  -> ujson.Obj("type" -> ujson.Str("string")),
            "last"                    -> ujson.Obj("type" -> ujson.Str("string"))),
          "required"                -> ujson.Arr(ujson.Str("first"), ujson.Str("last")))),
      "required"              -> ujson.Arr(ujson.Str("age"), ujson.Str("name"), ujson.Str("rating")),
      "definitions"           -> ujson.Obj(
        "com.github.andyglow.jsonschema.AsUSpec.Rating" -> ujson.Obj(
          "type"                  -> ujson.Str("object"),
          "additionalProperties"  -> ujson.False,
          "properties"            -> ujson.Obj(
            "value"                 -> ujson.Obj(
              "type"                  -> ujson.Str("integer"))),
          "required"              -> ujson.Arr(ujson.Str("value")))))
  }

  property("Check Schema.asU draft-06") {
    import AsU._

    implicit val ratingSchema = json.Json.schema[Rating]
    ratingSchema.refName

    json.Json.schema[UserProfile].asU(Draft06(id = "http://models.org/userProfile.json")) shouldEqual ujson.Obj(
      f"$$schema"             -> ujson.Str("http://json-schema.org/draft-06/schema#"),
      f"$$id"                 -> ujson.Str("http://models.org/userProfile.json"),
      "type"                  -> ujson.Str("object"),
      "additionalProperties"  -> ujson.False,
      "properties"            -> ujson.Obj(
        "rating"                  -> ujson.Obj(f"$$ref" -> ujson.Str("#com.github.andyglow.jsonschema.AsUSpec.Rating")),
        "age"                     -> ujson.Obj("type" -> ujson.Str("integer")),
        "active"                  -> ujson.Obj("type" -> ujson.Str("boolean"), "default" -> ujson.True),
        "name"                    -> ujson.Obj(
          "type"                    -> ujson.Str("object"),
          "additionalProperties"    -> ujson.False,
          "properties"              -> ujson.Obj(
            "first"                   -> ujson.Obj("type" -> ujson.Str("string")),
            "middle"                  -> ujson.Obj("type" -> ujson.Str("string")),
            "last"                    -> ujson.Obj("type" -> ujson.Str("string"))),
          "required"                -> ujson.Arr(ujson.Str("first"), ujson.Str("last")))),
      "required"              -> ujson.Arr(ujson.Str("age"), ujson.Str("name"), ujson.Str("rating")),
      "definitions"           -> ujson.Obj(
        "com.github.andyglow.jsonschema.AsUSpec.Rating" -> ujson.Obj(
          f"$$id"                 -> ujson.Str("#com.github.andyglow.jsonschema.AsUSpec.Rating"),
          "type"                  -> ujson.Str("object"),
          "additionalProperties"  -> ujson.False,
          "properties"            -> ujson.Obj(
            "value"                 -> ujson.Obj(
              "type"                  -> ujson.Str("integer"))),
          "required"              -> ujson.Arr(ujson.Str("value")))))
  }

  property("Check Schema.asU draft-07") {
    import AsU._

    implicit val ratingSchema = json.Json.schema[Rating]
    ratingSchema.refName

    json.Json.schema[UserProfile].asU(Draft07(id = "http://models.org/userProfile.json")) shouldEqual ujson.Obj(
      f"$$schema"             -> ujson.Str("http://json-schema.org/draft-07/schema#"),
      f"$$id"                 -> ujson.Str("http://models.org/userProfile.json"),
      "type"                  -> ujson.Str("object"),
      "additionalProperties"  -> ujson.False,
      "properties"            -> ujson.Obj(
        "rating"                  -> ujson.Obj(f"$$ref" -> ujson.Str("#com.github.andyglow.jsonschema.AsUSpec.Rating")),
        "age"                     -> ujson.Obj("type" -> ujson.Str("integer")),
        "active"                  -> ujson.Obj("type" -> ujson.Str("boolean"), "default" -> ujson.True),
        "name"                    -> ujson.Obj(
          "type"                    -> ujson.Str("object"),
          "additionalProperties"    -> ujson.False,
          "properties"              -> ujson.Obj(
            "first"                   -> ujson.Obj("type" -> ujson.Str("string")),
            "middle"                  -> ujson.Obj("type" -> ujson.Str("string")),
            "last"                    -> ujson.Obj("type" -> ujson.Str("string"))),
          "required"                -> ujson.Arr(ujson.Str("first"), ujson.Str("last")))),
      "required"              -> ujson.Arr(ujson.Str("age"), ujson.Str("name"), ujson.Str("rating")),
      "definitions"           -> ujson.Obj(
        "com.github.andyglow.jsonschema.AsUSpec.Rating" -> ujson.Obj(
          f"$$id"                 -> ujson.Str("#com.github.andyglow.jsonschema.AsUSpec.Rating"),
          "type"                  -> ujson.Str("object"),
          "additionalProperties"  -> ujson.False,
          "properties"            -> ujson.Obj(
            "value"                 -> ujson.Obj(
              "type"                  -> ujson.Str("integer"))),
          "required"              -> ujson.Arr(ujson.Str("value")))))
  }
}

object AsUSpec {

  case class Name(
    first: String,
    middle: Option[String],
    last: String)

  case class Rating(value: Int)

  case class UserProfile(
    name: Name,
    rating: Rating,
    age: Int,
    active: Boolean = true)

  implicit val jsValEq: Equality[ujson.Value] = new Equality[ujson.Value] {
    override def areEqual(a: ujson.Value, b: Any): Boolean = a match {
      case ujson.Null => b == ujson.Null
      case ujson.True => b == ujson.True
      case ujson.False => b == ujson.False
      case ujson.Num(a) if b.isInstanceOf[ujson.Num] => b.asInstanceOf[ujson.Num].value == a
      case ujson.Str(a) if b.isInstanceOf[ujson.Str] => b.asInstanceOf[ujson.Str].value == a
      case a: ujson.Arr => jsArrEq.areEqual(a, b)
      case a: ujson.Obj => jsObjEq.areEqual(a, b)
    }
  }

  implicit val jsArrEq: Equality[ujson.Arr] = new Equality[ujson.Arr] {

    override def areEqual(a: ujson.Arr, b: Any): Boolean = b match {
      case b: ujson.Arr =>
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

  implicit val jsObjEq: Equality[ujson.Obj] = new Equality[ujson.Obj] {

    override def areEqual(a: ujson.Obj, b: Any): Boolean = b match {
      case b: ujson.Obj =>
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