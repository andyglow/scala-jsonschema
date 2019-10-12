package com.github.andyglow.jsonschema

import org.scalatest._
import org.scalatest.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks._
import com.github.andyglow.json.Value._
import io.circe._
import json.schema.Version.Draft04
import org.scalactic.Equality

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

    json.Json.schema[UserProfile].asCirce(Draft04()) shouldEqual Json.obj(
      f"$$schema"             -> Json.fromString("http://json-schema.org/draft-04/schema#"),
      "type"                  -> Json.fromString("object"),
      "additionalProperties"  -> Json.False,
      "properties"            -> Json.obj(
        "firstName"               -> Json.obj("type" -> Json.fromString("string")),
        "middleName"              -> Json.obj("type" -> Json.fromString("string")),
        "lastName"                -> Json.obj("type" -> Json.fromString("string")),
        "age"                     -> Json.obj("type" -> Json.fromString("integer")),
        "active"                  -> Json.obj("type" -> Json.fromString("boolean"), "default" -> Json.True)),
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