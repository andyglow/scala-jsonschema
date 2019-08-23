package com.github.andyglow.jsonschema

import com.github.andyglow.json.Value
import org.scalatest._
import org.scalatest.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks._
import com.github.andyglow.json.Value._
import json.schema.Version.Draft04
import org.scalactic.Equality
import spray.json._

class AsSpraySpec extends PropSpec {
  import AsSpraySpec._

  private val examples = Table[Value, JsValue](
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

    json.Json.schema[UserProfile].asSpray(Draft04()) shouldEqual JsObject(
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

  implicit val jsValEq: Equality[JsValue] = new Equality[JsValue] {
    override def areEqual(a: JsValue, b: Any): Boolean = a match {
      case JsNull => b == JsNull
      case JsTrue => b == JsTrue
      case JsFalse => b == JsFalse
      case JsNumber(a) if b.isInstanceOf[JsNumber] => b.asInstanceOf[JsNumber].value == a
      case JsString(a) if b.isInstanceOf[JsString] => b.asInstanceOf[JsString].value == a
      case a: JsArray => jsArrEq.areEqual(a, b)
      case a: JsObject => jsObjEq.areEqual(a, b)
    }
  }

  implicit val jsArrEq: Equality[JsArray] = new Equality[JsArray] {

    override def areEqual(a: JsArray, b: Any): Boolean = b match {
      case b: JsArray =>
        if (a.elements.size == b.elements.size) {
          a.elements forall { aa =>
            b.elements exists { bb =>
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
        val keys = a.fields.keySet ++ b.fields.keySet
        keys.foldLeft(true) {
          case (true, k)  =>
            val r = for {
              a <- a.fields.get(k)
              b <- b.fields.get(k)
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