package com.github.andyglow.jsonschema

import com.github.andyglow.json.Value
import org.scalatest._
import org.scalatest.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks._
import com.github.andyglow.json.Value._
import json.schema.Version.Draft04
import org.json4s.JsonAST._
import org.scalactic.Equality

class AsJson4sSpec extends PropSpec{
  import AsJson4sSpec._

  private val examples = Table[Value, JValue](
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

    json.Json.schema[UserProfile].asJson4s(Draft04()) shouldEqual JObject(
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


  implicit val jsValEq: Equality[JValue] = new Equality[JValue] {
    override def areEqual(a: JValue, b: Any): Boolean = a match {
      case JNull => b == JNull
      case JBool.True => b == JBool.True
      case JBool.False => b == JBool.False
      case JDecimal(a) if b.isInstanceOf[JDecimal] => b.asInstanceOf[JDecimal].num == a
      case JString(a) if b.isInstanceOf[JString] => b.asInstanceOf[JString].s == a
      case a: JArray => jsArrEq.areEqual(a, b)
      case a: JObject => jsObjEq.areEqual(a, b)
    }
  }

  implicit val jsArrEq: Equality[JArray] = new Equality[JArray] {

    override def areEqual(a: JArray, b: Any): Boolean = b match {
      case b: JArray =>
        if (a.arr.size == b.arr.size) {
          a.arr forall { aa =>
            b.arr exists { bb =>
              jsValEq.areEqual(aa, bb)
            }
          }
        } else
          false
      case _ => false
    }
  }

  implicit val jsObjEq: Equality[JObject] = new Equality[JObject] {

    override def areEqual(a: JObject, b: Any): Boolean = b match {
      case b: JObject =>
        val am = a.obj.toMap
        val bm = b.obj.toMap
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
      case _ => false
    }
  }
}