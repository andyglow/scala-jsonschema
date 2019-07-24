package com.github.andyglow.jsonschema

import com.github.andyglow.json.Value
import org.scalatest._
import org.scalatest.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks._
import com.github.andyglow.json.Value._
import org.scalactic.Equality
import play.api.libs.json._

class AsPlaySpec extends PropSpec{
  import AsPlaySpec._

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