package com.github.andyglow.jsonschema

import com.github.andyglow.json.Value
import org.scalatest.prop.TableDrivenPropertyChecks._
import com.github.andyglow.json.Value._
import com.github.andyglow.jsonschema.model.UserProfile
import json.schema.Version.Draft04
import org.scalactic.Equality
import spray.json._
import org.scalatest.matchers.should.Matchers._
import org.scalatest.propspec.AnyPropSpec

class AsSpraySpec extends AnyPropSpec {
  import AsSpraySpec._
  import UserProfileJson._

  private val examples = Table[Value, JsValue](
    ("json", "SprayJson"),
    (`null`, JsNull),
    (`true`, JsTrue),
    (`false`, JsFalse),
    (str("foo"), JsString("foo")),
    (num(4), JsNumber(4)),
    (num(4.78), JsNumber(4.78)),
    (arr(1, 2, 3), JsArray(JsNumber(1), JsNumber(2), JsNumber(3))),
    (obj("foo" -> "foo", "bar" -> 15), JsObject("foo" -> JsString("foo"), "bar" -> JsNumber(15)))
  )

  property("Check that AsSpray translates internal representation of json to Spray Json") {
    forAll(examples) { (internal, spray) => AsSpray(internal) shouldEqual spray }
  }

  property("AsSpray keeps key order") {
    AsSpray(obj("k1" -> 1, "k2" -> 2, "k3" -> 3, "k4" -> 4, "k5" -> 5, "k6" -> 6)).compactPrint shouldBe """{"k1":1,"k2":2,"k3":3,"k4":4,"k5":5,"k6":6}"""
  }

  property("AsSpray.toValue keeps key order (a case-class default)") {
    val props = AsValue.schema(withObjDefault, Draft04()).value("properties").asInstanceOf[obj]
    props.value("s").asInstanceOf[obj].value("default").asInstanceOf[obj].fields.map(_._1).toList shouldBe List("a1", "a2", "a3", "a4", "a5", "a6")
  }

  property("AsSpray escapes") {
    val jobj = AsSpray(obj(""""quoted-key"""" -> "\n\t'val\""))
    jobj.compactPrint shouldBe """{"\"quoted-key\"":"\n\t'val\""}"""
  }

  property("Check Schema.asSpray") {
    import AsSpray._

    json.Json.schema[UserProfile].asSpray(Draft04()) shouldEqual JsObject(
      f"$$schema"            -> JsString("http://json-schema.org/draft-04/schema#"),
      "type"                 -> JsString("object"),
      "additionalProperties" -> JsFalse,
      "properties"           -> JsObject(
        "firstName"   -> JsObject("type" -> JsString("string")),
        "middleName"  -> JsObject("type" -> JsArray(JsString("string"), JsString("null"))),
        "lastName"    -> JsObject("type" -> JsString("string")),
        "age"         -> JsObject("type" -> JsString("integer")),
        "lastLoginMs" -> JsObject("type" -> JsArray(JsString("number"), JsString("null"))),
        "role"        -> JsObject(
          "type"    -> JsString("string"),
          "default" -> JsString("e-user"),
          "enum"    -> JsArray(JsString("e-admin"), JsString("e-manager"), JsString("e-user"))
        ),
        "active" -> JsObject(
          "type"    -> JsString("string"),
          "default" -> JsString("On"),
          "enum"    -> JsArray(JsString("On"), JsString("Off"), JsString("Suspended"))
        ),
        "enabledFeatures" -> JsObject(
          "type"        -> JsString("array"),
          "uniqueItems" -> JsTrue,
          "default"     -> JsArray(JsString("feature-0-name"), JsString("feature-1-name")),
          "items"       -> JsObject(
            "type" -> JsString("string"),
            "enum" -> JsArray(
              JsString("feature-0-name"),
              JsString("feature-1-name"),
              JsString("feature-2-name")
            )
          )
        ),
        "credentials" -> JsObject(
          "type"                 -> JsString("object"),
          "additionalProperties" -> JsFalse,
          "required"             -> JsArray(JsString("login"), JsString("password")),
          "properties"           -> JsObject(
            "login"    -> JsObject("type" -> JsString("string")),
            "password" -> JsObject("type" -> JsString("string"))
          ),
          "default" -> JsObject("login" -> JsString("anonymous"), "password" -> JsString("-"))
        ),
        "notes" -> JsObject(
          "type"                 -> JsArray(JsString("object"), JsString("null")),
          "additionalProperties" -> JsFalse,
          "required"             -> JsArray(JsString("head"), JsString("tail")),
          "properties"           -> JsObject(
            "head" -> JsObject("type" -> JsString("string")),
            "tail" -> JsObject(
              "type"  -> JsString("array"),
              "items" -> JsObject("type" -> JsString("string"))
            )
          ),
          "default" -> JsObject("head" -> JsString("initial note"), "tail" -> JsArray())
        )
      ),
      "required" -> JsArray(JsString("age"), JsString("lastName"), JsString("firstName"))
    )
  }
}

object AsSpraySpec {

  // key order through `toValue`: a case-class default, encoded by spray (a ListMap: spray's own `JsObject(k -> v, ...)` is a HashMap)
  final case class Six(a1: Int, a2: Int, a3: Int, a4: Int, a5: Int, a6: Int)
  final case class WithObjDefault(s: Six = Six(1, 2, 3, 4, 5, 6))
  implicit val sixWriter: JsonWriter[Six] = new JsonWriter[Six] {
    def write(s: Six): JsValue = JsObject(
      scala.collection.immutable
        .ListMap[String, JsValue]("a1" -> JsNumber(s.a1), "a2" -> JsNumber(s.a2), "a3" -> JsNumber(s.a3), "a4" -> JsNumber(s.a4), "a5" -> JsNumber(s.a5), "a6" -> JsNumber(s.a6))
    )
  }
  lazy val withObjDefault: _root_.json.Schema[WithObjDefault] = {
    import AsSpray.toValue
    _root_.json.Json.schema[WithObjDefault]
  }

  implicit val jsValEq: Equality[JsValue] = new Equality[JsValue] {
    override def areEqual(a: JsValue, b: Any): Boolean = a match {
      case JsNull                                  => b == JsNull
      case JsTrue                                  => b == JsTrue
      case JsFalse                                 => b == JsFalse
      case JsNumber(a) if b.isInstanceOf[JsNumber] => b.asInstanceOf[JsNumber].value == a
      case JsString(a) if b.isInstanceOf[JsString] => b.asInstanceOf[JsString].value == a
      case a: JsArray                              => jsArrEq.areEqual(a, b)
      case a: JsObject                             => jsObjEq.areEqual(a, b)
      case _                                       => false
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
          case (true, k) =>
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
