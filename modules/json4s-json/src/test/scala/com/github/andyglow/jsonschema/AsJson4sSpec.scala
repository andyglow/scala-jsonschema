package com.github.andyglow.jsonschema

import com.github.andyglow.json.Value
import org.scalatest.prop.TableDrivenPropertyChecks._
import com.github.andyglow.json.Value._
import com.github.andyglow.jsonschema.model.UserProfile
import json.schema.Version.Draft04
import org.json4s.JsonAST._
import org.scalactic.Equality
import org.scalatest.matchers.should.Matchers._
import org.scalatest.propspec.AnyPropSpec

class AsJson4sSpec extends AnyPropSpec {
  import AsJson4sSpec._
  import UserProfileJson._

  private val examples = Table[Value, JValue](
    ("json", "Json4S"),
    (`null`, JNull),
    (`true`, JBool(true)),
    (`false`, JBool(false)),
    (str("foo"), JString("foo")),
    (num(4), JDecimal(4)),
    (num(4.78), JDecimal(4.78)),
    (arr(1, 2, 3), JArray(List(JDecimal(1), JDecimal(2), JDecimal(3)))),
    (obj("foo" -> "foo", "bar" -> 15), JObject("foo" -> JString("foo"), "bar" -> JDecimal(15)))
  )

  property("Check that AsJson4s translates internal representation of json to json4s Json") {
    forAll(examples) { (internal, play) => AsJson4s(internal) shouldEqual play }
  }

  property("AsJson4s keeps key order") {
    import org.json4s.native.JsonMethods._

    compact(render(AsJson4s(obj("k1" -> 1, "k2" -> 2, "k3" -> 3, "k4" -> 4, "k5" -> 5, "k6" -> 6)))) shouldBe """{"k1":1,"k2":2,"k3":3,"k4":4,"k5":5,"k6":6}"""
  }

  property("AsJson4s.toValue keeps key order (a case-class default)") {
    val props = AsValue.schema(withObjDefault, Draft04()).value("properties").asInstanceOf[obj]
    props.value("s").asInstanceOf[obj].value("default").asInstanceOf[obj].fields.map(_._1).toList shouldBe List("a1", "a2", "a3", "a4", "a5", "a6")
  }

  property("AsCirce escapes") {
    import org.json4s.native.JsonMethods._

    val jobj = AsJson4s(obj(""""quoted-key"""" -> "\n\t'val\""))
    compact(render(jobj)) shouldBe """{"\"quoted-key\"":"\n\t'val\""}"""
  }

  property("Check Schema.asJson4s") {
    import AsJson4s._
    import org.json4s.DefaultJsonFormats._

    json.Json.schema[UserProfile].asJson4s(Draft04()) shouldEqual JObject(
      f"$$schema"            -> JString("http://json-schema.org/draft-04/schema#"),
      "type"                 -> JString("object"),
      "additionalProperties" -> JBool(false),
      "properties"           -> JObject(
        "firstName"   -> JObject("type" -> JString("string")),
        "middleName"  -> JObject("type" -> JArray(List(JString("string"), JString("null")))),
        "lastName"    -> JObject("type" -> JString("string")),
        "age"         -> JObject("type" -> JString("integer")),
        "lastLoginMs" -> JObject("type" -> JArray(List(JString("number"), JString("null")))),
        "role"        -> JObject(
          "type"    -> JString("string"),
          "default" -> JString("e-user"),
          "enum"    -> JArray(List(JString("e-admin"), JString("e-manager"), JString("e-user")))
        ),
        "active" -> JObject(
          "type"    -> JString("string"),
          "default" -> JString("On"),
          "enum"    -> JArray(List(JString("On"), JString("Off"), JString("Suspended")))
        ),
        "enabledFeatures" -> JObject(
          "type"        -> JString("array"),
          "uniqueItems" -> JBool.True,
          "default"     -> JArray(List(JString("feature-0-name"), JString("feature-1-name"))),
          "items"       -> JObject(
            "type" -> JString("string"),
            "enum" -> JArray(
              List(JString("feature-0-name"), JString("feature-1-name"), JString("feature-2-name"))
            )
          )
        ),
        "credentials" -> JObject(
          "type"                 -> JString("object"),
          "additionalProperties" -> JBool.False,
          "required"             -> JArray(List(JString("login"), JString("password"))),
          "properties"           -> JObject(
            "login"    -> JObject("type" -> JString("string")),
            "password" -> JObject("type" -> JString("string"))
          ),
          "default" -> JObject("login" -> JString("anonymous"), "password" -> JString("-"))
        ),
        "notes" -> JObject(
          "type"                 -> JArray(List(JString("object"), JString("null"))),
          "additionalProperties" -> JBool.False,
          "required"             -> JArray(List(JString("head"), JString("tail"))),
          "properties"           -> JObject(
            "head" -> JObject("type" -> JString("string")),
            "tail" -> JObject(
              "type"  -> JString("array"),
              "items" -> JObject("type" -> JString("string"))
            )
          ),
          "default" -> JObject("head" -> JString("initial note"), "tail" -> JArray(Nil))
        )
      ),
      "required" -> JArray(List(JString("age"), JString("lastName"), JString("firstName")))
    )
  }
}

object AsJson4sSpec {

  // key order through `toValue`: a case-class default, encoded by json4s
  final case class Six(a1: Int, a2: Int, a3: Int, a4: Int, a5: Int, a6: Int)
  final case class WithObjDefault(s: Six = Six(1, 2, 3, 4, 5, 6))
  implicit val sixWriter: org.json4s.Writer[Six] = new org.json4s.Writer[Six] {
    def write(s: Six): JValue = JObject(List("a1" -> JInt(s.a1), "a2" -> JInt(s.a2), "a3" -> JInt(s.a3), "a4" -> JInt(s.a4), "a5" -> JInt(s.a5), "a6" -> JInt(s.a6)))
  }
  lazy val withObjDefault: _root_.json.Schema[WithObjDefault] = {
    import AsJson4s.toValue
    _root_.json.Json.schema[WithObjDefault]
  }

  implicit val jsValEq: Equality[JValue] = new Equality[JValue] {
    override def areEqual(a: JValue, b: Any): Boolean = a match {
      case JNull                                   => b == JNull
      case JBool.True                              => b == JBool.True
      case JBool.False                             => b == JBool.False
      case JDecimal(a) if b.isInstanceOf[JDecimal] => b.asInstanceOf[JDecimal].num == a
      case JString(a) if b.isInstanceOf[JString]   => b.asInstanceOf[JString].s == a
      case a: JArray                               => jsArrEq.areEqual(a, b)
      case a: JObject                              => jsObjEq.areEqual(a, b)
      case _                                       => false
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
        val am   = a.obj.toMap
        val bm   = b.obj.toMap
        val keys = am.keySet ++ bm.keySet
        keys.foldLeft(true) {
          case (true, k) =>
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
