package com.github.andyglow.jsonschema

import com.github.andyglow.json.Value
import org.scalatest.prop.TableDrivenPropertyChecks._
import com.github.andyglow.json.Value._
import com.github.andyglow.jsonschema.model.UserProfile
import json.schema.Version.Draft04
import org.scalactic.Equality
import play.api.libs.json._
import org.scalatest.matchers.should.Matchers._
import org.scalatest.propspec.AnyPropSpec

class AsPlaySpec extends AnyPropSpec {
  import AsPlaySpec._
  import UserProfileJson._

  private val examples = Table[Value, JsValue](
    ("json", "PlayJson"),
    (`null`, JsNull),
    (`true`, JsTrue),
    (`false`, JsFalse),
    (str("foo"), JsString("foo")),
    (num(4), JsNumber(4)),
    (num(4.78), JsNumber(4.78)),
    (arr(1, 2, 3), Json.arr(1, 2, 3)),
    (obj("foo" -> "foo", "bar" -> 15), Json.obj("foo" -> "foo", "bar" -> 15))
  )

  property("Check that AsPlay translates internal representation of json to Play Json") {
    forAll(examples) { (internal, play) => AsPlay(internal) shouldEqual play }
  }

  property("AsPlay keeps key order") {
    Json.stringify(AsPlay(obj("k1" -> 1, "k2" -> 2, "k3" -> 3, "k4" -> 4, "k5" -> 5, "k6" -> 6))) shouldBe """{"k1":1,"k2":2,"k3":3,"k4":4,"k5":5,"k6":6}"""
  }

  property("AsPlay.toValue keeps key order (a case-class default)") {
    val props = AsValue.schema(withObjDefault, Draft04()).value("properties").asInstanceOf[obj]
    props.value("s").asInstanceOf[obj].value("default").asInstanceOf[obj].fields.map(_._1).toList shouldBe List("a1", "a2", "a3", "a4", "a5", "a6")
  }

  property("AsPlay escapes") {
    val jobj = AsPlay(obj(""""quoted-key"""" -> "\n\t'val\""))
    Json.stringify(jobj) shouldBe """{"\"quoted-key\"":"\n\t'val\""}"""
  }

  property("Check Schema.asPlay") {
    import AsPlay._

    json.Json.schema[UserProfile].asPlay(Draft04()) shouldEqual Json.obj(
      f"$$schema"            -> "http://json-schema.org/draft-04/schema#",
      "type"                 -> "object",
      "additionalProperties" -> false,
      "properties"           -> Json.obj(
        "firstName"   -> Json.obj("type" -> "string"),
        "middleName"  -> Json.obj("type" -> Json.arr("string", "null")),
        "lastName"    -> Json.obj("type" -> "string"),
        "age"         -> Json.obj("type" -> "integer"),
        "lastLoginMs" -> Json.obj("type" -> Json.arr("number", "null")),
        "role"        -> Json.obj(
          "type"    -> "string",
          "default" -> "e-user",
          "enum"    -> Json.arr("e-admin", "e-manager", "e-user")
        ),
        "active" -> Json
          .obj("type" -> "string", "default" -> "On", "enum" -> Json.arr("On", "Off", "Suspended")),
        "enabledFeatures" -> Json.obj(
          "type"        -> "array",
          "uniqueItems" -> true,
          "default"     -> Json.arr("feature-0-name", "feature-1-name"),
          "items"       -> Json.obj(
            "type" -> "string",
            "enum" -> Json.arr("feature-0-name", "feature-1-name", "feature-2-name")
          )
        ),
        "credentials" -> Json.obj(
          "type"                 -> "object",
          "additionalProperties" -> false,
          "required"             -> Json.arr("login", "password"),
          "properties"           -> Json.obj(
            "login"    -> Json.obj("type" -> "string"),
            "password" -> Json.obj("type" -> "string")
          ),
          "default" -> Json.obj("login" -> "anonymous", "password" -> "-")
        ),
        "notes" -> Json.obj(
          "type"                 -> Json.arr("object", "null"),
          "additionalProperties" -> false,
          "required"             -> Json.arr("head", "tail"),
          "properties"           -> Json.obj(
            "head" -> Json.obj("type" -> "string"),
            "tail" -> Json.obj("type" -> "array", "items" -> Json.obj("type" -> "string"))
          ),
          "default" -> Json.obj("head" -> "initial note", "tail" -> Json.arr())
        )
      ),
      "required" -> Json.arr("age", "lastName", "firstName")
    )
  }

  property("Option: X or null by default; Legacy as an implicit or per schema; Option defaults") {
    import AsPlay._
    import json.schema.JsonSchemaProfile
    import json.schema.Version.Draft07

    val s       = json.Json.schema[com.github.andyglow.jsonschema.model.WithOptionDefaults]
    val neutral = s.asPlay(Draft07("x"))
    (neutral \ "properties" \ "some").get shouldEqual Json.obj("type" -> Json.arr("integer", "null"), "default" -> 5)
    (neutral \ "properties" \ "none").get shouldEqual Json.obj("type" -> Json.arr("integer", "null"))
    (neutral \ "required").toOption shouldBe None

    val legacy = s.withProfile(JsonSchemaProfile.Legacy).asPlay(Draft07("x"))
    (legacy \ "properties" \ "some").get shouldEqual Json.obj("type" -> "integer", "default" -> 5)
    (legacy \ "properties" \ "none").get shouldEqual Json.obj("type" -> "integer")
    locally {
      implicit val profile: JsonSchemaProfile = JsonSchemaProfile.Legacy
      s.asPlay(Draft07("x")) shouldEqual legacy
    }
  }

  property("Check Schema.asPlay (+ free notation, as schema/definition") {
    import AsPlay._

    implicit val metaSchema: json.Schema[JsObject] = json.Schema.`object`.Free[JsObject]()

    json.Json.schema[Person].asPlay(Draft04()) shouldEqual Json.obj(
      f"$$schema"            -> "http://json-schema.org/draft-04/schema#",
      "type"                 -> "object",
      "additionalProperties" -> false,
      "properties"           -> Json.obj(
        "id"       -> Json.obj("type" -> "string"),
        "name"     -> Json.obj("type" -> "string"),
        "metadata" -> Json.obj(f"$$ref" -> "#/definitions/play.api.libs.json.JsObject")
      ),
      "required"    -> Json.arr("id", "name", "metadata"),
      "definitions" -> Json.obj(
        "play.api.libs.json.JsObject" -> Json.obj(
          "type"                 -> "object",
          "additionalProperties" -> true
        )
      )
    )
  }

  property("Check Schema.asPlay (+ free notation, predef/inline") {
    import AsPlay._

    implicit val metaSchema: json.schema.Predef[JsObject] =
      json.schema.Predef(json.Schema.`object`.Free[JsObject]())

    json.Json.schema[Person].asPlay(Draft04()) shouldEqual Json.obj(
      f"$$schema"            -> "http://json-schema.org/draft-04/schema#",
      "type"                 -> "object",
      "additionalProperties" -> false,
      "properties"           -> Json.obj(
        "id"       -> Json.obj("type" -> "string"),
        "name"     -> Json.obj("type" -> "string"),
        "metadata" -> Json.obj("type" -> "object", "additionalProperties" -> true)
      ),
      "required" -> Json.arr("id", "name", "metadata")
    )
  }
}

object AsPlaySpec {

  // key order through `toValue`: a case-class default, encoded by play-json
  final case class Six(a1: Int, a2: Int, a3: Int, a4: Int, a5: Int, a6: Int)
  final case class WithObjDefault(s: Six = Six(1, 2, 3, 4, 5, 6))
  implicit val sixWrites: Writes[Six] = Writes[Six] { s => Json.obj("a1" -> s.a1, "a2" -> s.a2, "a3" -> s.a3, "a4" -> s.a4, "a5" -> s.a5, "a6" -> s.a6) }
  lazy val withObjDefault: _root_.json.Schema[WithObjDefault] = {
    import AsPlay.toValue
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
          case (true, k) =>
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
