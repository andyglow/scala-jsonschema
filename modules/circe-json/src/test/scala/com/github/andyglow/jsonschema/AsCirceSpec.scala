package com.github.andyglow.jsonschema

import org.scalatest.prop.TableDrivenPropertyChecks._
import com.github.andyglow.json.Value._
import com.github.andyglow.jsonschema.model.UserProfile
import io.circe._
import json.schema.Version.Draft04
import org.scalactic.Equality
import org.scalatest.matchers.should.Matchers._
import org.scalatest.propspec.AnyPropSpec

class AsCirceSpec extends AnyPropSpec {
  import AsCirceSpec._
  import UserProfileJson._

  private val examples = Table(
    ("json", "Circe"),
    (`null`, Json.Null),
    (`true`, Json.True),
    (`false`, Json.False),
    (str("foo"), Json.fromString("foo")),
    (num(4), Json.fromInt(4)),
    (num(4.78), Json.fromDoubleOrNull(4.78)),
    (arr(1, 2, 3), Json.arr(Json.fromInt(1), Json.fromInt(2), Json.fromInt(3))),
    (
      obj("foo"      -> "foo", "bar"                  -> 15),
      Json.obj("foo" -> Json.fromString("foo"), "bar" -> Json.fromInt(15))
    )
  )

  property("Check that AsCirce translates internal representation of json to Circe") {
    forAll(examples) { (internal, circe) => AsCirce(internal) shouldEqual circe }
  }

  property("AsCirce keeps key order") {
    AsCirce(obj("k1" -> 1, "k2" -> 2, "k3" -> 3, "k4" -> 4, "k5" -> 5, "k6" -> 6)).noSpaces shouldBe """{"k1":1,"k2":2,"k3":3,"k4":4,"k5":5,"k6":6}"""
  }

  property("AsCirce.toValue keeps key order (a case-class default)") {
    val props = AsValue.schema(withObjDefault, Draft04()).value("properties").asInstanceOf[obj]
    props.value("s").asInstanceOf[obj].value("default").asInstanceOf[obj].fields.map(_._1).toList shouldBe List("a1", "a2", "a3", "a4", "a5", "a6")
  }

  property("AsCirce escapes") {
    AsCirce(
      obj(""""quoted-key"""" -> "\n\t'val\"")
    ).noSpaces shouldBe """{"\"quoted-key\"":"\n\t'val\""}"""
  }

  property("Check Schema.asCirce") {
    import AsCirce._

    json.Json.schema[UserProfile].asCirce(Draft04()) shouldEqual Json.obj(
      f"$$schema"            -> Json.fromString("http://json-schema.org/draft-04/schema#"),
      "type"                 -> Json.fromString("object"),
      "additionalProperties" -> Json.False,
      "properties"           -> Json.obj(
        "firstName"   -> Json.obj("type" -> Json.fromString("string")),
        "middleName"  -> Json.obj("type" -> Json.arr(Json.fromString("string"), Json.fromString("null"))),
        "lastName"    -> Json.obj("type" -> Json.fromString("string")),
        "age"         -> Json.obj("type" -> Json.fromString("integer")),
        "lastLoginMs" -> Json.obj("type" -> Json.arr(Json.fromString("number"), Json.fromString("null"))),
        "role"        -> Json.obj(
          "type"    -> Json.fromString("string"),
          "default" -> Json.fromString("e-user"),
          "enum"    -> Json.arr(
            Json.fromString("e-admin"),
            Json.fromString("e-manager"),
            Json.fromString("e-user")
          )
        ),
        "active" -> Json.obj(
          "type"    -> Json.fromString("string"),
          "default" -> Json.fromString("On"),
          "enum"    -> Json
            .arr(Json.fromString("On"), Json.fromString("Off"), Json.fromString("Suspended"))
        ),
        "enabledFeatures" -> Json.obj(
          "type"        -> Json.fromString("array"),
          "uniqueItems" -> Json.True,
          "default"     -> Json
            .arr(Json.fromString("feature-0-name"), Json.fromString("feature-1-name")),
          "items" -> Json.obj(
            "type" -> Json.fromString("string"),
            "enum" -> Json.arr(
              Json.fromString("feature-0-name"),
              Json.fromString("feature-1-name"),
              Json.fromString("feature-2-name")
            )
          )
        ),
        "credentials" -> Json.obj(
          "type"                 -> Json.fromString("object"),
          "additionalProperties" -> Json.False,
          "required"             -> Json.arr(Json.fromString("login"), Json.fromString("password")),
          "properties"           -> Json.obj(
            "login"    -> Json.obj("type" -> Json.fromString("string")),
            "password" -> Json.obj("type" -> Json.fromString("string"))
          ),
          "default" -> Json
            .obj("login" -> Json.fromString("anonymous"), "password" -> Json.fromString("-"))
        ),
        "notes" -> Json.obj(
          "type"                 -> Json.arr(Json.fromString("object"), Json.fromString("null")),
          "additionalProperties" -> Json.False,
          "required"             -> Json.arr(Json.fromString("head"), Json.fromString("tail")),
          "properties"           -> Json.obj(
            "head" -> Json.obj("type" -> Json.fromString("string")),
            "tail" -> Json.obj(
              "type"  -> Json.fromString("array"),
              "items" -> Json.obj("type" -> Json.fromString("string"))
            )
          ),
          "default" -> Json.obj("head" -> Json.fromString("initial note"), "tail" -> Json.arr())
        )
      ),
      "required" -> Json.arr(
        Json.fromString("age"),
        Json.fromString("lastName"),
        Json.fromString("firstName")
      )
    )
  }
}

object AsCirceSpec {

  // key order through `toValue`: a case-class default, encoded by circe
  final case class Six(a1: Int, a2: Int, a3: Int, a4: Int, a5: Int, a6: Int)
  final case class WithObjDefault(s: Six = Six(1, 2, 3, 4, 5, 6))
  implicit val sixEncoder: Encoder[Six] = Encoder.instance { s =>
    Json.obj(
      "a1" -> Json.fromInt(s.a1),
      "a2" -> Json.fromInt(s.a2),
      "a3" -> Json.fromInt(s.a3),
      "a4" -> Json.fromInt(s.a4),
      "a5" -> Json.fromInt(s.a5),
      "a6" -> Json.fromInt(s.a6)
    )
  }
  lazy val withObjDefault: _root_.json.Schema[WithObjDefault] = {
    import AsCirce.toValue
    _root_.json.Json.schema[WithObjDefault]
  }

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
            val am   = aa.toMap
            val bm   = bb.toMap
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
          }

          r getOrElse false
        } else
          false
      case _ => false
    }
  }
}
