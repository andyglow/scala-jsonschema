package com.github.andyglow.jsonschema

import _root_.json.{Json => J}
import _root_.json.Schema._
import _root_.json.schema.JsonSchemaProfile
import com.github.andyglow.json.{Value => V}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._
import petshop.Acceptance
import play.api.libs.json._

/** play-json's AST types with `import AsPlay.predef._` (design doc `docs/design/295-any-value.md`, #286). */
class AnyValueSpec extends AnyFunSuite {
  import AnyValueSpec._
  import AsPlay.predef._

  private val profile: JsonSchemaProfile = PlayProfile.fromConfig(JsonConfiguration.default)

  test("the trees") {
    J.schema[JsValue] shouldBe `any`[JsValue]()
    J.schema[JsObject] shouldBe `object`.Free[JsObject]() // Scala 2: the Predef is found before derivation, which crashed on JsObject (B4)
    J.schema[JsArray] shouldBe `array`[JsValue, IndexedSeq](`any`[JsValue]())
    J.schema[JsString] shouldBe `string`[JsString]
    J.schema[JsNumber] shouldBe `number`[BigDecimal]
    J.schema[JsBoolean] shouldBe `boolean`
  }

  test("as fields: inlined") {
    val s = J.schema[Payload].asInstanceOf[`object`[_]]
    s.fields.map(f => f.name -> f.tpe).toMap.apply("data") shouldBe `any`[JsValue]()
    s.fields.map(f => f.name -> f.tpe).toMap.apply("obj") shouldBe `object`.Free[JsObject]()
  }

  test("validated: what play-json writes") {
    val render                                           = Acceptance.rendered(J.schema[Payload])(profile)
    def doc(data: JsValue, opt: Option[JsValue]): String =
      Json.stringify(
        Json.obj("id" -> "1", "data" -> data, "obj" -> Json.obj("a" -> Json.arr(1, "x")), "arr" -> Json.arr(JsNull, 2), "s" -> "s", "n" -> 1.5, "b" -> true) ++
          opt.fold(Json.obj())(o => Json.obj("opt" -> o))
      )
    Acceptance.assertValid(render, List(doc(JsNumber(5), None), doc(JsNull, Some(JsNull)), doc(Json.obj("x" -> Json.arr()), Some(JsString("o")))))
    Acceptance.assertInvalid(render, List(Json.stringify(Json.obj("id" -> "1", "data" -> 1, "obj" -> 5, "arr" -> Json.arr(), "s" -> "s", "n" -> 1, "b" -> true))))
  }

  test("validated: a Payload written by play's Json.writes") {
    val render = Acceptance.rendered(J.schema[Payload])(profile)
    val sample = Payload("1", Json.obj("x" -> Json.arr(1, JsNull)), None, Json.obj("a" -> 1), Json.arr("x", 2), JsString("s"), JsNumber(1.5), JsBoolean(true))
    Acceptance.assertValid(render, List(Json.stringify(Json.toJson(sample)), Json.stringify(Json.toJson(sample.copy(data = JsNull, opt = Some(JsNull))))))
  }

  test("defaults: play's Writes (AsPlay._)") {
    import AsPlay._
    val o = ProfileSpec.body(ProfileSpec.render(J.schema[WithDefault], _root_.json.schema.Version.Draft07("id"))(profile))
    ProfileSpec.prop(o, "data").asInstanceOf[V.obj].value("default") shouldBe V.num(5)
    ProfileSpec.prop(o, "obj").asInstanceOf[V.obj].value("default") shouldBe V.obj("a" -> V.arr(V.num(1)))
  }
}

object AnyValueSpec {
  case class Payload(id: String, data: JsValue, opt: Option[JsValue], obj: JsObject, arr: JsArray, s: JsString, n: JsNumber, b: JsBoolean)
  implicit val payloadWrites: OWrites[Payload] = Json.writes[Payload]

  case class WithDefault(data: JsValue = JsNumber(5), obj: JsObject = Json.obj("a" -> Json.arr(1)))
}
