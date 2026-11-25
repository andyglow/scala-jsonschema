package com.github.andyglow.jsonschema

import _root_.json.{Json => J}
import _root_.json.Schema._
import _root_.json.schema.JsonSchemaProfile
import _root_.json.schema.Version.Draft07
import com.github.andyglow.json.{Value => V}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._
import petshop.{Acceptance, JsonValidator}
import spray.json._

/** spray-json's AST types with `import AsSpray.predef._` (design doc `docs/design/295-any-value.md`). Without the import they derive as a family of spray's case classes, which rejects what spray
  * writes.
  */
class AnyValueSpec extends AnyFunSuite {
  import AnyValueSpec._
  import AsSpray.predef._

  private val profile: JsonSchemaProfile = SprayProfile.default

  test("the trees") {
    J.schema[JsValue] shouldBe `any`[JsValue]()
    J.schema[JsObject] shouldBe `object`.Free[JsObject]()
    J.schema[JsArray] shouldBe `array`[JsValue, Vector](`any`[JsValue]())
    J.schema[JsString] shouldBe `string`[JsString]
    J.schema[JsNumber] shouldBe `number`[BigDecimal]
    J.schema[JsBoolean] shouldBe `boolean`
  }

  test("as fields: inlined") {
    val fields = J.schema[Payload].asInstanceOf[`object`[_]].fields.map(f => f.name -> f.tpe).toMap
    fields("data") shouldBe `any`[JsValue]()
    fields("obj") shouldBe `object`.Free[JsObject]()
  }

  test("validated: what spray-json writes (the structural schema didn't accept these)") {
    val render                                           = Acceptance.rendered(J.schema[Payload])(profile)
    def doc(data: JsValue, opt: Option[JsValue]): String =
      JsObject(
        Map[String, JsValue](
          "id"   -> JsString("1"),
          "data" -> data,
          "obj"  -> JsObject("a" -> JsArray(JsNumber(1), JsString("x"))),
          "arr"  -> JsArray(JsNull, JsNumber(2)),
          "s"    -> JsString("s"),
          "n"    -> JsNumber(1.5),
          "b"    -> JsTrue
        ) ++ opt.map("opt" -> _)
      ).compactPrint
    Acceptance.assertValid(render, List(doc(JsNumber(5), None), doc(JsNull, Some(JsNull)), doc(JsArray(JsFalse), Some(JsString("o")))))
    Acceptance.assertInvalid(
      render,
      List(JsObject("id" -> JsString("1"), "data" -> JsNumber(1), "obj" -> JsNumber(5), "arr" -> JsArray(), "s" -> JsString("s"), "n" -> JsNumber(1), "b" -> JsTrue).compactPrint)
    )
    accepts(J.schema[JsValue], "\"abc\"") shouldBe true
  }

  test("without the import, spray's AST derives structurally and rejects what spray writes") {
    accepts(structuralJsValue, "\"abc\"") shouldBe false
    accepts(structuralJsObject, """{"a":1}""") shouldBe false
  }

  test("defaults: spray's JsonWriter (AsSpray._)") {
    import AsSpray._
    import DefaultJsonProtocol._
    val o = ProfileSpec.body(ProfileSpec.render(J.schema[WithDefault], Draft07("id"))(profile))
    ProfileSpec.prop(o, "data").asInstanceOf[V.obj].value("default") shouldBe V.num(5)
    ProfileSpec.prop(o, "obj").asInstanceOf[V.obj].value("default") shouldBe V.obj("a" -> V.arr(V.num(1)))
  }
}

object AnyValueSpec {
  // derived here, where `AsSpray.predef._` isn't imported
  val structuralJsValue: _root_.json.Schema[JsValue]   = J.schema[JsValue]
  val structuralJsObject: _root_.json.Schema[JsObject] = J.schema[JsObject]

  def accepts(s: _root_.json.Schema[_], doc: String): Boolean = JsonValidator.errors(Draft07("https://e2.test/s.json"), s.draft07("https://e2.test/s.json"), doc).isEmpty

  case class Payload(id: String, data: JsValue, opt: Option[JsValue], obj: JsObject, arr: JsArray, s: JsString, n: JsNumber, b: JsBoolean)

  case class WithDefault(data: JsValue = JsNumber(5), obj: JsObject = JsObject("a" -> JsArray(JsNumber(1))))
}
