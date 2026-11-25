package com.github.andyglow.jsonschema

import _root_.json.{Json => J}
import _root_.json.Schema._
import _root_.json.schema.JsonSchemaProfile
import _root_.json.schema.Version.Draft07
import com.github.andyglow.json.{Value => V}
import org.json4s._
import org.json4s.native.JsonMethods.{compact, render => jrender}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._
import petshop.{Acceptance, JsonValidator}

/** json4s' AST types with `import AsJson4s.predef._` (design doc `docs/design/295-any-value.md`). Without the import they derive as a family of json4s' case classes, which rejects what json4s writes.
  */
class AnyValueSpec extends AnyFunSuite {
  import AnyValueSpec._
  import AsJson4s.predef._

  private val profile: JsonSchemaProfile = Json4sProfile.default

  private def write(x: JValue): String = compact(jrender(x))

  test("the trees") {
    J.schema[JValue] shouldBe `any`[JValue]()
    J.schema[JObject] shouldBe `object`.Free[JObject]()
    J.schema[JArray] shouldBe `array`[JValue, List](`any`[JValue]())
    J.schema[JSet] shouldBe `array`[JValue, Set](`any`[JValue](), unique = true)
    J.schema[JString] shouldBe `string`[JString]
    J.schema[JDouble] shouldBe `number`[Double]
    J.schema[JDecimal] shouldBe `number`[BigDecimal]
    J.schema[JLong] shouldBe `number`[Long]
    J.schema[JInt] shouldBe `number`[BigInt]
    J.schema[JNumber] shouldBe `number`[BigDecimal]
    J.schema[JBool] shouldBe `boolean`
  }

  test("as fields: inlined") {
    val fields = J.schema[Payload].asInstanceOf[`object`[_]].fields.map(f => f.name -> f.tpe).toMap
    fields("data") shouldBe `any`[JValue]()
    fields("obj") shouldBe `object`.Free[JObject]()
  }

  test("validated: what json4s writes (the structural schema didn't accept these)") {
    val render                                         = Acceptance.rendered(J.schema[Payload])(profile)
    def doc(data: JValue, opt: Option[JValue]): String =
      write(
        JObject(
          List(
            "id"   -> JString("1"),
            "data" -> data,
            "obj"  -> JObject("a" -> JArray(List(JInt(1), JString("x")))),
            "arr"  -> JArray(List(JNull, JInt(2))),
            "s"    -> JString("s"),
            "n"    -> JDouble(1.5),
            "b"    -> JBool(true)
          ) ++
            opt.map("opt" -> _).toList
        )
      )
    Acceptance.assertValid(render, List(doc(JInt(5), None), doc(JNull, Some(JNull)), doc(JArray(List(JBool(false))), Some(JString("o")))))
    Acceptance.assertInvalid(render, List(write(JObject("id" -> JString("1"), "data" -> JInt(1), "obj" -> JInt(5), "arr" -> JArray(Nil), "s" -> JString("s"), "n" -> JInt(1), "b" -> JBool(true)))))
    accepts(J.schema[JValue], "\"abc\"") shouldBe true
  }

  test("without the import, json4s' AST derives structurally and rejects what json4s writes") {
    accepts(structuralJValue, "\"abc\"") shouldBe false
    accepts(structuralJObject, """{"a":1}""") shouldBe false
  }

  test("JNothing: json4s omits the field, so a required JValue field fails `required` (documented)") {
    val written = write(JObject("id" -> JString("1"), "data" -> JNothing))
    written shouldBe """{"id":"1"}"""
    accepts(J.schema[Small], written) shouldBe false
  }

  test("defaults: json4s' Writer (AsJson4s._)") {
    import AsJson4s._
    import org.json4s.DefaultJsonFormats._
    val o = ProfileSpec.body(ProfileSpec.render(J.schema[WithDefault], Draft07("id"))(profile))
    ProfileSpec.prop(o, "data").asInstanceOf[V.obj].value("default") shouldBe V.num(5)
    ProfileSpec.prop(o, "obj").asInstanceOf[V.obj].value("default") shouldBe V.obj("a" -> V.arr(V.num(1)))
  }
}

object AnyValueSpec {
  // derived here, where `AsJson4s.predef._` isn't imported
  val structuralJValue: _root_.json.Schema[JValue]   = J.schema[JValue]
  val structuralJObject: _root_.json.Schema[JObject] = J.schema[JObject]

  def accepts(s: _root_.json.Schema[_], doc: String): Boolean = JsonValidator.errors(Draft07("https://e2.test/s.json"), s.draft07("https://e2.test/s.json"), doc).isEmpty

  case class Payload(id: String, data: JValue, opt: Option[JValue], obj: JObject, arr: JArray, s: JString, n: JDouble, b: JBool)

  case class Small(id: String, data: JValue)

  case class WithDefault(data: JValue = JInt(5), obj: JObject = JObject("a" -> JArray(List(JInt(1)))))
}
