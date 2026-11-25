package com.github.andyglow.jsonschema

import _root_.json.{Json => J}
import _root_.json.Schema._
import _root_.json.schema.JsonSchemaProfile
import com.github.andyglow.json.{Value => V}
import io.circe.{Json, JsonNumber, JsonObject}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._
import petshop.Acceptance

/** circe's AST types with `import AsCirce.predef._` (design doc `docs/design/295-any-value.md`). */
class AnyValueSpec extends AnyFunSuite {
  import AnyValueSpec._
  import AsCirce.predef._

  private val profile: JsonSchemaProfile = CirceProfile.default

  test("the trees") {
    J.schema[Json] shouldBe `any`[Json]()
    J.schema[JsonObject] shouldBe `object`.Free[JsonObject]()
    J.schema[JsonNumber] shouldBe `number`[BigDecimal]
  }

  test("as fields: inlined") {
    val fields = J.schema[Payload].asInstanceOf[`object`[_]].fields.map(f => f.name -> f.tpe).toMap
    fields("data") shouldBe `any`[Json]()
    fields("obj") shouldBe `object`.Free[JsonObject]()
  }

  test("validated: what circe writes") {
    val render                                     = Acceptance.rendered(J.schema[Payload])(profile)
    def doc(data: Json, opt: Option[Json]): String =
      Json
        .fromFields(
          List("id" -> Json.fromString("1"), "data" -> data, "obj" -> Json.obj("a" -> Json.arr(Json.fromInt(1))), "n" -> Json.fromDoubleOrNull(1.5)) ++ opt.map("opt" -> _).toList
        )
        .noSpaces
    Acceptance.assertValid(render, List(doc(Json.fromInt(5), None), doc(Json.Null, Some(Json.Null)), doc(Json.arr(Json.True), Some(Json.fromString("o")))))
    Acceptance.assertInvalid(render, List(Json.obj("id" -> Json.fromString("1"), "data" -> Json.Null, "obj" -> Json.fromInt(5), "n" -> Json.fromInt(1)).noSpaces))
  }

  test("validated: a Payload written by circe's deriveEncoder") {
    val render = Acceptance.rendered(J.schema[Payload])(profile)
    val sample = Payload("1", Json.obj("x" -> Json.arr(Json.Null)), None, JsonObject("a" -> Json.fromInt(1)), JsonNumber.fromDecimalStringUnsafe("1.5"))
    Acceptance.assertValid(render, List(payloadEncoder(sample).noSpaces, payloadEncoder(sample.copy(data = Json.Null, opt = Some(Json.Null))).noSpaces))
  }

  test("defaults: circe's Encoder (AsCirce._)") {
    import AsCirce._
    val o = ProfileSpec.body(ProfileSpec.render(J.schema[WithDefault], _root_.json.schema.Version.Draft07("id"))(profile))
    ProfileSpec.prop(o, "data").asInstanceOf[V.obj].value("default") shouldBe V.num(5)
    ProfileSpec.prop(o, "obj").asInstanceOf[V.obj].value("default") shouldBe V.obj("a" -> V.arr(V.num(1)))
  }
}

object AnyValueSpec {
  case class Payload(id: String, data: Json, opt: Option[Json], obj: JsonObject, n: JsonNumber)
  val payloadEncoder: io.circe.Encoder[Payload] = io.circe.generic.semiauto.deriveEncoder[Payload]

  case class WithDefault(data: Json = Json.fromInt(5), obj: JsonObject = JsonObject("a" -> Json.arr(Json.fromInt(1))))
}
