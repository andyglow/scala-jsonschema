package com.github.andyglow.jsonschema

import _root_.json.{Json => J}
import _root_.json.Schema._
import _root_.json.schema.JsonSchemaProfile
import com.github.andyglow.json.{Value => V}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._
import petshop.Acceptance

/** ujson's AST types with `import AsU.predef._` (design doc `docs/design/295-any-value.md`). */
class AnyValueSpec extends AnyFunSuite {
  import AnyValueSpec._
  import AsU.predef._

  private val profile: JsonSchemaProfile = UPickleProfile.default

  test("the trees") {
    J.schema[ujson.Value] shouldBe `any`[ujson.Value]()
    J.schema[ujson.Obj] shouldBe `object`.Free[ujson.Obj]()
    J.schema[ujson.Arr] shouldBe `array`[ujson.Value, Seq](`any`[ujson.Value]())
    J.schema[ujson.Str] shouldBe `string`[ujson.Str]
    J.schema[ujson.Num] shouldBe `number`[Double]
    J.schema[ujson.Bool] shouldBe `boolean`
  }

  test("as fields: inlined") {
    val fields = J.schema[Payload].asInstanceOf[`object`[_]].fields.map(f => f.name -> f.tpe).toMap
    fields("data") shouldBe `any`[ujson.Value]()
    fields("obj") shouldBe `object`.Free[ujson.Obj]()
  }

  test("validated: what ujson writes") {
    val render                                                   = Acceptance.rendered(J.schema[Payload])(profile)
    def doc(data: ujson.Value, opt: Option[ujson.Value]): String = {
      val o = ujson.Obj("id" -> "1", "data" -> data, "obj" -> ujson.Obj("a" -> ujson.Arr(1, "x")), "arr" -> ujson.Arr(ujson.Null, 2), "s" -> "s", "n" -> 1.5, "b" -> true)
      o("opt") = opt.getOrElse(ujson.Null) // upickle writes the key of a `None` too, as null (the profile requires it)
      o.render()
    }
    Acceptance.assertValid(render, List(doc(ujson.Num(5), None), doc(ujson.Null, Some(ujson.Null)), doc(ujson.Arr(ujson.True), Some(ujson.Str("o")))))
    Acceptance.assertInvalid(render, List(ujson.Obj("id" -> "1", "data" -> 1, "obj" -> 5, "arr" -> ujson.Arr(), "s" -> "s", "n" -> 1, "b" -> true).render()))
  }

  test("defaults: upickle's Writer (AsU._)") {
    import AsU._
    val o = ProfileSpec.body(ProfileSpec.render(J.schema[WithDefault], _root_.json.schema.Version.Draft07("id"))(profile))
    ProfileSpec.prop(o, "data").asInstanceOf[V.obj].value("default") shouldBe V.num(5)
    ProfileSpec.prop(o, "obj").asInstanceOf[V.obj].value("default") shouldBe V.obj("a" -> V.arr(V.num(1)))
  }
}

object AnyValueSpec {
  case class Payload(id: String, data: ujson.Value, opt: Option[ujson.Value], obj: ujson.Obj, arr: ujson.Arr, s: ujson.Str, n: ujson.Num, b: ujson.Bool)

  case class WithDefault(data: ujson.Value = ujson.Num(5), obj: ujson.Obj = ujson.Obj("a" -> ujson.Arr(1)))
}
