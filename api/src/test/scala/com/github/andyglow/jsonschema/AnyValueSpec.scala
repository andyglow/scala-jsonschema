package com.github.andyglow.jsonschema

import com.github.andyglow.json.Value
import com.github.andyglow.json.Value._
import com.github.andyglow.jsonschema.harness.Golden
import json._
import json.Schema._
import json.schema.JsonSchemaProfile
import json.schema.Version._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._
import petshop.Acceptance

/** The "any JSON value" schema, `` Schema.`any` `` (`{}`), and our own JSON AST's Predefs (design doc `docs/design/295-any-value.md`, #295, #289, #286). The bridges' AST types are in each bridge's
  * `AnyValueSpec`. The validating tests use the acceptance harness' validator (JVM; canceled on Scala.js).
  */
class AnyValueSpec extends AnyFunSuite {
  import AnyValueSpec._
  import ProfileSpec.{body, drafts, prop, render, required}

  private val neutral = JsonSchemaProfile.Neutral

  test("the node: equality, copies keep it an any, toString / toDebugString") {
    `any`[Int]() shouldBe `any`[String]()
    `any`[Int]().hashCode shouldBe `any`[String]().hashCode
    assert(`any`[Int]().withDescription("d") != `any`[Int]())
    `any`[Int]().withDescription("d").duplicate() shouldBe `any`[Int]().withDescription("d")
    assert(`any`[Int]() != `object`.Free[Int]())
    `any`[Int]().toString shouldBe "any"
    `any`[Int]().toDebugString shouldBe "any"
  }

  test("our Value and its kinds: core Predefs, no import") {
    Json.schema[Value] shouldBe `any`[Value]()
    Json.schema[Value.obj] shouldBe `object`.Free[Value.obj]()
    Json.schema[Value.arr] shouldBe `array`[Value, Seq](`any`[Value]())
    Json.schema[Value.str] shouldBe `string`[Value.str]
    Json.schema[Value.num] shouldBe `number`[BigDecimal]
    Json.schema[Value.bool] shouldBe `boolean`
    Json.schema[Value.`null`.type] shouldBe Schema.`null`
  }

  test("as fields: inlined, never a definition") {
    val s = Json.schema[Payload]
    s.asInstanceOf[`object`[_]].fields.map(f => f.name -> f.tpe).toMap.apply("data") shouldBe `any`[Value]()
    val o = body(render(s, Draft07("id"))(neutral))
    prop(o, "data") shouldBe obj()
    prop(o, "meta") shouldBe obj("type" -> "object", "additionalProperties" -> true)
    prop(o, "list") shouldBe obj("type" -> "array", "items" -> obj())
    prop(o, "wrapped") shouldBe obj() // a value class over `Value`: no "type": "any"
    o.value.get("definitions") shouldBe None
    required(o) shouldBe Set[Value](str("id"), str("data"), str("meta"), str("list"), str("map"), str("wrapped"))
  }

  test("rendered: {} in every draft and under every profile") {
    for {
      d <- drafts
      p <- List(neutral, JsonSchemaProfile.Legacy, neutral.withOptionKeyRequired(true))
    } body(render(`any`[Value](), d)(p)) shouldBe obj()
    // the `not` node itself writes `"type": "not"` (pre-existing, every `not`; reported to the lead): only its body is E2's
    body(render(Schema.`not`(`any`[Value]()), Draft07("id"))(neutral)).value("not") shouldBe obj()
  }

  test("Option of an any is the any itself (oneOf[{}, null] would reject null)") {
    `option`(`any`[Value]()).withProfile(neutral) shouldBe `any`[Value]()
    `option`(`value-class`[Wrapped, Value](`any`[Value]())).withProfile(neutral) shouldBe `value-class`[Wrapped, Value](`any`[Value]())
    val s = Json.schema[Payload]
    prop(body(render(s, Draft07("id"))(neutral)), "opt") shouldBe obj()
    assert(!required(body(render(s, Draft07("id"))(neutral))).contains(str("opt")))
    required(body(render(s, Draft07("id"))(neutral.withOptionKeyRequired(true)))) should contain(str("opt"))
    prop(body(render(s, Draft07("id"))(JsonSchemaProfile.Legacy)), "opt") shouldBe obj()
  }

  test("Option of a definition of an any: the $ref alone") {
    import WithDefinition._
    val s = Json.schema[UsesDef]
    s.withProfile(neutral).asInstanceOf[`object`[_]].fields.find(_.name == "opt").get.tpe match {
      case `def`(_, t) => t shouldBe `any`[Value]()
      case other       => fail(s"not a definition: $other")
    }
    val o = render(s, Draft07("id"))(neutral)
    prop(body(o), "opt").asInstanceOf[obj].value.keySet shouldBe Set("$ref")
  }

  test("a hand-built oneof{any, null} renders the any alone") {
    body(render(`oneof`[Any](Set(`any`[Any](), Schema.`null`)), Draft07("id"))(neutral)) shouldBe obj()
  }

  test("oneOf / allOf / enum with an any member never state \"type\": \"any\"") {
    body(render(`oneof`[Any](Set(`string`[String], `any`[Any]())), Draft07("id"))(neutral)) shouldBe obj("oneOf" -> arr(obj("type" -> "string"), obj()))
    val withDef = body(render(`oneof`[Any](Set(`def`[Any]("A", `any`[Any]()), `string`[String])), Draft07("id"))(neutral))
    withDef.value.get("type") shouldBe None
    withDef.value("oneOf").asInstanceOf[arr].value(1) shouldBe obj("type" -> "string")
    val allOf = `object`[Any](`object`.Field("a", `allof`[Any](Set(`any`[Any](), `string`[String]))))
    prop(body(render(allOf, Draft07("id"))(neutral)), "a") shouldBe obj("type" -> "string", "allOf" -> arr(obj(), obj()))
    body(render(`enum`[Any](`any`[Any](), Set[Value](num(1))), Draft07("id"))(neutral)) shouldBe obj("enum" -> arr(num(1)))
  }

  test("a default of type Value (the identity ToValue)") {
    prop(body(render(Json.schema[WithDefault], Draft07("id"))(neutral)), "data") shouldBe obj("default" -> obj("a" -> 1))
  }

  test("validated: {} accepts every value, Option[Value] accepts null") {
    Acceptance.assertValid(Acceptance.rendered(`any`[Value]())(neutral), List("1", "\"s\"", "null", "[]", "{}", "true", "[1, {\"a\": null}]"))
    val payload = Acceptance.rendered(Json.schema[Payload])(neutral)
    Acceptance.assertValid(
      payload,
      List(
        """{"id": "1", "data": 5, "meta": {"a": [1]}, "list": [1, "a", null], "map": {"k": {"x": 1}}, "wrapped": "w"}""",
        """{"id": "1", "data": null, "opt": null, "meta": {}, "list": [], "map": {}, "wrapped": null}"""
      )
    )
    Acceptance.assertInvalid(payload, List("""{"id": "1", "data": 5, "meta": 5, "list": [], "map": {}, "wrapped": 1}"""))
    import WithDefinition._
    Acceptance.assertValid(Acceptance.rendered(Json.schema[UsesDef])(neutral), List("""{"opt": null}""", """{"opt": [1]}""", "{}"))
    Acceptance.assertValid(Acceptance.rendered(Json.schema[UsesDef])(neutral.withOptionKeyRequired(true)), List("""{"opt": null}"""))
  }

  test("golden") {
    Golden.check("shared/AnyValueSpec.Payload", Json.schema[Payload])
  }
}

object AnyValueSpec {

  case class Wrapped(value: Value) extends AnyVal

  case class Payload(id: String, data: Value, opt: Option[Value], meta: Value.obj, list: List[Value], map: Map[String, Value], wrapped: Wrapped)

  case class WithDefault(data: Value = Value.obj("a" -> 1))

  object WithDefinition {
    implicit val valueSchema: Schema[Value] = Schema.`any`[Value]()

    case class UsesDef(opt: Option[Value])
  }
}
