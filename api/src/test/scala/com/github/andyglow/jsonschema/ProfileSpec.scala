package com.github.andyglow.jsonschema

import com.github.andyglow.json.{JsonFormatter, Value}
import com.github.andyglow.json.Value._
import json._
import json.Schema._
import json.Schema.`object`.Field
import json.schema.{JsonSchemaProfile, Predef, Version}
import json.schema.Version._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

object ProfileSpec {
  case class Bar(x: Int)
  case class Ref(v: String)
  object Ref {
    implicit val refSchema: Schema[Ref] = Json.schema[Ref]
  }
  sealed trait Kind
  object Kind {
    case object A extends Kind
    case object B extends Kind
  }
  case class Pet(
    name: String,
    age: Option[Int],
    tags: List[Option[String]],
    meta: Map[String, Option[Int]],
    nested: Option[Option[Int]],
    bar: Option[Bar],
    kind: Option[Kind],
    ref: Option[Ref],
    d2: Option[Int] = None
  )

  /** The 0.7 tree of a non-field `Option[X]`. */
  def legacyOption(x: Schema[_]): Schema[_] =
    `oneof`[Any](Set(`object`.fromList[Any](List(Field("value", x, required = true))), `enum`[Any](`string`, Set[Value](str("None")))))

  val drafts: List[Version] = List(Draft04(), Draft06("id"), Draft07("id"), Draft09("id"), Draft12("id"))

  def render(s: Schema[_], v: Version)(implicit p: JsonSchemaProfile): obj = v match {
    case v: Draft04 => AsValue.schema(s, v)
    case v: Draft06 => AsValue.schema(s, v)
    case v: Draft07 => AsValue.schema(s, v)
    case v: Draft09 => AsValue.schema(s, v)
    case v: Draft12 => AsValue.schema(s, v)
    case _          => AsValue.schema(s, Raw)
  }

  /** The root schema, without `$schema`/`$id`/definitions. */
  def body(o: obj): obj = o - f"$$schema" - f"$$id" - "definitions" - f"$$defs"

  def prop(o: obj, name: String): Value = o.value("properties").asInstanceOf[obj].value(name)

  def required(o: obj): Set[Value] = o.value.get("required").fold(Set.empty[Value])(_.asInstanceOf[arr].value.toSet)

  def nullableTypes(x: String): Value = arr(str(x), str("null"))
}

class ProfileSpec extends AnyFunSuite {
  import ProfileSpec._

  private val pet = Json.schema[Pet]

  test("the derived tree keeps every Option as an `option` node (fields: not required; nesting kept)") {
    val fields = pet.asInstanceOf[`object`[_]].fields.map(f => f.name -> (f: Field[_])).toMap[String, Field[_]]
    fields("age") shouldEqual Field("age", `option`(`integer`), required = false)
    fields("nested") shouldEqual Field("nested", `option`(`option`(`integer`)), required = false)
    fields("tags").tpe shouldEqual `array`[Option[String], List](`option`(`string`))
    fields("meta").tpe shouldEqual `dictionary`[String, Option[Int], Map](`option`(`integer`))
    fields("d2") shouldEqual Field("d2", `option`(`integer`), required = false).withHasDefault(true) // `= None`: a default with no JSON form
    Json.schema[Option[Int]] shouldEqual `option`(`integer`)
    Json.schema[Option[Option[Bar]]] shouldEqual `option`(`option`(Json.schema[Bar]))
    pet.toDebugString should include("option(")
  }

  test("Neutral (the default): every Option[X] is X or null, in every position and draft") {
    drafts foreach { v =>
      val o = render(pet, v)
      prop(o, "age") shouldEqual obj("type" -> nullableTypes("integer"))
      prop(o, "nested") shouldEqual obj("type" -> nullableTypes("integer"))
      prop(o, "d2") shouldEqual obj("type" -> nullableTypes("integer")) // `= None`: no default, as in 0.7
      prop(o, "tags") shouldEqual obj("type" -> "array", "items" -> obj("type" -> nullableTypes("string")))
      prop(o, "meta") shouldEqual obj("type" -> "object", "patternProperties" -> obj("^[\\s\\S]*$" -> obj("type" -> nullableTypes("integer"))))
      prop(o, "bar").asInstanceOf[obj].value("type") shouldEqual nullableTypes("object")
      // an enum keeps the oneOf form (a type array would let null through `type` but `enum` still rejects it)
      prop(o, "kind") shouldEqual obj("oneOf" -> arr(obj("type" -> "string", "enum" -> arr("A", "B")), obj("type" -> "null")))
      // a definition keeps the oneOf form (keywords next to $ref are ignored before 2019-09)
      val refO = prop(o, "ref").asInstanceOf[obj].value("oneOf").asInstanceOf[arr].value
      refO.last shouldEqual obj("type" -> "null")
      refO.head.asInstanceOf[obj].keys shouldEqual Set(f"$$ref")
      required(o) shouldEqual Set[Value](str("name"), str("tags"), str("meta"))

      body(render(Json.schema[Option[Int]], v)) shouldEqual obj("type" -> nullableTypes("integer"))
      body(render(Json.schema[List[Option[Int]]], v)) shouldEqual obj("type" -> "array", "items" -> obj("type" -> nullableTypes("integer")))
    }
  }

  test("no profile in scope means Neutral, found in the implicit scope") {
    implicitly[JsonSchemaProfile] shouldBe JsonSchemaProfile.Neutral
    AsValue.schema(pet, Draft07("id")) shouldEqual AsValue.schema(pet, Draft07("id"))(implicitly, JsonSchemaProfile.Neutral)
  }

  test("Legacy: the 0.7 trees") {
    val legacy = pet.withProfile(JsonSchemaProfile.Legacy).asInstanceOf[`object`[_]].fields.map(f => f.name -> (f.tpe: Schema[_])).toMap[String, Schema[_]]
    legacy("age") shouldEqual `integer`
    legacy("nested") shouldEqual legacyOption(`integer`) // a field drops one layer
    legacy("tags") shouldEqual `array`[Any, List](legacyOption(`string`).asInstanceOf[Schema[Any]])
    Json.schema[Option[Int]].withProfile(JsonSchemaProfile.Legacy) shouldEqual legacyOption(`integer`)
  }

  test("Legacy renders the 0.7 member order (the goldens sort arrays, so this is checked here)") {
    val none                                = obj("type" -> "string", "enum" -> arr("None"))
    val value                               = obj("type" -> "object", "additionalProperties" -> false, "properties" -> obj("value" -> obj("type" -> "integer")), "required" -> arr("value"))
    implicit val profile: JsonSchemaProfile = JsonSchemaProfile.Legacy
    // arr equality is ordered: 0.7 (Scala 2) rendered enum["None"] first
    body(AsValue.schema(Json.schema[Option[Int]], Draft07("id"))) shouldEqual obj("oneOf" -> arr(none, value))
    body(AsValue.schema(Json.schema[List[Option[Int]]], Draft07("id"))) shouldEqual obj("type" -> "array", "items" -> obj("oneOf" -> arr(none, value)))
    JsonFormatter.format(body(AsValue.schema(Json.schema[Option[Int]], Draft04()))) shouldEqual JsonFormatter.format(obj("oneOf" -> arr(none, value)))
  }

  test("Legacy as the implicit profile where the schema is rendered, or per schema with withProfile") {
    val expected = AsValue.schema(pet.withProfile(JsonSchemaProfile.Legacy), Draft07("id"))
    prop(expected, "age") shouldEqual obj("type" -> "integer")
    locally {
      implicit val profile: JsonSchemaProfile = JsonSchemaProfile.Legacy
      AsValue.schema(pet, Draft07("id")) shouldEqual expected
      Json.stringify(pet, Draft07("id")) shouldEqual JsonFormatter.format(expected)
      pet.draft07("id") shouldEqual JsonFormatter.format(expected)
      pet.stringify shouldEqual JsonFormatter.format(AsValue.schema(pet.withProfile(JsonSchemaProfile.Legacy), Raw))
    }
    locally {
      // an import of the companion next to a local implicit is not ambiguous
      import JsonSchemaProfile._
      implicit val profile: JsonSchemaProfile = Legacy
      AsValue.schema(pet, Draft07("id")) shouldEqual expected
    }
    (pet.draft07("id") == JsonFormatter.format(expected)) shouldBe false
  }

  test("lowering is idempotent, and a shaped schema keeps its shape under any later profile") {
    val profiles = List(JsonSchemaProfile.Neutral, JsonSchemaProfile.Legacy)
    for (p <- profiles; q <- profiles) {
      val once = pet.withProfile(p)
      once.withProfile(p) shouldEqual once
      once.withProfile(q) shouldEqual once
      AsValue.schema(once, Draft12("id"))(implicitly, q) shouldEqual AsValue.schema(pet, Draft12("id"))(implicitly, p)
    }
    val plain = Json.schema[Bar]
    plain.withProfile(JsonSchemaProfile.Neutral) shouldEqual plain
    plain.withProfile(JsonSchemaProfile.Legacy) shouldEqual plain
  }

  test("lowering keeps title, description, validations and walks into definitions") {
    val inner = `string`.withTitle("t").withDescription("d")
    `option`(inner).withProfile(JsonSchemaProfile.Neutral) shouldEqual `oneof`[Any](Set(inner, Schema.`null`))
    `option`(`integer`).withDescription("o").withProfile(JsonSchemaProfile.Neutral).description shouldBe Some("o")
    val d = `def`[Any]("D", `object`[Any](Field("o", `option`(`integer`), required = false)))
    d.withProfile(JsonSchemaProfile.Neutral) shouldEqual `def`[Any]("D", `object`[Any](Field("o", `oneof`[Any](Set(`integer`, Schema.`null`)), required = false)))
    // renaming a definition follows refs inside an option
    `def`[Any]("a", `object`[Any](Field("next", `option`(`ref`[Any]("a")), required = false))).toDefinition("b") shouldEqual
      `def`[Any]("b", `object`[Any](Field("next", `option`(`ref`[Any]("b")), required = false)))
  }

  test("a hand-written oneof{X, null} renders the same way") {
    drafts foreach { v =>
      body(render(`oneof`.of(`string`, Schema.`null`), v)) shouldEqual obj("type" -> nullableTypes("string"))
      body(render(`oneof`.of(`array`[Int, List](`integer`), Schema.`null`), v)) shouldEqual obj("type" -> nullableTypes("array"), "items" -> obj("type" -> "integer"))
      body(render(Schema.`null`, v)) shouldEqual obj("type" -> "null")
      body(render(`oneof`.of(`string`, `integer`, Schema.`null`), v)).value("oneOf").asInstanceOf[arr].value.last shouldEqual obj("type" -> "null")
    }
  }

  test("a raw tree handed to a renderer directly is rendered under Neutral") {
    new AsDraft07(Draft07("id")).schema(Json.schema[Option[Int]]) shouldEqual AsValue.schema(Json.schema[Option[Int]], Draft07("id"))
  }

  test("an implicit Predef or Schema of an Option wins over Option derivation (Schema vs Predef)") {
    locally {
      implicit val p: Predef[Option[Bar]] = Predef(`string`.asInstanceOf[Schema[Option[Bar]]])
      Json.schema[List[Option[Bar]]] shouldEqual `array`[Option[Bar], List](`string`.asInstanceOf[Schema[Option[Bar]]])
    }
    locally {
      implicit val s: Schema[Option[Bar]] = `string`.asInstanceOf[Schema[Option[Bar]]]
      Json.schema[List[Option[Bar]]].asInstanceOf[`array`[_, List]].componentType shouldBe a[`def`[_]]
    }
  }
}
