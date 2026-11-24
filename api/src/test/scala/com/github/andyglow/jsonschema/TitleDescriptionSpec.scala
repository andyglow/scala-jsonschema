package com.github.andyglow.jsonschema

import com.github.andyglow.json.Value
import com.github.andyglow.json.Value._
import json.Json
import json.Schema
import json.Schema._
import json.Schema.`object`.Field
import json.schema.{description, title, JsonSchemaProfile}
import json.schema.Version._
import json.schema.validation.Instance._
import scala.annotation.meta.field
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

/** B1 (#268, #290): `title` and `description` render on every kind of schema, not only on objects and consts, and `@title` on a field renders on its property. */
class TitleDescriptionSpec extends AnyWordSpec {
  import TitleDescriptionSpec._

  private val drafts: List[(String, Schema[_] => obj)] = List(
    "draft-04"      -> (s => AsValue.schema(s, Draft04())),
    "draft-06"      -> (s => AsValue.schema(s, Draft06("id"))),
    "draft-07"      -> (s => AsValue.schema(s, Draft07("id"))),
    "draft-2019-09" -> (s => AsValue.schema(s, Draft09("id"))),
    "draft-2020-12" -> (s => AsValue.schema(s, Draft12("id")))
  )

  private def at(v: Value, path: String*): Option[Value] = path.foldLeft(Option(v)) {
    case (Some(o: obj), k) => o.value.get(k)
    case _                 => None
  }

  /** `title` and `description` of the node at `path`, in every draft. */
  private def texts(s: Schema[_], path: String*): List[(String, Option[Value], Option[Value])] = drafts map { case (d, render) =>
    val node = at(render(s), path: _*)
    (d, node.flatMap(at(_, "title")), node.flatMap(at(_, "description")))
  }

  private def expectTexts(s: Schema[_], path: String*)(t: String, d: String): Unit =
    texts(s, path: _*) foreach { case (draft, ti, de) =>
      withClue(s"$draft: ") {
        (ti, de) shouldBe ((Some(str(t)), Some(str(d))))
      }
    }

  "the derived tree" should {

    "carry the family's @title/@description after lowering (only the renderer drops them)" in {
      val color = JsonSchemaProfile.Neutral.lower(Json.schema[TdColor])
      (color.title, color.description) shouldBe ((Some("Color"), Some("A color")))
      val shape = JsonSchemaProfile.Neutral.lower(Json.schema[TdShape])
      (shape.title, shape.description) shouldBe ((Some("Shape"), Some("A shape")))
    }
  }

  "title and description" should {

    "render on a string" in {
      expectTexts(Json.schema[String].withTitle("T").withDescription("D"))("T", "D")
    }

    "render on an integer, a number and a boolean" in {
      expectTexts(Json.schema[Int].withTitle("T").withDescription("D"))("T", "D")
      expectTexts(Json.schema[Double].withTitle("T").withDescription("D"))("T", "D")
      expectTexts(Json.schema[Boolean].withTitle("T").withDescription("D"))("T", "D")
    }

    "render on an array" in {
      expectTexts(Json.schema[List[Int]].withTitle("T").withDescription("D"))("T", "D")
    }

    "render on a map" in {
      expectTexts(Json.schema[Map[String, Int]].withTitle("T").withDescription("D"))("T", "D")
    }

    "render on an Option (lowered to string-or-null)" in {
      expectTexts(Json.schema[Option[String]].withTitle("T").withDescription("D"))("T", "D")
    }

    "render on a value class" in {
      expectTexts(Json.schema[TdId].withTitle("T").withDescription("D"))("T", "D")
    }

    "render on a derived enum (sealed family of case objects) from @title/@description" in {
      expectTexts(Json.schema[TdColor])("Color", "A color")
    }

    "render on a derived oneOf (sealed family of case classes) from @title/@description" in {
      expectTexts(Json.schema[TdShape])("Shape", "A shape")
    }

    "render @title on a field (#290)" in {
      expectTexts(Json.schema[TdPerson], "properties", "name")("Name", "The name")
    }

    "render a field's own type title (#290)" in {
      expectTexts(Json.schema[TdBox], "properties", "items")("Items", "The items")
    }

    "render on an enum, allOf, not and oneOf members built by hand" in {
      expectTexts(`enum`.of("a", "b").withTitle("T").withDescription("D"))("T", "D")
      expectTexts(`allof`.of(`string`[String](None), `string`[String](None).withValidation(`minLength` := 1)).withTitle("T").withDescription("D"))("T", "D")
      expectTexts(Schema.`not`(`string`[String](None)).withTitle("T").withDescription("D"))("T", "D")
      val members = drafts map { case (d, render) => d -> render(`oneof`.of(`string`[String](None).withTitle("S"), `integer`.withTitle("I"))) }
      members foreach { case (d, out) =>
        withClue(s"$d: ") {
          at(out, "oneOf").collect { case a: arr => a.value.flatMap(at(_, "title")).toSet } shouldBe Some(Set(str("S"), str("I")))
        }
      }
    }

    "render a definition's and a ref's own texts next to their ref" in {
      val s = `object`[Any](
        Field("d", `string`[String](None).toDefinition("dd").withTitle("DT").withDescription("DD")),
        Field("r", `ref`[Any]("dd").withTitle("RT").withDescription("RD"))
      )
      expectTexts(s, "properties", "d")("DT", "DD")
      expectTexts(s, "properties", "r")("RT", "RD")
      drafts foreach { case (d, render) =>
        withClue(s"$d: ") {
          definitionsOf(render(s)).values.flatMap(at(_, "title")) shouldBe empty
        }
      }
    }

    "let a value class's own texts win over its underlying type's" in {
      val s = `value-class`[TdId, String](`string`[String](None).withTitle("inner").withDescription("inner d")).withTitle("outer")
      expectTexts(s)("outer", "inner d")
    }

    "render on the definition of a value class given as an implicit Schema" in {
      drafts foreach { case (d, render) =>
        withClue(s"$d: ") {
          val out  = render(Json.schema[TdHasVC])
          val defs = definitionsOf(out).values.toList
          defs.map(v => (at(v, "type"), at(v, "title"), at(v, "description"))) shouldBe List((Some(str("string")), Some(str("VC T")), Some(str("VC D"))))
        }
      }
    }

    "render a field @title on an Option field" in {
      expectTexts(Json.schema[TdOpt], "properties", "o")("O", "OD")
    }

    "read @(title @field)" in {
      texts(Json.schema[TdFieldTarget], "properties", "a") foreach { case (d, t, _) => withClue(s"$d: ")(t shouldBe Some(str("FT"))) }
    }

    "keep a field @title under Json.objectSchema" in {
      expectTexts(Json.objectSchema[TdPerson]("name" -> "from objectSchema"), "properties", "name")("Name", "from objectSchema")
    }

    "put a field @title next to the ref of a definition-typed field, not into the definition" in {
      drafts foreach { case (d, render) =>
        withClue(s"$d: ") {
          val out = render(Json.schema[TdTwo])
          at(out, "properties", "a", "title") shouldBe Some(str("First"))
          at(out, "properties", "a", Ref) shouldBe defined
          at(out, "properties", "b") shouldBe at(out, "properties", "a").map(_.asInstanceOf[obj] - "title")
          val defs = definitionsOf(out)
          defs.size shouldBe 1
          defs.values.flatMap(at(_, "title")).toList shouldBe List(str("Inner"))

          val both = render(Json.schema[TdBoth])
          at(both, "properties", "inner", Ref) shouldBe defined
        }
      }
      expectTexts(Json.schema[TdBoth], "properties", "inner")("FT", "FD")
    }

    "put a field @title of a recursive type on the field, not on its items" in {
      drafts foreach { case (d, render) =>
        withClue(s"$d: ") {
          // a recursive root is a definition: its body holds the properties
          val body = definitionsOf(render(Json.schema[TdNode])).values.toList
          body.size shouldBe 1
          at(body.head, "properties", "kids", "title") shouldBe Some(str("kids"))
          at(body.head, "properties", "kids", "items", Ref) shouldBe defined
          at(body.head, "properties", "kids", "items", "title") shouldBe None
        }
      }
    }
  }

  "under JsonSchemaProfile.Legacy" should {

    def legacy(s: Schema[_]): obj = {
      implicit val profile: JsonSchemaProfile = JsonSchemaProfile.Legacy
      AsValue.schema(s, Draft07("id"))
    }

    def textsOf(v: Option[Value]): (Option[Value], Option[Value]) = (v.flatMap(at(_, "title")), v.flatMap(at(_, "description")))

    // lead policy 2026-11-23: Legacy keeps 0.7's style, not its bugs; 0.7 dropped these texts
    "render them too, on every kind of schema" in {
      textsOf(Some(legacy(Json.schema[String].withTitle("T").withDescription("D")))) shouldBe ((Some(str("T")), Some(str("D"))))
      textsOf(Some(legacy(Json.schema[List[Int]].withTitle("T").withDescription("D")))) shouldBe ((Some(str("T")), Some(str("D"))))
      textsOf(Some(legacy(Json.schema[TdColor]))) shouldBe ((Some(str("Color")), Some(str("A color"))))
      textsOf(Some(legacy(Json.schema[TdId].withTitle("T").withDescription("D")))) shouldBe ((Some(str("T")), Some(str("D"))))
      textsOf(at(legacy(Json.schema[TdPerson]), "properties", "name")) shouldBe ((Some(str("Name")), Some(str("The name"))))
      definitionsOf(legacy(Json.schema[TdHasVC])).values.toList.map(v => textsOf(Some(v))) shouldBe List((Some(str("VC T")), Some(str("VC D"))))
    }

    "keep the texts of objects (and field descriptions)" in {
      val out = legacy(Json.schema[TdPerson].withTitle("P").withDescription("PD"))
      at(out, "title") shouldBe Some(str("P"))
      at(out, "description") shouldBe Some(str("PD"))
      at(out, "properties", "name", "description") shouldBe Some(str("The name"))
    }
  }

  private val Ref = f"$$ref"

  private def definitionsOf(out: obj): Map[String, Value] =
    (at(out, "definitions").toList ++ at(out, f"$$defs").toList).collect { case o: obj => o.value.toMap }.flatten.toMap
}

object TitleDescriptionSpec {

  case class TdId(value: String) extends AnyVal

  @title("Color")
  @description("A color")
  sealed trait TdColor
  object TdColor {
    case object Red  extends TdColor
    case object Blue extends TdColor
  }

  @title("Shape")
  @description("A shape")
  sealed trait TdShape
  object TdShape {
    final case class Circle(r: Double) extends TdShape
    final case class Square(a: Double) extends TdShape
  }

  final case class TdPerson(@title("Name") @description("The name") name: String)

  implicit val tdItems: json.schema.Predef[List[String]] = json.schema.Predef(Json.schema[List[String]].withTitle("Items").withDescription("The items"))

  final case class TdBox(items: List[String])

  @title("VC T")
  @description("VC D")
  final case class TdVC(v: String) extends AnyVal
  object TdVC {
    implicit val schema: Schema[TdVC] = Json.schema[TdVC]
  }
  final case class TdHasVC(vc: TdVC)

  final case class TdOpt(@title("O") @description("OD") o: Option[String])

  final case class TdFieldTarget(@(title @field)("FT") a: Int)

  @title("Inner")
  final case class TdInner(x: Int)
  object TdInner {
    implicit val schema: Schema[TdInner] = Json.schema[TdInner]
  }
  final case class TdTwo(@title("First") a: TdInner, b: TdInner)
  final case class TdBoth(@title("FT") @description("FD") inner: TdInner)

  final case class TdNode(@title("kids") kids: List[TdNode])
}
