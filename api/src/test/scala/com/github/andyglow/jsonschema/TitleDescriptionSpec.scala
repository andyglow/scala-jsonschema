package com.github.andyglow.jsonschema

import com.github.andyglow.json.Value
import com.github.andyglow.json.Value._
import json.Json
import json.Schema
import json.schema.{description, title, JsonSchemaProfile}
import json.schema.Version._
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
  }

  "under JsonSchemaProfile.Legacy" should {

    "stay as in 0.7: no title or description on a non-object" in {
      implicit val profile: JsonSchemaProfile = JsonSchemaProfile.Legacy
      val s                                   = Json.schema[String].withTitle("T").withDescription("D")
      val out                                 = AsValue.schema(s, Draft07("id"))
      at(out, "title") shouldBe None
      at(out, "description") shouldBe None
    }
  }
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
    final case class Circle(r: Double)  extends TdShape
    final case class Square(a: Double) extends TdShape
  }

  final case class TdPerson(@title("Name") @description("The name") name: String)

  implicit val tdItems: json.schema.Predef[List[String]] = json.schema.Predef(Json.schema[List[String]].withTitle("Items").withDescription("The items"))

  final case class TdBox(items: List[String])
}
