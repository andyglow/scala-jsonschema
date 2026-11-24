package com.github.andyglow.jsonschema

import com.github.andyglow.json.Value
import com.github.andyglow.json.Value._
import json._
import json.schema.discriminator
import json.schema.validation.Instance._
import json.schema.Version._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

/** Object keys come out in insertion order: `properties` in field declaration order, `required` likewise, definitions in the order they are met, keywords in the order the renderer writes them.
  * Scala's immutable `Map` keeps insertion order only up to 4 entries, so every case here has at least 5.
  */
object PropertyOrderSpec {

  final case class Six(a1: Int, a2: Int, a3: Int, a4: Int, a5: Int, a6: Int)

  final case class SixWithDefaults(a1: Int = 1, a2: Int = 2, a3: Int = 3, a4: Int = 4, a5: Int = 5, a6: Int = 6)

  @discriminator sealed trait Root
  object Root {
    final case class Member(a1: Int, a2: Int, a3: Int, a4: Int, a5: Int) extends Root
    final case class Other(b: String)                                    extends Root
  }

  final case class D1(x: Int)
  final case class D2(x: Int)
  final case class D3(x: Int)
  final case class D4(x: Int)
  final case class D5(x: Int)
  final case class D6(x: Int)
  final case class WithDefs(f1: D1, f2: D2, f3: D3, f4: D4, f5: D5, f6: D6)

  final case class WithMapDefault(m: Map[String, Int] = scala.collection.immutable.ListMap("e" -> 5, "d" -> 4, "c" -> 3, "b" -> 2, "a" -> 1))

  implicit val d1: Schema[D1] = Json.schema[D1]
  implicit val d2: Schema[D2] = Json.schema[D2]
  implicit val d3: Schema[D3] = Json.schema[D3]
  implicit val d4: Schema[D4] = Json.schema[D4]
  implicit val d5: Schema[D5] = Json.schema[D5]
  implicit val d6: Schema[D6] = Json.schema[D6]

  def keys(v: Value): List[String] = v match {
    case o: obj => o.fields.map(_._1).toList
    case _      => Nil
  }

  def field(v: Value, path: String*): Value = path.foldLeft(v) {
    case (o: obj, k) => o.value(k)
    case (x, _)      => x
  }

  def strings(v: Value): List[String] = v match {
    case a: arr => a.value.collect { case str(s) => s }.toList
    case _      => Nil
  }

  val Id = "order"

  val drafts: List[(String, Schema[_] => Value)] = List(
    "draft-04"      -> (s => AsValue.schema(s, Draft04())),
    "draft-06"      -> (s => AsValue.schema(s, Draft06(Id))),
    "draft-07"      -> (s => AsValue.schema(s, Draft07(Id))),
    "draft-2019-09" -> (s => AsValue.schema(s, Draft09(Id))),
    "draft-2020-12" -> (s => AsValue.schema(s, Draft12(Id)))
  )
}

class PropertyOrderSpec extends AnyFunSuite {
  import PropertyOrderSpec._

  private val six = (1 to 6).map(i => s"a$i").toList

  test("properties of a 6-field case class are in declaration order") {
    for ((d, render) <- drafts) {
      val v = render(Json.schema[Six])
      withClue(d)(keys(field(v, "properties")) shouldBe six)
    }
  }

  test("required of a 6-field case class is in declaration order") {
    for ((d, render) <- drafts) {
      val v = render(Json.schema[Six])
      withClue(d)(strings(field(v, "required")) shouldBe six)
    }
  }

  test("properties of a 6-field case class with defaults are in declaration order") {
    val v = AsValue.schema(Json.schema[SixWithDefaults], Draft07(Id))
    keys(field(v, "properties")) shouldBe six
  }

  test("properties of a Tuple5 are _1.._5") {
    val v = AsValue.schema(Json.schema[(Int, String, Boolean, Double, Long)], Draft07(Id))
    keys(field(v, "properties")) shouldBe List("_1", "_2", "_3", "_4", "_5")
    strings(field(v, "required")) shouldBe List("_1", "_2", "_3", "_4", "_5")
  }

  test("a discriminated member lists its fields in declaration order, then the discriminator") {
    val v      = AsValue.schema(Json.schema[Root], Draft07(Id))
    val member = field(v, "oneOf").asInstanceOf[arr].value.collect { case o: obj if keys(field(o, "properties")).contains("a1") => o }.head
    keys(field(member, "properties")) shouldBe List("a1", "a2", "a3", "a4", "a5", "type")
    strings(field(member, "required")) shouldBe List("a1", "a2", "a3", "a4", "a5", "type")
  }

  test("definitions are in the order they are met") {
    val v = AsValue.schema(Json.schema[WithDefs], Draft07(Id))
    keys(field(v, "definitions")).map(_.split('.').last) shouldBe List("D1", "D2", "D3", "D4", "D5", "D6")
  }

  test("a Map default keeps the order of the default value") {
    val v = AsValue.schema(Json.schema[WithMapDefault], Draft07(Id))
    keys(field(v, "properties", "m", "default")) shouldBe List("e", "d", "c", "b", "a")
  }

  test("validation keywords are in the order they were added") {
    val s = Json.schema[String].withValidation(`minLength` := 1, `maxLength` := 9, `pattern` := "^a", `contentEncoding` := "base64", `contentMediaType` := "text/plain")
    val v = AsValue.schema(s, Draft07(Id))
    keys(v).filterNot(k => k == "$schema" || k == "$id" || k == "type") shouldBe List("minLength", "maxLength", "pattern", "contentEncoding", "contentMediaType")
  }
}
