package com.github.andyglow.jsonschema

import java.util.concurrent.TimeUnit

import com.github.andyglow.json.Value
import com.github.andyglow.json.Value._
import com.github.andyglow.jsonschema.harness.CompileErrors.errorOf
import json._
import json.schema.{JsonSchemaProfile, Version => v}
import json.schema.JsonSchemaProfile.{Legacy, Neutral, OptionStyle, SumStyle}
import json.schema.validation.Instance._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

import scala.annotation.nowarn

/** Issue #481 (item E8): java.util collections and Optional, `Array[Byte]` / `Predef.bytesAsBase64`, `@deprecated`. */
@nowarn("cat=deprecation")
object MiscTypesSpec {
  case class User(name: String)

  // ---- java.util

  case class JavaColls(
    list: java.util.List[Int],
    set: java.util.Set[String],
    users: java.util.List[User],
    map: java.util.Map[String, User],
    opt: java.util.Optional[User]
  )
  case class ScalaColls(
    list: List[Int],
    set: Set[String],
    users: List[User],
    map: Map[String, User],
    opt: Option[User]
  )

  class Names extends java.util.ArrayList[String]

  case class JOpt(o: java.util.Optional[Int])
  case class SOpt(o: Option[Int])

  object UserSchema { implicit val userSchema: Schema[User] = Json.schema[User] }

  // ---- @deprecated

  case class Old(@deprecated("use b", "1.0") a: Int, b: Int)
  case class OldJava(@Deprecated a: Int, b: Int)
  case class OldOpt(@deprecated("use b", "1.0") a: Option[Int], b: Int)
  case class OldStr(@deprecated("use b", "1.0") s: String)

  case class Holder(@deprecated("use u", "1.0") old: User, u: User)

  @deprecated("use New", "1.0") case class OldType(a: Int)
  // Scala 3 reports the reference to a deprecated type in the macro's expansion at a position in the macro's source, which a `@nowarn` here doesn't cover;
  // a deprecated owner silences it (and the uses of this def are under the spec's `@nowarn`)
  @deprecated("a deprecated type", "1.0") def oldTypeSchema: Schema[OldType] = Json.schema[OldType]

  case class Bytes(data: Array[Byte])
}

@nowarn("cat=deprecation")
class MiscTypesSpec extends AnyFunSuite {
  import MiscTypesSpec._

  private val d07 = v.Draft07("x")
  private val d09 = v.Draft09("x")
  private val d12 = v.Draft12("x")

  private def at(x: Value, path: String*): Value = path.foldLeft(x) {
    case (o: obj, k) => o.value.getOrElse(k, fail(s"no '$k' in $o"))
    case (other, k)  => fail(s"'$k' looked up in a non-object $other")
  }
  private def has(x: Value, path: String*): Boolean = path.init.foldLeft(Option(x)) {
    case (Some(o: obj), k) => o.value.get(k)
    case _                 => None
  } exists {
    case o: obj => o.value.contains(path.last)
    case _      => false
  }

  // ---- java.util collections

  test("java.util collections render like their Scala counterparts (List, Set, Map, Optional; a derived element, a Map value)") {
    AsValue.schema(Json.schema[JavaColls], d12) shouldEqual AsValue.schema(Json.schema[ScalaColls], d12)
    AsValue.schema(Json.schema[JavaColls], v.Draft04()) shouldEqual AsValue.schema(Json.schema[ScalaColls], v.Draft04())
  }

  test("subtypes: ArrayList, LinkedList, ArrayDeque are arrays; HashSet, TreeSet are unique arrays; HashMap, TreeMap, LinkedHashMap are dictionaries") {
    val list = AsValue.schema(Json.schema[List[Int]], d12)
    AsValue.schema(Json.schema[java.util.ArrayList[Int]], d12) shouldEqual list
    AsValue.schema(Json.schema[java.util.LinkedList[Int]], d12) shouldEqual list
    AsValue.schema(Json.schema[java.util.ArrayDeque[Int]], d12) shouldEqual list
    AsValue.schema(Json.schema[java.util.Collection[Int]], d12) shouldEqual list
    val set = AsValue.schema(Json.schema[Set[String]], d12)
    AsValue.schema(Json.schema[java.util.HashSet[String]], d12) shouldEqual set
    AsValue.schema(Json.schema[java.util.TreeSet[String]], d12) shouldEqual set
    val map = AsValue.schema(Json.schema[Map[String, Int]], d12)
    AsValue.schema(Json.schema[java.util.HashMap[String, Int]], d12) shouldEqual map
    AsValue.schema(Json.schema[java.util.TreeMap[String, Int]], d12) shouldEqual map
    AsValue.schema(Json.schema[java.util.LinkedHashMap[String, Int]], d12) shouldEqual map
  }

  test("bounded type parameters: EnumSet[TimeUnit], EnumMap[TimeUnit, Int] (E8-R1)") {
    AsValue.schema(Json.schema[java.util.EnumSet[TimeUnit]], d12) shouldEqual AsValue.schema(Json.schema[Set[TimeUnit]], d12)
    AsValue.schema(Json.schema[java.util.EnumMap[TimeUnit, Int]], d12) shouldEqual AsValue.schema(Json.schema[Map[TimeUnit, Int]], d12)
  }

  test("a java collection without an element type parameter (class Names extends ArrayList[String]) is an array of its element") {
    AsValue.schema(Json.schema[Names], d12) shouldEqual AsValue.schema(Json.schema[List[String]], d12)
  }

  test("java.util.Map keys follow the Scala Map rules: a KeyPattern'd key gets its pattern, an unsupported key is the key error") {
    AsValue.schema(Json.schema[java.util.Map[Int, User]], d12) shouldEqual AsValue.schema(Json.schema[Map[Int, User]], d12)
    errorOf(assertCompiles("Json.schema[java.util.Map[java.util.UUID, Int]]")) should include("a Map key must be a String")
  }

  test("an element with an implicit Schema is a definition + $ref") {
    import UserSchema._
    val s = AsValue.schema(Json.schema[java.util.List[User]], d12)
    AsValue.schema(Json.schema[java.util.List[User]], d12) shouldEqual AsValue.schema(Json.schema[List[User]], d12)
    has(s, "items", "$ref") shouldBe true
    at(s, "$defs").asInstanceOf[obj].keys should have size 1
  }

  test("validations attach to a java collection schema like to a Scala one (magnets, E8-R5)") {
    val l = Json.schema[java.util.List[Int]].withValidation(`minItems` := 1)
    at(AsValue.schema(l, d12), "minItems") shouldEqual num(1)
    val m = Json.schema[java.util.Map[String, Int]].withValidation(`minProperties` := 1)
    at(AsValue.schema(m, d12), "minProperties") shouldEqual num(1)
  }

  // ---- java.util.Optional

  test("Optional renders like Option under every profile: root, nested, field (E8-R7)") {
    for (p <- List(Neutral, Legacy, Neutral.withOptionKeyRequired(true), Neutral.withSum(SumStyle.WrapperObject))) {
      implicit val profile: JsonSchemaProfile = p
      AsValue.schema(Json.schema[java.util.Optional[Int]], d12) shouldEqual AsValue.schema(Json.schema[Option[Int]], d12)
      AsValue.schema(Json.schema[java.util.Optional[java.util.Optional[Int]]], d12) shouldEqual AsValue.schema(Json.schema[Option[Option[Int]]], d12)
      AsValue.schema(Json.schema[JOpt], d12) shouldEqual AsValue.schema(Json.schema[SOpt], d12)
    }
  }

  test("an Optional field is not required (Neutral: nullable), required under optionKeyRequired") {
    val s = AsValue.schema(Json.schema[JOpt], d12)
    has(s, "required") shouldBe false
    at(s, "properties", "o", "type") shouldEqual arr("integer", "null")
    at(AsValue.schema(Json.schema[JOpt], d12)(implicitly[AsValueBuilder[v.Draft12]], Neutral.withOptionKeyRequired(true)), "required") shouldEqual arr("o")
  }

  // ---- Array[Byte]

  test("Array[Byte] stays an array of numbers") {
    at(AsValue.schema(Json.schema[Bytes], d12), "properties", "data") shouldEqual obj("type" -> "array", "items" -> obj("type" -> "number"))
  }

  test("Predef.bytesAsBase64: a base64 string on every draft") {
    val expected = obj(
      "type"            -> "string",
      "contentEncoding" -> "base64",
      "pattern"         -> "^(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=)?$"
    )
    for (ver <- List(v.Draft04(), v.Draft06("x"), d07, d09, d12)) {
      val s = ver match {
        case x: v.Draft04 => AsValue.schema(schema.Predef.bytesAsBase64.schema, x)
        case x: v.Draft06 => AsValue.schema(schema.Predef.bytesAsBase64.schema, x)
        case x: v.Draft07 => AsValue.schema(schema.Predef.bytesAsBase64.schema, x)
        case x: v.Draft09 => AsValue.schema(schema.Predef.bytesAsBase64.schema, x)
        case x: v.Draft12 => AsValue.schema(schema.Predef.bytesAsBase64.schema, x)
        case _            => fail()
      }
      expected.fields.foreach { case (k, x) => at(s, k) shouldEqual x }
    }
    val pattern = java.util.regex.Pattern.compile("^(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=)?$")
    List("", "Af5/", "AQ==", "AQI=", "AQID") foreach { x => assert(pattern.matcher(x).matches, x) }
    List("A", "AQ=", "A===", "Af5/\n", "Af-_") foreach { x => assert(!pattern.matcher(x).matches, x) }
  }

  test("Predef.bytesAsBase64 opted in through derivation: a local implicit wins over Predef.arrayS") {
    implicit val bytes: schema.Predef[Array[Byte]] = schema.Predef.bytesAsBase64
    val s                                          = AsValue.schema(Json.schema[Bytes], d12)
    at(s, "properties", "data", "type") shouldEqual str("string")
    at(s, "properties", "data", "contentEncoding") shouldEqual str("base64")
  }

  // ---- @deprecated

  test("@deprecated on a field: deprecated: true from 2019-09, nothing before") {
    for (s <- List(Json.schema[Old], Json.schema[OldJava])) {
      at(AsValue.schema(s, d12), "properties", "a", "deprecated") shouldEqual `true`
      at(AsValue.schema(s, d09), "properties", "a", "deprecated") shouldEqual `true`
      has(AsValue.schema(s, d12), "properties", "b", "deprecated") shouldBe false
      has(AsValue.schema(s, d07), "properties", "a", "deprecated") shouldBe false
      has(AsValue.schema(s, v.Draft06("x")), "properties", "a", "deprecated") shouldBe false
      has(AsValue.schema(s, v.Draft04()), "properties", "a", "deprecated") shouldBe false
    }
    Json.schema[Old].toString should include("deprecated")
  }

  test("@deprecated on an Option field: kept by Neutral, optionKeyRequired and Neutral.withOption(Legacy)'s unwrap (E8-R2)") {
    for (p <- List(Neutral, Neutral.withOptionKeyRequired(true), Neutral.withOption(OptionStyle.Legacy))) {
      implicit val profile: JsonSchemaProfile = p
      withClue(p.toString) { at(AsValue.schema(Json.schema[OldOpt], d12), "properties", "a", "deprecated") shouldEqual `true` }
    }
  }

  test("@deprecated on a $ref field: the $ref is marked, the shared definition isn't (E8-R3)") {
    import UserSchema._
    val s = AsValue.schema(Json.schema[Holder], d12)
    val u = at(s, "properties", "u").asInstanceOf[obj]
    u.keys shouldBe Set("$ref")
    at(s, "properties", "old") shouldEqual u + ("deprecated" -> `true`)
    val shared = at(s, "$defs").asInstanceOf[obj]
    shared.keys should have size 1
    shared.values.foreach(d => d.asInstanceOf[obj].keys should not contain "deprecated")
  }

  test("@deprecated on a type: the root is marked") {
    at(AsValue.schema(oldTypeSchema, d12), "deprecated") shouldEqual `true`
    has(AsValue.schema(oldTypeSchema, d07), "deprecated") shouldBe false
  }

  test("Legacy strips deprecated: fields, a leaf, an Option field, a $ref field, a type; the builders keep the knob (E8-R9)") {
    import UserSchema._
    Legacy.deprecatedKeyword shouldBe false
    Neutral.deprecatedKeyword shouldBe true
    val legacies = List(Legacy, Legacy.withName("l"), Legacy.withOption(OptionStyle.Nullable), Legacy.withSum(SumStyle.WrapperObject))
    for (p <- legacies) {
      implicit val profile: JsonSchemaProfile = p
      withClue(p.toString) {
        p.deprecatedKeyword shouldBe false
        has(AsValue.schema(Json.schema[Old], d12), "properties", "a", "deprecated") shouldBe false
        has(AsValue.schema(Json.schema[OldStr], d12), "properties", "s", "deprecated") shouldBe false
        has(AsValue.schema(Json.schema[OldOpt], d12), "properties", "a", "deprecated") shouldBe false
        has(AsValue.schema(Json.schema[Holder], d12), "properties", "old", "deprecated") shouldBe false
        has(AsValue.schema(oldTypeSchema, d12), "deprecated") shouldBe false
      }
    }
    Legacy.withDeprecatedKeyword(true).deprecatedKeyword shouldBe true
  }

  test("a profile that keeps the flag doesn't copy unflagged nodes (lowering stays equal)") {
    val s = Json.schema[ScalaColls]
    Neutral.lower(Neutral.lower(s)) shouldEqual Neutral.lower(s)
    Legacy.lower(s) shouldEqual Legacy.lower(Legacy.lower(s))
  }
}
