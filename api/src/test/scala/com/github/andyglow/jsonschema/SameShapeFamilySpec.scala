package com.github.andyglow.jsonschema

import com.github.andyglow.json.{JsonFormatter, Value}
import json._
import json.schema.{JsonSchemaProfile, Version}
import json.schema.JsonSchemaProfile.SumStyle
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._
import petshop.Acceptance

/** B3: sealed families whose members have the same (or an overlapping) JSON shape, and the order of a family's members in the rendered schema. */
object SameShapeFamilySpec {

  // the same shape, the same texts: equal member schemas
  sealed trait Ev
  case class Created(id: String) extends Ev
  case class Deleted(id: String) extends Ev

  // the same shape, different texts
  sealed trait DocEv
  @json.schema.description("created") case class DCreated(id: String) extends DocEv
  @json.schema.description("deleted") case class DDeleted(id: String) extends DocEv

  // the same shape, each member a definition (an implicit Schema): two `$ref`s to equal bodies
  sealed trait RefEv
  case class RCreated(id: String) extends RefEv
  object RCreated { implicit val s: Schema[RCreated] = Json.schema[RCreated] }
  case class RDeleted(id: String) extends RefEv
  object RDeleted { implicit val s: Schema[RDeleted] = Json.schema[RDeleted] }

  // overlapping shapes: `{"id": "x"}` is either
  sealed trait OvEv
  case class OCreated(id: String)                       extends OvEv
  case class OUpdated(id: String, name: Option[String]) extends OvEv

  // five members, no two alike
  sealed trait Five
  case class E1(a: Int) extends Five
  case class D2(b: Int) extends Five
  case class C3(c: Int) extends Five
  case class B4(d: Int) extends Five
  case class A5(e: Int) extends Five
}

class SameShapeFamilySpec extends AnyFunSuite {
  import SameShapeFamilySpec._

  val d07: Version.Draft07 = Version.Draft07("https://b3.test/schema.json")

  def render(s: Schema[_])(implicit p: JsonSchemaProfile): Value.obj = AsValue.schema(s, d07)
  def text(s: Schema[_])(implicit p: JsonSchemaProfile): String      = JsonFormatter.format(render(s))

  /** The keys each rendered `oneOf`/`anyOf` member requires, in order. */
  def memberKeys(v: Value.obj): List[List[String]] = {
    val members = v.value.collectFirst { case ("oneOf" | "anyOf", Value.arr(xs)) => xs.toList }.getOrElse(fail(s"no oneOf/anyOf in $v"))
    members map {
      case m: Value.obj => m.value.collectFirst { case ("required", Value.arr(ks)) => ks.toList.collect { case Value.str(k) => k } }.getOrElse(Nil)
      case other        => fail(s"not an object: $other")
    }
  }

  val untagged: List[JsonSchemaProfile] = List(JsonSchemaProfile.Neutral, JsonSchemaProfile.Neutral.withHonorsSchemaAnnotations(false).withName("untagged-library"))
  val tagged: List[JsonSchemaProfile]   = List(JsonSchemaProfile.Neutral.withSum(SumStyle.WrapperObject), JsonSchemaProfile.Neutral.withSum(SumStyle.Discriminator("type")))

  test("a family's members are rendered in declaration order, also with five or more members") {
    (untagged ++ tagged) foreach { implicit p =>
      val keys = memberKeys(render(Json.schema[Five])).map(_.filterNot(_ == "type"))
      withClue(p.name) {
        p.sumStyle match {
          case SumStyle.WrapperObject => keys.map(_.map(_.split('.').last)) shouldBe List(List("E1"), List("D2"), List("C3"), List("B4"), List("A5"))
          case _                      => keys shouldBe List(List("a"), List("b"), List("c"), List("d"), List("e"))
        }
      }
    }
  }

  test("untagged: a document of a member with the same shape as another one is valid") {
    untagged foreach { implicit p =>
      Acceptance.assertValid(Acceptance.rendered(Json.schema[Ev]), List("""{"id":"x"}"""))
      Acceptance.assertValid(Acceptance.rendered(Json.schema[DocEv]), List("""{"id":"x"}"""))
      Acceptance.assertValid(Acceptance.rendered(Json.schema[RefEv]), List("""{"id":"x"}"""))
    }
  }

  test("untagged: a document that matches two overlapping members is valid") {
    untagged foreach { implicit p =>
      Acceptance.assertValid(Acceptance.rendered(Json.schema[OvEv]), List("""{"id":"x"}""", """{"id":"x","name":"n"}""", """{"id":"x","name":null}"""))
      Acceptance.assertInvalid(Acceptance.rendered(Json.schema[OvEv]), List("""{}""", """{"id":"x","other":1}"""))
    }
  }

  test("untagged: same-shape members with different texts or definitions are all kept") {
    untagged foreach { implicit p =>
      withClue(p.name) {
        memberKeys(render(Json.schema[DocEv])) should have size 2
        val refs = text(Json.schema[RefEv])
        refs should include("RCreated")
        refs should include("RDeleted")
      }
    }
  }

  test("tagged: every member of a same-shape family is kept, and each one's documents are valid") {
    tagged foreach { implicit p =>
      withClue(p.name) { memberKeys(render(Json.schema[Ev])) should have size 2 }
    }
    val created = "com.github.andyglow.jsonschema.SameShapeFamilySpec.Created"
    val deleted = "com.github.andyglow.jsonschema.SameShapeFamilySpec.Deleted"
    Acceptance.assertValid(Acceptance.rendered(Json.schema[Ev])(tagged(0)), List(s"""{"$created":{"id":"x"}}""", s"""{"$deleted":{"id":"x"}}"""))
    Acceptance.assertValid(Acceptance.rendered(Json.schema[Ev])(tagged(1)), List(s"""{"id":"x","type":"$created"}""", s"""{"id":"x","type":"$deleted"}"""))
  }
}
