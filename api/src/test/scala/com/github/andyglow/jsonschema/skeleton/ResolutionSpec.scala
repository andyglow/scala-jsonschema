package com.github.andyglow.jsonschema.skeleton

import com.github.andyglow.jsonschema.harness.Golden
import json._
import json.Schema._
import json.Schema.`object`.Field
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

object ResolutionSpec {
  case class Id(value: String) extends AnyVal
  case class HasId(id: Id, n: Int)

  case class Inner(a: Int)
  case class Outer(inner: Inner, opt: Option[Inner])
  object InnerSchema { implicit val innerSchema: Schema[Inner] = Json.schema[Inner]                                                         }
  object InnerPredef { implicit val innerPredef: json.schema.Predef[Inner] = json.schema.Predef(`object`[Inner](Field("custom", `string`))) }

  case class HasString(s: String, n: Int)
  object StringSchema { implicit val stringSchema: Schema[String] = `string`[String] }

  case class Linked(value: Int, next: Option[Linked])
  case class MutualA(b: Option[MutualB])
  case class MutualB(a: Option[MutualA], n: Int)

  case class Defaults(a: String = "x", b: Option[Int] = None, d: Double = 1.5)

  case class Described(a: Int, b: String)

  object SelfInObject {
    implicit val innerSelf: Schema[Inner] = Json.schema[Inner]
  }

  case class Curried(a: Int)(val b: String)

  /** Documented in scaladoc */
  @json.schema.title("Titled")
  case class WithTexts(a: Int)
  object WithTextsSchema { implicit val withTextsSchema: Schema[WithTexts] = `object`[WithTexts](Field("a", `integer`)) }

  case class Gen[A, B](a: A, b: B)
}

/** Resolution rules the Scala 3 skeleton implements, pinned on every Scala version (the goldens are Scala 2's output). */
class ResolutionSpec extends AnyFunSuite {
  import ResolutionSpec._
  import Golden.check

  test("value classes: root and field") {
    Json.schema[Id] shouldEqual `value-class`[Id, String](`string`)
    check("skeleton/HasId", Json.schema[HasId])
  }

  test("an implicit Schema becomes a definition named after the type, an implicit Predef is inlined") {
    locally {
      import InnerSchema._
      check("skeleton/Outer.implicitSchema", Json.schema[Outer])
      // the root itself (R5)
      Json.schema[Inner] shouldEqual `def`[Inner]("com.github.andyglow.jsonschema.skeleton.ResolutionSpec.Inner", `object`(Field("a", `integer`)))
    }
    locally {
      import InnerPredef._
      check("skeleton/Outer.implicitPredef", Json.schema[Outer])
    }
  }

  test("types are dealiased: an implicit Schema[String] on a field is the definition java.lang.String") {
    import StringSchema._
    check("skeleton/HasString.implicitSchema", Json.schema[HasString])
  }

  test("a typed implicit val defined with Json.schema doesn't pick itself up") {
    SelfInObject.innerSelf shouldEqual `object`[Inner](Field("a", `integer`))
  }

  test("recursion through Option fields") {
    check("skeleton/Linked", Json.schema[Linked])
    check("skeleton/MutualA", Json.schema[MutualA])
  }

  test("defaults are runtime values; = None emits no default") {
    check("skeleton/Defaults", Json.schema[Defaults])
  }

  test("objectSchema: decorations by field name") {
    Json.objectSchema[Described]("a" -> "A", "b" -> "B") shouldEqual `object`[Described](
      Field("a", `integer`).withDescription(Some("A")),
      Field("b", `string`).withDescription(Some("B"))
    )
    Json.objectSchema[Described](("a", "A")) shouldEqual `object`[Described](Field("a", `integer`).withDescription(Some("A")), Field("b", `string`))
  }

  test("the root's own title and scaladoc apply on top of a root implicit Schema too") {
    import WithTextsSchema._
    Json.schema[WithTexts] shouldEqual `def`[WithTexts]("com.github.andyglow.jsonschema.skeleton.ResolutionSpec.WithTexts", withTextsSchema)
      .withTitle("Titled")
      .withDescription("Documented in scaladoc")
  }

  test("curried case class: first parameter list only") {
    Json.schema[Curried] shouldEqual `object`[Curried](Field("a", `integer`))
  }

  test("signatures") {
    Json.sig[List[Option[Int]]].signature shouldEqual "scala.List[scala.Option[scala.Int]]"
    Json.sig[Map[String, Int]].signature shouldEqual "scala.Predef.Map[scala.Predef.String,scala.Int]"
    Json.sig[Set[BigDecimal]].signature shouldEqual "scala.Predef.Set[scala.BigDecimal]"
    Json.sig[Either[Int, String]].signature shouldEqual "scala.Either[scala.Int,scala.Predef.String]"
    Json.sig[(Int, String)].signature shouldEqual "scala.Tuple2[scala.Int,scala.Predef.String]"
    Json.sig[Gen[Id, Gen[Int, Long]]].signature shouldEqual
      "com.github.andyglow.jsonschema.skeleton.ResolutionSpec.Gen[com.github.andyglow.jsonschema.skeleton.ResolutionSpec.Id,com.github.andyglow.jsonschema.skeleton.ResolutionSpec.Gen[scala.Int,scala.Long]]"
    Json.sig[java.util.UUID].signature shouldEqual "java.util.UUID"
    Json.sig[None.type].signature shouldEqual "scala.None"
  }
}
