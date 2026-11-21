package com.github.andyglow.jsonschema

import json.Json
import json.Schema
import json.Schema._
import json.Schema.`object`.Field
import json.schema.Predef
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

// An implicit defined with Json.schema of its own type must not pick itself up. The search finds the implicit being defined, which is still null
// when the derived code runs: before, Scala 2 checked this for `Schema` only (and only when the definition spans at most two lines), so an
// `implicit val p: Predef[X] = Predef(Json.schema[X])` failed at initialization with a NullPointerException / ExceptionInInitializerError.
class SelfReferenceSpec extends AnyFunSuite {
  import SelfReferenceSpec._

  private def obj(fields: Field[_]*) = `object`.fromList[Any](fields.toList)

  test("an implicit val Predef defined with Json.schema doesn't pick itself up") {
    ViaVal.p.schema shouldEqual obj(Field("a", `integer`))
  }

  test("an implicit def Predef defined with Json.schema doesn't pick itself up") {
    ViaDef.p.schema shouldEqual obj(Field("b", `string`))
  }

  test("an implicit lazy val Predef defined with Json.schema doesn't pick itself up") {
    ViaLazy.p.schema shouldEqual obj(Field("c", `boolean`))
  }

  test("an implicit Predef spanning several lines doesn't pick itself up") {
    ViaMultiline.p.schema shouldEqual obj(Field("d", `integer`))
  }

  test("an implicit Predef in a class doesn't pick itself up") {
    new InClass().p.schema shouldEqual obj(Field("e", `string`))
  }

  test("the Predef is used (inlined) once defined") {
    import ViaVal.p
    Json.schema[UsesP1] shouldEqual obj(Field("p", obj(Field("a", `integer`))))
  }

  test("an implicit Schema spanning several lines doesn't pick itself up") {
    // before (Scala 2): the source-text check only looks at the current and the previous line, so this one wasn't detected
    ViaMultilineSchema.s shouldEqual obj(Field("f", `integer`))
  }
}

object SelfReferenceSpec {
  case class P1(a: Int)
  case class P2(b: String)
  case class P3(c: Boolean)
  case class P4(d: Int)
  case class P5(e: String)
  case class S1(f: Int)
  case class UsesP1(p: P1)

  object ViaVal {
    implicit val p: Predef[P1] = Predef(Json.schema[P1])
  }

  object ViaDef {
    implicit def p: Predef[P2] = Predef(Json.schema[P2])
  }

  object ViaLazy {
    implicit lazy val p: Predef[P3] = Predef(Json.schema[P3])
  }

  object ViaMultiline {
    implicit val p: Predef[P4] =
      Predef(
        Json.schema[P4]
      )
  }

  class InClass {
    implicit val p: Predef[P5] = Predef(Json.schema[P5])
  }

  object ViaMultilineSchema {
    implicit val s: Schema[S1] =
      json.Json
        .schema[S1]
  }
}
