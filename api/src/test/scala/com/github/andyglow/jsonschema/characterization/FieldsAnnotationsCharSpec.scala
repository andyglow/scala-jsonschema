package com.github.andyglow.jsonschema.characterization

import com.github.andyglow.json.{ToValue, Value}
import com.github.andyglow.jsonschema.harness.{ErrorPhrases, Golden, Scala, TriageA}
import com.github.andyglow.jsonschema.harness.CompileErrors.errorOf
import json._
import json.schema._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

import scala.annotation.nowarn

object FieldsAnnotationsCharSpec {
  case class Custom(i: Int)
  case class NoToValue(c: Custom = Custom(1))
  case class NoneDefault(a: Option[String] = None)
  case class OptionEmptyDefault(a: Option[String] = Option.empty)
  case class ReadWrite(@readOnly r: Int, @writeOnly w: Int, @readOnly @writeOnly both: Int, plain: Int)

  @definition("root-def") case class DefinitionOnRoot(a: Int)
  @definition("field-def") case class DefinitionOnFieldType(a: Int)
  case class HasDefinitionOnFieldType(d: DefinitionOnFieldType)
  sealed trait DefinitionOnCaseObject
  object DefinitionOnCaseObject {
    @definition("o") case object O1        extends DefinitionOnCaseObject
    @definition("c") case class C1(a: Int) extends DefinitionOnCaseObject
  }

  @discriminator sealed trait DiscriminatedEnum
  object DiscriminatedEnum {
    case object X extends DiscriminatedEnum
    case object Y extends DiscriminatedEnum
  }
  @discriminator sealed trait DiscriminatedWithValueClass extends Any
  object DiscriminatedWithValueClass {
    case class V1(v: Int)    extends AnyVal with DiscriminatedWithValueClass
    case class V2(w: String) extends DiscriminatedWithValueClass
  }
  sealed trait KeyWithoutDiscriminator
  object KeyWithoutDiscriminator {
    @discriminatorKey("k1") case class K1(v: Int) extends KeyWithoutDiscriminator
    case class K2(w: String)                      extends KeyWithoutDiscriminator
  }
  // TRIAGE-A: these derive on Scala 3 (expected values in scala-3/Scala3OnlyCharSpec), but crash Scala 2's re-parse
  case class SomeDefault(b: Option[String] = Some("x"))
  case class OptionEmptyTypedDefault(a: Option[String] = Option.empty[String])
  // core has no ToValue[Option[_]] (the bridges bring one), so a Some(..) default needs it explicitly
  object OptionToValue {
    implicit val optStringTV: ToValue[Option[String]] = ToValue.mk(_.fold[Value](Value.`null`)(Value.str(_)))
  }
}

// scalac 2 warns about named args out of order in an annotation; @nowarn has to sit on an enclosing definition
@nowarn("msg=named or default arguments")
object DiscOrder {
  @discriminator(phantom = false, field = "t") sealed trait Disc
  case class D1(t: String) extends Disc
}

/** Characterization of fields, defaults and annotations on Scala 2 (inventory §4). */
class FieldsAnnotationsCharSpec extends AnyFunSuite {
  import FieldsAnnotationsCharSpec._
  import Golden.check

  import ErrorPhrases._

  test("F3: a default without a ToValue doesn't compile") {
    errorOf(assertCompiles("Json.schema[NoToValue]")) should include(NoToValueForDefault)
  }

  test("F4: `= None` and `= Option.empty` emit no default") {
    check("char/fields/NoneDefault", Json.schema[NoneDefault])
    check("char/fields/OptionEmptyDefault", Json.schema[OptionEmptyDefault])
  }

  test("F4: `= Some(x)` (same compilation run) works on Scala 3, crashes the Scala 2 re-parse", TriageA) {
    // Scala 3 value: scala-3/Scala3OnlyCharSpec
    if (Scala.is3) assertCompiles("{ import OptionToValue._; Json.schema[SomeDefault] }")
    else errorOf(assertCompiles("{ import OptionToValue._; Json.schema[SomeDefault] }")) should include("TypecheckException")
  }

  test("F4: `= Option.empty[String]` works on Scala 3, crashes the Scala 2 re-parse", TriageA) {
    // Scala 3 value: scala-3/Scala3OnlyCharSpec
    if (Scala.is3) assertCompiles("Json.schema[OptionEmptyTypedDefault]")
    else errorOf(assertCompiles("Json.schema[OptionEmptyTypedDefault]")) should include("TypecheckException")
  }

  test("F11/A7: @readOnly, @writeOnly, both (= neither)") {
    check("char/fields/ReadWrite", Json.schema[ReadWrite])
  }

  test("A3: @definition is ignored on the root, on field types and on case objects") {
    check("char/fields/DefinitionOnRoot", Json.schema[DefinitionOnRoot])
    check("char/fields/HasDefinitionOnFieldType", Json.schema[HasDefinitionOnFieldType])
    check("char/fields/DefinitionOnCaseObject", Json.schema[DefinitionOnCaseObject])
  }

  test("A4: @discriminator on an all-case-object family is ignored") {
    check("char/fields/DiscriminatedEnum", Json.schema[DiscriminatedEnum])
  }

  test("A4: @discriminator with a value-class member doesn't compile") {
    errorOf(assertCompiles("Json.schema[DiscriminatedWithValueClass]")) should include(DiscriminatorOnly)
  }

  test("A4: @discriminator named args out of order: rejected on 2.12, work on 2.13 (golden in scala-2.13+)") {
    if (Golden.scalaBinaryVersion == "2.12") errorOf(assertCompiles("Json.schema[DiscOrder.Disc]")) should include(DiscriminatorOrder)
    else assertCompiles("Json.schema[DiscOrder.Disc]")
  }

  test("A5: @discriminatorKey is applied even without @discriminator") {
    check("char/fields/KeyWithoutDiscriminator", Json.schema[KeyWithoutDiscriminator])
  }
}
