package com.github.andyglow.jsonschema.characterization

import com.github.andyglow.jsonschema.harness.{ErrorPhrases, Golden}
import com.github.andyglow.jsonschema.harness.CompileErrors.errorOf
import json._
import json.schema._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

import scala.annotation.nowarn

object FieldsAnnotationsCharSpec {
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
}

// scalac 2 warns about named args out of order in an annotation; @nowarn has to sit on an enclosing definition
@nowarn("msg=named or default arguments")
object DiscOrder {
  @discriminator(phantom = false, field = "t") sealed trait Disc
  case class D1(t: String) extends Disc
}

/** Characterization of the sum-type annotations on Scala 2 (inventory §4). The product fields and field annotations are in FieldsCharSpec. */
class FieldsAnnotationsCharSpec extends AnyFunSuite {
  import FieldsAnnotationsCharSpec._
  import Golden.check

  import ErrorPhrases._

  test("A3: @definition is ignored on case objects") {
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
