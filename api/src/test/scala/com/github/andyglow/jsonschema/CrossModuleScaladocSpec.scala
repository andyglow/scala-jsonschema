package com.github.andyglow.jsonschema

import com.github.andyglow.jsonschema.testmodels._
import json._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

/** Scaladoc of models from another module (`test-models`, with sbt-scaladoc). Scala 2: embedded in the bytecode by the compiler plugin. Scala 3: from TASTy, with -Yread-docs.
  */
class CrossModuleScaladocSpec extends AnyFunSuite {

  private def fieldDescriptions(s: Schema.`object`[_]) = s.fields.toList.flatMap(f => f.description.map(f.name -> _))

  test("a documented class from another module: type and field descriptions") {
    val s = Json.objectSchema[FromOtherModule]()
    s.description shouldBe Some("A model from another module")
    fieldDescriptions(s) should contain.only("id" -> "the identifier", "n" -> "a number")
    Json.schema[FromOtherModule].description shouldBe Some("A model from another module")
  }

  test("an undocumented class from another module gets no description") {
    val s = Json.objectSchema[UndocumentedInOtherModule]()
    s.description shouldBe None
    fieldDescriptions(s) shouldBe empty
  }

  test("non-public vals of a class from another module are fields (B4)") {
    val s = Json.objectSchema[NonPublicValsInOtherModule]()
    s.fields.toList.map(_.name) shouldBe List("a", "b", "c")
    fieldDescriptions(s) should contain.only("a" -> "a private val", "b" -> "a protected val")
  }

  test("field-targeted annotations of public and non-public vals of a class from another module (B4)") {
    val s = Json.objectSchema[FieldAnnotatedInOtherModule]()
    fieldDescriptions(s) should contain.only("a" -> "public, on the field", "b" -> "private, on the field")
    s.fields.toList.map(f => f.name -> f.rwMode) should contain.only(
      "a" -> Schema.`object`.Field.RWMode.ReadWrite,
      "b" -> Schema.`object`.Field.RWMode.ReadWrite,
      "c" -> Schema.`object`.Field.RWMode.ReadOnly,
      "d" -> Schema.`object`.Field.RWMode.WriteOnly
    )
  }

  test("a class nested in an object of another module") {
    val s = Json.objectSchema[Container.InObject]()
    s.description shouldBe Some("Nested in an object")
    fieldDescriptions(s) should contain.only("a" -> "a field of InObject")
  }

  test("a class nested in a trait of another module (embedded from scaladoc 0.2.0 on Scala 2)") {
    val s = Json.objectSchema[Models.InTrait]()
    s.description shouldBe Some("Nested in a trait")
    fieldDescriptions(s) should contain.only("a" -> "a field of InTrait")
  }
}
