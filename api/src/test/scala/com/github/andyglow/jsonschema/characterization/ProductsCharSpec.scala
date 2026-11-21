package com.github.andyglow.jsonschema.characterization

import com.github.andyglow.jsonschema.harness.Golden
import json._
import json.schema._
import org.scalatest.funsuite.AnyFunSuite

import scala.annotation.meta.field

// NOTE: undocumented models come first. On Scala 2 (scaladoc-parser 0.1.0) a class WITHOUT a doc comment picks up the nearest preceding comment in the file (BACKLOG #5).
object ProductsCharSpec {
  // annotations aimed at the underlying field (`@(x @field)`) instead of the constructor parameter
  case class FieldTargets(@(description @field)("from the field") a: Int, @(readOnly @field) b: Int, @(writeOnly @field) @description("from the param") c: Int)

  case class HasId(id: Id)

  case class Generic[T](@description("a T") t: T, @readOnly ts: Option[T])

  /** A wrapper
    *
    * @param v
    *   the value
    */
  @title("Id")
  case class Id(v: String) extends AnyVal
}

/** Characterization of product details on Scala 2 (stream 1): field-targeted annotations, value-class texts, annotations on generic fields. */
class ProductsCharSpec extends AnyFunSuite {
  import ProductsCharSpec._
  import Golden.check

  test("F10: annotations on the underlying field (@(x @field))") {
    check("char/products/FieldTargets", Json.schema[FieldTargets])
  }

  test("T9: a value class as the root gets its type texts; as a field it doesn't (R8)") {
    check("char/products/Id", Json.schema[Id])
    check("char/products/HasId", Json.schema[HasId])
  }

  test("F7/F11: field annotations of a generic case class") {
    check("char/products/Generic[Int]", Json.schema[Generic[Int]])
  }
}
