package com.github.andyglow.jsonschema.characterization

import com.github.andyglow.jsonschema.harness.Golden
import json._
import json.schema._
import org.scalatest.funsuite.AnyFunSuite

import scala.annotation.meta.field

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

  // non-public constructor vals (B4): Scala 2 adds a `<name>$access$<i>` case accessor for each
  case class PrivateVal(private val a: Int, b: String)
  case class PrivatePkgVal(private[characterization] val a: Int, b: String)
  case class ProtectedVal(protected val a: Int, b: String)
  case class PrivateValAnnotated(@(description @field)("from the field") private val a: Int, @description("from the param") private val b: Int, @(readOnly @field) protected val c: Int)
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

  test("B4: non-public constructor vals (private, private[pkg], protected) are fields") {
    check("char/products/PrivateVal", Json.schema[PrivateVal])
    check("char/products/PrivatePkgVal", Json.schema[PrivatePkgVal])
    check("char/products/ProtectedVal", Json.schema[ProtectedVal])
  }

  test("B4: annotations on non-public constructor vals") {
    check("char/products/PrivateValAnnotated", Json.schema[PrivateValAnnotated])
  }

  test("F7/F11: field annotations of a generic case class") {
    check("char/products/Generic[Int]", Json.schema[Generic[Int]])
  }
}
