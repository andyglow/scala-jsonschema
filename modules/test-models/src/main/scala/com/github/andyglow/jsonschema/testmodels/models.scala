package com.github.andyglow.jsonschema.testmodels

import json.schema.{description, readOnly, writeOnly}

import scala.annotation.meta.field

// Compiled apart from api's tests (see `test-models` in build.sbt): the Json.schema calls in CrossModuleScaladocSpec
// can only find these doc comments in the bytecode (Scala 2, embedded by the scaladoc compiler plugin) or in the
// TASTy (Scala 3, with -Yread-docs).

/** A model from another module
  *
  * @param id
  *   the identifier
  * @param n
  *   a number
  */
case class FromOtherModule(id: String, n: Int)

case class UndocumentedInOtherModule(a: Int)

/** Non-public vals from another module
  *
  * @param a
  *   a private val
  * @param b
  *   a protected val
  */
case class NonPublicValsInOtherModule(private val a: Int, protected val b: String, c: Int)

// annotations aimed at the underlying field (`@(x @field)`): read from the unpickled field on Scala 2, from TASTy on Scala 3
case class FieldAnnotatedInOtherModule(
  @(description @field)("public, on the field") a: Int,
  @(description @field)("private, on the field") private val b: Int,
  @(readOnly @field) protected val c: Int,
  @(writeOnly @field) d: Int
)

object Container {

  /** Nested in an object
    *
    * @param a
    *   a field of InObject
    */
  case class InObject(a: Int)
}

trait Models {

  /** Nested in a trait
    *
    * @param a
    *   a field of InTrait
    */
  case class InTrait(a: Int)
}

object Models extends Models

// EnumOrderSpec (#458): a case-object family loaded from class files. Scala 2 has no source positions for its members,
// so it lists them by name; Scala 3 reads the declaration order from TASTy.
sealed trait LibraryColor
object LibraryColor {
  case object Red    extends LibraryColor
  case object Green  extends LibraryColor
  case object Blue   extends LibraryColor
  case object Cyan   extends LibraryColor
  case object Yellow extends LibraryColor
  case object Black  extends LibraryColor
}
