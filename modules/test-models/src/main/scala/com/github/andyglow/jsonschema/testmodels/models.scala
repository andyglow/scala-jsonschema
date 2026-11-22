package com.github.andyglow.jsonschema.testmodels

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
