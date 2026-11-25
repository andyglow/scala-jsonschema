package com.github.andyglow.jsonschema.semiauto

import json._
import json.Schema._
import json.Schema.`object`.Field
import json.schema.Predef
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

object AutoPrecedenceScala3Spec {
  import AutoPrecedenceSpec.custom

  case class Given(a: Int)
  object Given { given Predef[Given] = Predef(custom[Given]) }

  case class GenericGiven[T](a: T)
  object GenericGiven { given [T]: Predef[GenericGiven[T]] = Predef(custom[GenericGiven[T]]) }

  case class Derives(a: Int) derives Schema

  case class Holder(g: Given, gg: GenericGiven[Int], d: Derives)
}

/** Scala 3: a companion `given` beats `Json.auto` too (also with 3.6+'s prioritization between givens: the bridge is an `implicit def`), and so does a `derives json.Schema` given. */
class AutoPrecedenceScala3Spec extends AnyFunSuite {
  import AutoPrecedenceScala3Spec._
  import AutoPrecedenceSpec.custom

  test("a companion given, a generic companion given, a derives given") {
    import Json.auto.given
    summon[Predef[Given]].schema shouldEqual custom[Given]
    summon[Predef[GenericGiven[Int]]].schema shouldEqual custom[GenericGiven[Int]]
    Json.schema[Holder] shouldEqual `object`[Holder](
      Field("g", custom[Given]),
      Field("gg", custom[GenericGiven[Int]]),
      Field("d", summon[Schema[Derives]])
    )
  }
}
