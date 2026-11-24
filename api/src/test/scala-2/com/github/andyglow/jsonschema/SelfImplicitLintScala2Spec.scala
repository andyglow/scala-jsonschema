package com.github.andyglow.jsonschema

import json._
import json.Schema._
import json.Schema.`object`.Field
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

import scala.annotation.nowarn

// Scala 2 only (Scala 3 requires the result type of an implicit definition): an implicit val without a result type is still being typed
// while its right-hand side expands, so it's no candidate of the search, and an implicit of its type elsewhere is used (#320, B9-R7).
class SelfImplicitLintScala2Spec extends AnyFunSuite {
  import SelfImplicitLintScala2Spec._

  test("#320: an un-annotated implicit val next to a companion Schema: a definition of the companion's") {
    Mine.mine shouldEqual `def`[U1](Json.sig[U1].signature, U1.companion)
  }
}

object SelfImplicitLintScala2Spec {
  case class U1(a: Int)
  object U1 { implicit val companion: Schema[U1] = `object`[U1](Field("custom", `string`)) }

  object Mine {
    // the point of the test: no result type (2.13 warns about that)
    @nowarn("msg=Implicit definition should have explicit type")
    implicit val mine = Json.schema[U1]
  }
}
