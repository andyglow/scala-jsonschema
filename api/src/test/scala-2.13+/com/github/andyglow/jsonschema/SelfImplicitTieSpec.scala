package com.github.andyglow.jsonschema

import json._
import json.Schema._
import json.Schema.`object`.Field
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

// #320 (B9-R7): a more specific Schema.object in a base trait against the Schema being defined in the object: scalac ranks them a tie.
// The plain search reports it ("ambiguous implicit values"): a compile error on 2.12 and on 2.13 compilers before 2.13.17, so this spec
// isn't shared with 2.12. On 2.13.17+ the search leaves the definition out, and the tie counts as a declined self reference: derived, as on Scala 3.
class SelfImplicitTieSpec extends AnyFunSuite {
  import SelfImplicitTieSpec._

  test("#320: a more specific Schema.object in a base trait ties with the one in the object: derived") {
    Mine.mine shouldEqual `object`.fromList[Any](List(Field("a", `integer`)))
  }
}

object SelfImplicitTieSpec {
  case class X(a: Int)
  trait Low { implicit def low: Schema.`object`[X] = `object`[X](Field("custom", `string`)) }
  object Mine extends Low { implicit val mine: Schema[X] = Json.schema[X] }
}
