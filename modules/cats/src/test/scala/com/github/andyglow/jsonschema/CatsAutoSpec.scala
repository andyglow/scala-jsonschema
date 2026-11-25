package com.github.andyglow.jsonschema

import cats.data.{Chain, NonEmptyList, NonEmptyVector}
import json.Json
import json.Schema
import json.schema.Predef
import CatsSupport._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

object CatsAutoSpec {
  case class P(a: Int)
  case class Holder(nel: NonEmptyList[P], nev: NonEmptyVector[P], chain: Chain[P])
  case class Tree(value: Int, children: NonEmptyList[Tree])

  /** The expected output: cats' instances with an explicit `Predef[P]`. */
  object WithPredef {
    implicit val pPredef: Predef[P] = Predef(Json.schema[P])
    val holder: Schema[Holder]      = Json.schema[Holder]
  }

  object Auto {
    import Json.auto._
    val holder: Schema[Holder] = Json.schema[Holder]
    val tree: Schema[Tree]     = Json.schema[Tree]
  }
}

/** Issue #542 (Scala 2 and 3): with `Json.auto` imported, cats' `Predef` constructors get their element from auto (`nelSchemaFromPredef(importedPredef(auto.derived[P]))`), so `NonEmptyList[P]`,
  * `NonEmptyVector[P]` and `Chain[P]` are arrays (0.9: `NonEmptyList[P]` was derived as the case class `{head, tail}`).
  */
class CatsAutoSpec extends AnyFunSuite {
  import CatsAutoSpec._

  test("NonEmptyList[P], NonEmptyVector[P], Chain[P] with Json.auto: arrays of P") {
    Auto.holder shouldEqual WithPredef.holder
    Auto.holder.toString shouldNot include("head")
    Auto.holder match {
      case o: Schema.`object`[_] =>
        o.fields.toList.map(_.tpe).foreach {
          case Schema.`array`(_, _) => succeed
          case other                => fail(s"not an array: $other")
        }
      case other => fail(s"not an object: $other")
    }
  }

  test("recursion through NonEmptyList with Json.auto: a definition and a ref") {
    val sig = Json.sig[Tree].signature
    Auto.tree match {
      case Schema.`def`(s, o: Schema.`object`[_]) =>
        s shouldBe sig
        o.fields.find(_.name == "children").map(_.tpe) match {
          case Some(Schema.`array`(Schema.`ref`(r), _)) => r shouldBe sig
          case other                                    => fail(s"children: $other")
        }
      case other => fail(s"not a definition: $other")
    }
  }
}
