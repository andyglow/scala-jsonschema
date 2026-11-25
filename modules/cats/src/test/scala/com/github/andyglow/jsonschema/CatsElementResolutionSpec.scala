package com.github.andyglow.jsonschema

import cats.data._
import json.Json
import json.Schema
import json.schema.{Flag, Predef}
import CatsSupport._
import org.scalatest.exceptions.TestFailedException
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

object CatsElementResolutionSpec {
  case class P(a: Int)

  case class All(
    nel: NonEmptyList[P],
    nev: NonEmptyVector[P],
    nes: NonEmptySet[P],
    nec: NonEmptyChain[P],
    neseq: NonEmptySeq[P],
    chain: Chain[P],
    nem: NonEmptyMap[String, P],
    oneAnd: OneAnd[List, P]
  )

  // one field each, for the error of rule 1
  case class WithNel(x: NonEmptyList[P])
  case class WithNev(x: NonEmptyVector[P])
  case class WithNes(x: NonEmptySet[P])
  case class WithNec(x: NonEmptyChain[P])
  case class WithNeSeq(x: NonEmptySeq[P])
  case class WithChain(x: Chain[P])
  case class WithNem(x: NonEmptyMap[String, P])
  case class WithOneAnd(x: OneAnd[List, P])

  /** Rule 3: an instance for the element. */
  object WithPredef {
    implicit val pPredef: Predef[P] = Predef(Json.schema[P])
    val all: Schema[All]            = Json.schema[All]
  }

  /** Rule 2: auto. */
  object Auto {
    import Json.auto._
    val all: Schema[All] = Json.schema[All]
  }
}

/** The three rules for cats collections of an element (issue #482, on top of #542), the same on Scala 2 and 3:
  *   1. no instance for the element and no auto: a compile error (with `Flag.SemiAuto`; in 0.10 without it, a warning), never the structural `{head, tail}` silently;
  *   2. `import Json.auto._`: the element is derived;
  *   3. an implicit `Predef` / `Schema` for the element: used.
  */
class CatsElementResolutionSpec extends AnyFunSuite {
  import CatsElementResolutionSpec._

  private def fields(s: Schema[_]): List[(String, Schema[_])] = s match {
    case o: Schema.`object`[_] => o.fields.toList.map(f => f.name -> f.tpe)
    case other                 => fail(s"not an object: $other")
  }

  private def isCollection(s: Schema[_]): Boolean = s match {
    case Schema.`array`(_, _) | Schema.`dictionary`(_) => true
    case _                                             => false
  }

  test("rule 3: an implicit Predef for the element: arrays (a dictionary for NonEmptyMap)") {
    fields(WithPredef.all) foreach { case (n, t) => withClue(n) { isCollection(t) shouldBe true } }
  }

  test("rule 2: Json.auto: the same as with an instance, never the case class internals") {
    Auto.all shouldEqual WithPredef.all
    Auto.all.toString shouldNot include("head")
  }

  test("rule 1: no instance, no auto, Flag.SemiAuto: a compile error, never the structural schema") {
    implicit val jsonSchemaFlags: Flag with Flag.SemiAuto                 = null
    def errorOf(f: => Any): String                                        = intercept[TestFailedException](f).getMessage
    def check(msg: String, owner: String, catsType: Option[String]): Unit = withClue(owner) {
      msg should include("is not supported")
      msg should include(owner)
      catsType foreach { t => msg should include(t) } // the newtypes (NonEmptySet, NonEmptyChain, NonEmptyMap) print as `cats.data.Newtype...Type` on Scala 2
    }
    check(errorOf(assertCompiles("Json.schema[WithNel]")), "WithNel", Some("NonEmptyList"))
    check(errorOf(assertCompiles("Json.schema[WithNev]")), "WithNev", None)
    check(errorOf(assertCompiles("Json.schema[WithNes]")), "WithNes", None)
    check(errorOf(assertCompiles("Json.schema[WithNec]")), "WithNec", None)
    check(errorOf(assertCompiles("Json.schema[WithNeSeq]")), "WithNeSeq", None)
    check(errorOf(assertCompiles("Json.schema[WithChain]")), "WithChain", Some("Chain"))
    check(errorOf(assertCompiles("Json.schema[WithNem]")), "WithNem", None)
    check(errorOf(assertCompiles("Json.schema[WithOneAnd]")), "WithOneAnd", Some("OneAnd"))
    jsonSchemaFlags shouldBe null
  }

}
