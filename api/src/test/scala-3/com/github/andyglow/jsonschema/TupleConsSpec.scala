package com.github.andyglow.jsonschema

import json.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*
import scala.compiletime.testing.typeCheckErrors

object TupleConsSpec {
  case class HasCons(t: Int *: String *: EmptyTuple)
}

/** B10 (#467): Scala 3's `*:` spelling of a tuple. Repro for now: fails until the fix; becomes the real spec. */
class TupleConsSpec extends AnyFunSuite {
  import TupleConsSpec.*

  test("repro: a *: chain of known arity derives like the TupleN") {
    typeCheckErrors("Json.schema[Int *: String *: EmptyTuple]").map(_.message) shouldBe empty
    typeCheckErrors("Json.schema[HasCons]").map(_.message) shouldBe empty
    typeCheckErrors("Json.schema[List[Int *: String *: EmptyTuple]]").map(_.message) shouldBe empty
    typeCheckErrors("Json.schema[Tuple.Concat[Int *: EmptyTuple, String *: EmptyTuple]]").map(_.message) shouldBe empty
  }

  test("repro: Json.schema[Tuple] must not compile (today: enum [\"EmptyTuple\"])") {
    typeCheckErrors("Json.schema[Tuple]") should not be empty
  }

  test("probe: today's messages") {
    info("cons2: " + typeCheckErrors("Json.schema[Int *: String *: EmptyTuple]").map(_.message).mkString(" | "))
    info("empty: " + typeCheckErrors("Json.schema[EmptyTuple]").map(_.message).mkString(" | "))
    info("nonempty: " + typeCheckErrors("Json.schema[NonEmptyTuple]").map(_.message).mkString(" | "))
    info("openTail: " + typeCheckErrors("Json.schema[Int *: Tuple]").map(_.message).mkString(" | "))
    info("Tuple: " + Json.schema[Tuple].toString)
  }
}
