package com.github.andyglow.jsonschema.sums

import com.github.andyglow.json.Value
import com.github.andyglow.json.Value.*
import com.github.andyglow.jsonschema.AsValue
import json.*
import json.schema.{JsonSchemaProfile, Version}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*

// B7: Scala 3 `enum` values keep declaration order (more than 4, so a plain Set would iterate in hash order)
object Scala3EnumOrderSpec {
  enum Weekday { case Mon, Tue, Wed, Thu, Fri, Sat, Sun }

  enum Cmd {
    case Run(arg: String)
    case Start, Stop, Pause, Resume, Reset, Kill
  }
}

class Scala3EnumOrderSpec extends AnyFunSuite {
  import Scala3EnumOrderSpec.*

  private def enums(v: Value): List[List[Value]] = v match {
    case o: obj =>
      o.fields.toList.flatMap {
        case ("enum", arr(vs)) => List(vs.toList)
        case (_, x)            => enums(x)
      }
    case arr(xs) => xs.toList.flatMap(enums)
    case _       => Nil
  }

  private def enumOf(s: Schema[?])(using JsonSchemaProfile): List[Value] = enums(AsValue.schema(s, Version.Draft07(id = "x"))) match {
    case List(vs) => vs
    case other    => fail(s"expected one enum, got $other")
  }

  test("Scala 3 enum of singletons: declaration order") {
    enumOf(Json.schema[Weekday]) shouldBe List("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun").map(str(_))
  }

  test("Scala 3 enum of singletons, Legacy profile: declaration order") {
    enumOf(Json.schema[Weekday])(using JsonSchemaProfile.Legacy) shouldBe List("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun").map(str(_))
  }

  test("Scala 3 enum with a parameterized case: the singletons' enum in declaration order") {
    enumOf(Json.schema[Cmd]) shouldBe List("Start", "Stop", "Pause", "Resume", "Reset", "Kill").map(str(_))
  }
}
