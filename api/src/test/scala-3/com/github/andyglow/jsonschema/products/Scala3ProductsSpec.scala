package com.github.andyglow.jsonschema.products

import com.github.andyglow.json.{ToValue, Value}
import com.github.andyglow.jsonschema.harness.CompileErrors.errorOf
import com.github.andyglow.jsonschema.harness.ErrorPhrases
import json.*
import json.Schema.*
import json.Schema.`object`.Field
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*

object Scala3ProductsSpec {
  // `None` defaults written in ways the Scala 2 re-parse couldn't handle: all give no default, without any ToValue
  case class NoneSpellings(
    a: Option[Int] = None /* a comment */,
    b: Option[Int] = scala.None,
    c: Option[Int] = // a comment
      Option.empty[Int],
    d: Option[Int] = Option.empty,
    e: Option[Int] = _root_.scala.None
  )

  object Defs {
    val none: Option[Int] = None
    val some: Option[Int] = Some(1)
  }
  case class SomeCall(a: Option[Int] = Some(1))
  case class FromVal(a: Option[Int] = Defs.some)
  case class NoneFromVal(a: Option[Int] = Defs.none)
  case class NestedSome(a: Option[Int] = Option(1))
  object Shadow { val None: Option[Int] = Some(1) }
  case class ShadowedNone(a: Option[Int] = Shadow.None)

  object OptIntToValue {
    given ToValue[Option[Int]] = ToValue.mk(_.fold[Value](Value.`null`)(Value.num(_)))
  }
}

/** Scala 3-only product behavior: how an `Option` default is told from `None` without `c.eval` (skeleton open point D2). */
class Scala3ProductsSpec extends AnyFunSuite {
  import Scala3ProductsSpec.*

  test("D2: `None` / `Option.empty` defaults in any spelling give no default, without a ToValue") {
    Json.schema[NoneSpellings] shouldEqual `object`[NoneSpellings](
      Field("a", `integer`, required = false),
      Field("b", `integer`, required = false),
      Field("c", `integer`, required = false),
      Field("d", `integer`, required = false),
      Field("e", `integer`, required = false)
    )
  }

  test("D2: any other Option default needs a ToValue[Option[X]] (Scala 2 parity)") {
    errorOf(assertCompiles("Json.schema[SomeCall]")) should include(ErrorPhrases.NoToValueForDefault)
    errorOf(assertCompiles("Json.schema[FromVal]")) should include(ErrorPhrases.NoToValueForDefault)
    errorOf(assertCompiles("Json.schema[NestedSome]")) should include(ErrorPhrases.NoToValueForDefault)
    errorOf(assertCompiles("Json.schema[ShadowedNone]")) should include(ErrorPhrases.NoToValueForDefault)
  }

  test("D2: a val that is None only at runtime can't be told at compile time, so it needs a ToValue too (Scala 2 evaluates it)") {
    errorOf(assertCompiles("Json.schema[NoneFromVal]")) should include(ErrorPhrases.NoToValueForDefault)
  }

  test("D2: with a ToValue[Option[X]], None is told apart at runtime") {
    import OptIntToValue.given
    Json.schema[SomeCall] shouldEqual `object`[SomeCall](Field.fromJson[Int]("a", `integer`, required = false, default = Some(Value.num(1))))
    Json.schema[FromVal] shouldEqual `object`[FromVal](Field.fromJson[Int]("a", `integer`, required = false, default = Some(Value.num(1))))
    Json.schema[NoneFromVal] shouldEqual `object`[NoneFromVal](Field("a", `integer`, required = false))
    Json.objectSchema[NoneSpellings]().fields.flatMap(_.default) shouldBe empty
  }
}
