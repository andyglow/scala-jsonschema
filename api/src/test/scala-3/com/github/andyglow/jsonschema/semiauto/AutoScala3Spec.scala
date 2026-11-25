package com.github.andyglow.jsonschema.semiauto

import json._
import json.Schema._
import json.Schema.`object`.Field
import json.schema.Predef
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

object AutoScala3Spec {
  case class Foo(a: Int)
  val fooSchema: Schema[Foo] = `object`[Foo](Field("a", `integer`))

  case class JStrMap(m: Map[java.lang.String, Foo])

  // the generic instance the semi-auto error suggests: under auto, its `Predef[T]` is a hole that the derivation fills (a // comment: scaladoc would be the description)
  case class GBox[T](items: List[T])
  object GBox { implicit def gboxSchema[T](implicit p: Predef[T]): Schema[GBox[T]] = Json.schema[GBox[T]] }
  case class HasGBox(b: GBox[Foo])

  object Plain { val jStrMap: Schema[JStrMap] = Json.schema[JStrMap] }

  // mutual recursion through `derives` givens (issue #542, `lazy-ref`): no deadlock, no null
  case class MA(b: Option[MB]) derives Schema
  case class MB(a: Option[MA]) derives Schema
}

/** Scala 3-only checks of `Json.auto` (issue #542). */
class AutoScala3Spec extends AnyFunSuite {
  import AutoScala3Spec._

  test("core's collection constructors with an auto element are derived natively: a Map[java.lang.String, X] keeps its key pattern") {
    import Json.auto.given
    Json.schema[JStrMap] shouldEqual Plain.jStrMap
    Plain.jStrMap.toString should include("patternProperties")
  }

  test("a generic implicit Schema whose Predef[T] comes from auto: a definition, named after the concrete type (#297)") {
    import Json.auto.given
    val sig = Json.sig[GBox[Foo]].signature
    Json.schema[HasGBox] shouldEqual `object`[HasGBox](Field("b", `def`[GBox[Foo]](sig, `object`[GBox[Foo]](Field("items", `array`[Foo, List](fooSchema))))))
  }

  test("a type that only contains a recursive one, through a container with a Predef constructor: the element is the definition") {
    import AutoSpec.{BForest, BNode}
    val sig   = Json.sig[BNode].signature
    val bnode = `def`[BNode](sig, `object`[BNode](Field("name", `string`), Field("children", `array`[BNode, List](`ref`[BNode](sig)))))
    AutoSpec.Auto.bforest shouldEqual `object`[BForest](Field("trees", `array`[BNode, List](bnode)))
  }

  test("mutual recursion through derives givens: both definitions, each refers to the other") {
    val rendered = com.github.andyglow.jsonschema.AsValue.schema(summon[Schema[MA]], json.schema.Version.Draft07("x")).toString
    rendered should include("AutoScala3Spec.MA")
    rendered should include("AutoScala3Spec.MB")
    summon[Schema[MA]].toString shouldNot include("null")
    summon[Schema[MB]].toString shouldNot include("null")
  }

  test("Json.auto.derived called explicitly: an Exported Predef") {
    Json.auto.derived[Foo].instance.schema shouldEqual fooSchema
  }
}
