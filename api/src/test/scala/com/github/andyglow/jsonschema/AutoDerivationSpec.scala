package com.github.andyglow.jsonschema

import json._
import json.Schema._
import json.Schema.`object`.Field
import json.schema.Predef
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

object AutoDerivationSpec {
  case class Foo(a: Int)
  case class HasFoo(foo: Foo, foos: List[Foo])
  case class Outer(inner: HasFoo, opt: Option[Foo])

  sealed trait Shape
  object Shape {
    case class Circle(r: Double) extends Shape
    case class Square(side: Foo) extends Shape
    case object Dot              extends Shape
  }
  case class Drawing(shapes: List[Shape])

  case class Node(name: String, children: List[Node])
  case class Forest(trees: List[Node])

  case class Explicit(x: String)
  object Explicit { implicit val schema: Schema[Explicit] = Json.schema[Explicit] }
  case class HasExplicit(e: Explicit, f: Foo)

  // an API that needs an implicit Predef, the reason to import Json.auto
  def predefOf[T](implicit p: Predef[T]): Schema[T] = p.schema

  // derived without the import, to compare with
  object Plain {
    val hasFoo: Schema[HasFoo]           = Json.schema[HasFoo]
    val outer: Schema[Outer]             = Json.schema[Outer]
    val drawing: Schema[Drawing]         = Json.schema[Drawing]
    val node: Schema[Node]               = Json.schema[Node]
    val forest: Schema[Forest]           = Json.schema[Forest]
    val hasExplicit: Schema[HasExplicit] = Json.schema[HasExplicit]
  }
}

/** `Json.auto` (0.7: scalac StackOverflowError on any use). Semantics (owner decision 2026-11-20): like plain derivation, i.e. an explicit implicit Schema[N] gives a definition + `$ref`, an implicit
  * Predef[N] is inlined, anything else is derived inline. `Json.auto.derived` itself never makes a definition and never shadows a built-in Predef.
  */
class AutoDerivationSpec extends AnyFunSuite {
  import AutoDerivationSpec._

  test("auto: an implicit Predef for a case class") {
    import Json.auto._
    implicitly[Predef[Foo]].schema shouldEqual `object`[Foo](Field("a", `integer`))
    predefOf[Foo] shouldEqual `object`[Foo](Field("a", `integer`))
  }

  test("auto: built-in Predefs still win (Int, String, List)") {
    import Json.auto._
    Json.schema[Int] shouldEqual `integer`
    implicitly[Predef[String]].schema shouldEqual `string`[String]
    predefOf[List[Int]] shouldEqual `array`[Int, List](`integer`)
  }

  test("auto: nested case classes are inlined, as without the import") {
    import Json.auto._
    Json.schema[HasFoo] shouldEqual Plain.hasFoo
    Json.schema[Outer] shouldEqual Plain.outer
    predefOf[Outer] shouldEqual Plain.outer
  }

  test("auto: sealed families") {
    import Json.auto._
    Json.schema[Drawing] shouldEqual Plain.drawing
    predefOf[Shape] shouldEqual Json.schema[Shape]
  }

  test("auto: recursive types") {
    import Json.auto._
    Json.schema[Node] shouldEqual Plain.node
    predefOf[Node] shouldEqual Plain.node
    Json.schema[Forest] shouldEqual Plain.forest
  }

  test("auto: an explicit implicit Schema wins and stays a definition") {
    import Json.auto._
    val s = Json.schema[HasExplicit]
    s shouldEqual Plain.hasExplicit
    // auto's own Predef keeps it too (this also uses the import: Json.schema never takes auto's given, so on Scala 3 the import is otherwise unused)
    predefOf[HasExplicit] shouldEqual s
    s shouldEqual `object`[HasExplicit](
      Field("e", `def`[Explicit]("com.github.andyglow.jsonschema.AutoDerivationSpec.Explicit", `object`[Explicit](Field("x", `string`)))),
      Field("f", `object`[Foo](Field("a", `integer`)))
    )
  }
}
