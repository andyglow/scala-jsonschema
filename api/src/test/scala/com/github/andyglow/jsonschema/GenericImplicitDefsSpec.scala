package com.github.andyglow.jsonschema

import json.Json
import json.Schema
import json.Schema._
import json.Schema.`object`.Field
import org.scalatest.matchers.should.Matchers._
import org.scalatest.funsuite.AnyFunSuite

// #297: a generic implicit `def f[A: Schema]: Schema[Foo[A]] = Json.schema[Foo[A]]`. Inside it, `A` is a type parameter: no definition name known at compile time is right for every `A`.
class GenericImplicitDefsSpec extends AnyFunSuite {
  import GenericImplicitDefsSpec._

  private val pkg = "com.github.andyglow.jsonschema.GenericImplicitDefsSpec"
  private val bar = `object`[Bar](Field("x", `integer`))
  private val baz = `object`[Baz](Field("y", `string`))

  test("#297: Foo[Bar] and Foo[Baz] through a generic implicit def are two definitions, each with its own body for A") {
    import Plain._
    Json.schema[Quux] shouldEqual `object`[Quux](
      Field("foobar", `def`[Foo[Bar]](s"$pkg.Foo[$pkg.Bar]", `object`[Foo[Bar]](Field("data", bar)))),
      Field("foobaz", `def`[Foo[Baz]](s"$pkg.Foo[$pkg.Baz]", `object`[Foo[Baz]](Field("data", baz))))
    )
  }

  test("#297: an evidence that is already a definition keeps its own name inside the generic def") {
    import Named._
    Json.schema[Quux] shouldEqual `object`[Quux](
      Field("foobar", `def`[Foo[Bar]](s"$pkg.Foo[$pkg.Bar]", `object`[Foo[Bar]](Field("data", `def`[Bar]("Bar", bar))))),
      Field("foobaz", `def`[Foo[Baz]](s"$pkg.Foo[$pkg.Baz]", `object`[Foo[Baz]](Field("data", `def`[Baz]("Baz", baz)))))
    )
  }

  private def treeDef(sig: String, v: Schema[_]) =
    `def`[Any](sig, `object`[Any](Field("v", v), Field("kids", `array`(`ref`[Any](sig)))))

  test("#297: a recursive Tree[A] through a generic implicit def: Tree[Bar] and Tree[Baz] are two definitions, refs included") {
    import Plain._
    Json.schema[Forest] shouldEqual `object`[Forest](
      Field("a", treeDef(s"$pkg.Tree[$pkg.Bar]", bar)),
      Field("b", treeDef(s"$pkg.Tree[$pkg.Baz]", baz))
    )
  }

  test("#297: a definition a generic implicit def names itself is renamed to the concrete type") {
    import UserNamed._
    Json.schema[Quux] shouldEqual `object`[Quux](
      Field("foobar", `def`[Foo[Bar]](s"$pkg.Foo[$pkg.Bar]", `object`[Foo[Bar]](Field("data", bar)))),
      Field("foobaz", `def`[Foo[Baz]](s"$pkg.Foo[$pkg.Baz]", `object`[Foo[Baz]](Field("data", baz))))
    )
  }

  test("#297: an implicit Schema for an abstract type member is used as it is, not named after the member") {
    BarMembers.fooSchema shouldEqual `object`[Foo[Bar]](Field("data", bar))
  }

  test("#297: an implicit Schema for an existential type is still a definition") {
    import Existential._
    Json.schema[HasAny] match {
      case o: `object`[_] => o.fields.head.tpe shouldBe a[`def`[_]]
      case other          => fail(s"not an object: $other")
    }
  }
}

object GenericImplicitDefsSpec {
  case class Foo[A](data: A)
  case class Tree[A](v: A, kids: List[Tree[A]])
  case class Bar(x: Int)
  case class Baz(y: String)
  case class Quux(foobar: Foo[Bar], foobaz: Foo[Baz])
  case class Forest(a: Tree[Bar], b: Tree[Baz])

  object Plain {
    implicit def jsonSchemaFoo[A: Schema]: Schema[Foo[A]]   = Json.schema[Foo[A]]
    implicit def jsonSchemaTree[A: Schema]: Schema[Tree[A]] = Json.schema[Tree[A]]
    implicit val jsonSchemaBar: Schema[Bar]                 = Json.schema[Bar]
    implicit val jsonSchemaBaz: Schema[Baz]                 = Json.schema[Baz]
  }

  object Named {
    implicit def jsonSchemaFoo[A: Schema]: Schema[Foo[A]] = Json.schema[Foo[A]]
    implicit val jsonSchemaBar: Schema[Bar]               = Json.schema[Bar].toDefinition("Bar")
    implicit val jsonSchemaBaz: Schema[Baz]               = Json.schema[Baz].toDefinition("Baz")
  }

  object UserNamed {
    implicit def jsonSchemaFoo[A: Schema]: Schema[Foo[A]] = Json.schema[Foo[A]].toDefinition("Foo")
    implicit val jsonSchemaBar: Schema[Bar]               = Json.schema[Bar]
    implicit val jsonSchemaBaz: Schema[Baz]               = Json.schema[Baz]
  }

  abstract class Members {
    type T
    implicit def tSchema: Schema[T]

    // (a blank line: Scala 2's source-text self-reference check reads the line above, README "Known issues")
    lazy val fooSchema: Schema[Foo[T]] = Json.schema[Foo[T]]
  }
  object BarMembers extends Members {
    type T = Bar
    implicit def tSchema: Schema[Bar] = Json.schema[Bar]
  }

  case class HasAny(f: Foo[_])
  object Existential {
    implicit val jsonSchemaFooAny: Schema[Foo[_]] = Json.schema[Foo[String]].asInstanceOf[Schema[Foo[_]]]
  }
}
