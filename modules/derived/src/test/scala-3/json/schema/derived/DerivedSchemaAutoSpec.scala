package json.schema.derived

import json.{Json, Schema}
import json.Schema._
import json.Schema.`object`.Field
import json.schema.Predef
import org.scalatest.funsuite._
import org.scalatest.matchers.should.Matchers._

object DerivedSchemaAutoSpec {
  case class Foo(a: Int)

  /** A container with a `Predef` constructor that needs its element's `Predef` (like cats' `NonEmptyList`). */
  case class Box[T](items: List[T])
  object Box {
    implicit def boxPredef[T](implicit p: Predef[T]): Predef[Box[T]] = Predef(`array`[T, List](p.schema).asInstanceOf[Schema[Box[T]]])
  }
  case class HasBox(foos: Box[Foo], foo: Foo)
  case class Node(name: String, children: Box[Node])
}

/** Scala 3: `DerivedSchema` runs in typer, so with `Json.auto` imported its searches get the unexpanded `auto.derived[X]` back; those are holes too (issue #542). */
class DerivedSchemaAutoSpec extends AnyFunSuite {
  import DerivedSchemaAutoSpec._

  test("Json.auto: a container with a Predef constructor gets its element from auto") {
    import json.Json.auto._
    implicitly[DerivedSchema[HasBox]].schema shouldBe `object`[HasBox](
      Field("foos", `array`[Foo, List](`object`[Foo](Field("a", `integer`)))),
      Field("foo", `object`[Foo](Field("a", `integer`)))
    )
  }

  test("Json.auto: recursion through a container with a Predef constructor") {
    import json.Json.auto._
    val sig = Json.sig[Node].signature
    implicitly[DerivedSchema[Node]].schema shouldBe `def`[Node](sig, `object`[Node](Field("name", `string`), Field("children", `array`[Node, List](`ref`[Node](sig)))))
  }
}
