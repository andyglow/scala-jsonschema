package com.example.unrelated

import json.{Json, Schema}
import org.scalatest._

/** This spec checks that the macro works even when names from
  * `com.github.andyglow.jsonschema` are not in scope.
  */
class SchemaMacroNamesSpec extends WordSpec {
  import SchemaNamesSpec._

  "Schema" should {

    "generate a schema with a reference to another schema" in {
      implicit val fooSchema: Schema[Foo] = Json.schema[Foo]
      implicit val barSchema: Schema[Bar] = Json.schema[Bar] // should compile
    }

  }
}

private object SchemaNamesSpec {
  final case class Foo(s: String)
  final case class Bar(foo: Foo)
}
