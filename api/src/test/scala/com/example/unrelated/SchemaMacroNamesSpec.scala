package com.example.unrelated

import org.scalatest._
import org.scalatest.Matchers._

/** This spec checks that the macro works even when names from
  * `com.github.andyglow.jsonschema` are not in scope.
  */
class SchemaMacroNamesSpec extends WordSpec {

  "Schema" should {

    "generate a schema with a reference to another schema" in {
      """import json._
        |import SchemaNamesSpec._
        |
        |implicit val fooSchema: Schema[Foo] = Json.schema[Foo]
        |implicit val barSchema: Schema[Bar] = Json.schema[Bar]
      """.stripMargin should compile
    }
  }
}

object SchemaNamesSpec {
  final case class Foo(s: String)
  final case class Bar(foo: Foo)
}
