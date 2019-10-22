package com.github.andyglow.jsonschema

import json.Json
import json.Schema.{`number`, `object`, `oneof`}
import org.scalatest._
import org.scalatest.Matchers._

class SchemaMacro213Spec extends WordSpec {
  import SchemaMacro213Spec._

  "generate schema for Sealed Trait subclasses defined inside of another object" in {
    import `object`.Field

    Json.schema[NestedFooBar] shouldEqual `oneof`(Set(
      `object`(Field("foo", `number`[Double]())),
      `object`(Field("bar", `number`[Double]()))))
  }
}

object SchemaMacro213Spec {

  sealed trait NestedFooBar
  object NestedFooBar {
    case class NestedFooBar1(foo: Double) extends NestedFooBar
    case class NestedFooBar2(bar: Double) extends NestedFooBar
  }
}
