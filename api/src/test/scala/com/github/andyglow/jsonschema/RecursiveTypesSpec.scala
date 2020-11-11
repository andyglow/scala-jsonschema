package com.github.andyglow.jsonschema

import com.github.andyglow.json.JsonFormatter
import json.Schema
import json.Schema._
import json.Schema.`object`.Field
import json.schema.Version
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec._

// comes first because otherwise it fails on directKnownSubclass resolution
object RecursiveTypesSpec {

  final object case1 {
    final case class SList(head: String, tail: Option[SList])
  }

  final object case2 {
    sealed trait Node
    final object Node {
      final case class Element(tag: String, children: List[Node]) extends Node
      final case class Text(content: String) extends Node
    }
  }

  final object case3 {
    final case class LList[T](head: T, tail: Option[LList[T]])
    final case class User(id: String, friends: LList[String])
  }

  def render(s: Schema[_]): Unit = println {
    JsonFormatter format AsValue.schema(s, Version.Draft07("examples"))
  }
}

class RecursiveTypesSpec extends AnyWordSpec {
  import RecursiveTypesSpec._

  "recursive types" should {

    "be supported" when {

      "product types. inner references root" in {
        import case1._

        json.Json.schema[SList] shouldEqual `ref`(
          "com.github.andyglow.jsonschema.RecursiveTypesSpec.case1.SList",
          `object`(
            Field("head", `string`(), required = true),
            Field("tail", `lazy-ref`("com.github.andyglow.jsonschema.RecursiveTypesSpec.case1.SList"), required = false)))
      }

      "product types. inner references root. specific ref-name" in {
        import case1._

        json.Json.schema[SList]("given-name") shouldEqual `ref`(
          "given-name",
          `object`(
            Field("head", `string`(), required = true),
            Field("tail", `lazy-ref`("given-name"), required = false)))
      }

      "sum types. inner references root" in {
        import case2._

        json.Json.schema[Node] shouldEqual `ref`(
          "com.github.andyglow.jsonschema.RecursiveTypesSpec.case2.Node",
          `oneof`.of(
            `object`(
              Field("tag", `string`(), required = true),
              Field("children", `array`(`lazy-ref`("com.github.andyglow.jsonschema.RecursiveTypesSpec.case2.Node")), required = true)),
            `object`(
              Field("content", `string`(), required = true))))
      }

      "inner references non-root" in {
        import case3._

        json.Json.schema[User] shouldEqual `object`(
          Field("id", `string`(), required = true),
          Field("friends", `ref`(
            "com.github.andyglow.jsonschema.RecursiveTypesSpec.case3.LList[scala.Predef.String]",
            `object`(
              Field("head", `string`(), required = true),
              Field("tail", `lazy-ref`("com.github.andyglow.jsonschema.RecursiveTypesSpec.case3.LList[scala.Predef.String]"), required = false)))))
      }

      "inner is explicitly given" in {
        import case3._

        implicit def llist[T](implicit t: Schema[T]): Schema[LList[T]] = json.Json.schema[LList[T]]
        json.Json.schema[User] shouldEqual `object`(
          Field("id", `string`(), required = true),
          Field("friends", `ref`(
            "com.github.andyglow.jsonschema.RecursiveTypesSpec.case3.LList[scala.Predef.String]",
            `object`(
              Field("head", `string`(), required = true),
              Field("tail", `lazy-ref`("com.github.andyglow.jsonschema.RecursiveTypesSpec.case3.LList[scala.Predef.String]"), required = false)))))
      }
    }
  }
}
