package com.github.andyglow.jsonschema

import json._
import org.scalatest.matchers.should.Matchers._
import org.scalatest.funsuite.AnyFunSuite
import com.github.andyglow.jsonschema.{model => case0}

object NoneForDefaultCirceCases {

  object case1 {
    case class Foo(value: Option[String] = None)
  }

  object case3 {

    case class Bar(name: String, innerLevel2: Option[String] = None)

    case class Foo(name: String, inner: Option[Bar] = None)
  }
}

object case2 {

  case class Foo(value: Option[String] = None)
}

class NoneForDefaultCirceSpec extends AnyFunSuite {
  import AsCirceSpec.jsValEq
  import NoneForDefaultCirceCases._
  import json.schema.Version.Draft07
  import io.circe.{ Json => Circe }
  import io.circe.generic.auto._
  import AsCirce._

  test("scala.None for default value") {

    // type id specified in external file
    Json.schema[case0.Foo].asCirce(Draft07("s0")) shouldEqual Circe.obj(
      """$id""" -> Circe.fromString("s0"),
      """$schema""" -> Circe.fromString("http://json-schema.org/draft-07/schema#"),
      "properties" -> Circe.obj(
        "value" -> Circe.obj(
          "type" -> Circe.fromString("string")
        )
      ),
      "type" -> Circe.fromString("object"),
      "additionalProperties" -> Circe.fromBoolean(false)
    )

    // type is specified in same file
    Json.schema[case2.Foo].asCirce(Draft07("s2")) shouldEqual Circe.obj(
      """$id""" -> Circe.fromString("s2"),
      """$schema""" -> Circe.fromString("http://json-schema.org/draft-07/schema#"),
      "properties" -> Circe.obj(
        "value" -> Circe.obj(
          "type" -> Circe.fromString("string")
        )
      ),
      "type" -> Circe.fromString("object"),
      "additionalProperties" -> Circe.fromBoolean(false)
    )

    // type is specified in same file, companion object
    Json.schema[case1.Foo].asCirce(Draft07("s1")) shouldEqual Circe.obj(
      """$id""" -> Circe.fromString("s1"),
      """$schema""" -> Circe.fromString("http://json-schema.org/draft-07/schema#"),
      "properties" -> Circe.obj(
        "value" -> Circe.obj(
          "type" -> Circe.fromString("string")
        )
      ),
      "type" -> Circe.fromString("object"),
      "additionalProperties" -> Circe.fromBoolean(false)
    )

    // type is specified in same file, companion object
    Json.schema[case3.Foo].asCirce(Draft07("s3")) shouldEqual Circe.obj(
      """$id""" -> Circe.fromString("s3"),
      """$schema""" -> Circe.fromString("http://json-schema.org/draft-07/schema#"),
      "type" -> Circe.fromString("object"),
      "additionalProperties" -> Circe.fromBoolean(false),
      "required" -> Circe.arr(Circe.fromString("name")),
      "properties" -> Circe.obj(
        "name" -> Circe.obj(
          "type" -> Circe.fromString("string")
        ),
        "inner" -> Circe.obj(
          "type" -> Circe.fromString("object"),
          "additionalProperties" -> Circe.fromBoolean(false),
          "required" -> Circe.arr(Circe.fromString("name")),
          "properties" -> Circe.obj(
            "name" -> Circe.obj(
              "type" -> Circe.fromString("string")
            ),
            "innerLevel2" -> Circe.obj(
              "type" -> Circe.fromString("string")
            )
          )
        )
      )
    )
  }
}
