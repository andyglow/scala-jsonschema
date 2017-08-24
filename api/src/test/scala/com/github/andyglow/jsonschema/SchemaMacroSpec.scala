package com.github.andyglow.jsonschema

import json.Json
import json.Schema._
import org.scalatest.Matchers._
import org.scalatest._

class SchemaMacroSpec extends WordSpec {
  import SchemaMacroSpec._

  "Schema" should {

    "generate schema for case class of primitive fields" in {
      import `object`.Field

      case class Foo(name: String, bar: Int)

      val expected = `object`(
        Field("name", `string`[String](None, None), required = true),
        Field("bar" , `integer`, required = true))

      Json.schema[Foo] shouldEqual expected
    }

    "generate schema for case class of optional primitive fields" in {
      import `object`.Field

      case class Foo(name: Option[String], bar: Option[Int])

      Json.schema[Foo] shouldEqual `object`(
        Field("name", `string`[String](None, None), required = false),
        Field("bar" , `integer`, required = false))
    }

    "generate schema for case class of primitive fields with default values" in {
      import `object`.Field

      case class Foo(name: String = "xxx", bar: Int = 5)

      Json.schema[Foo] shouldEqual `object`(
        Field("name", `string`[String](None, None), required = false),
        Field("bar" , `integer`, required = false))
    }

    "generate schema for Sealed Trait Enums" in {
      sealed trait Color
      object Color {
        case object Red extends Color
        case object Green extends Color
        case object Blue extends Color
      }

      Json.schema[Color] shouldEqual `enum`(Set("Red", "Green", "Blue"))
    }

    "generate schema for case class using another case class" in {
      import `object`.Field

      case class Foo(name: String, bar: Int)

      case class Bar(foo: Foo)

      Json.schema[Bar] shouldEqual `object`(
        Field("foo", `object`(
          Field("name", `string`[String](None, None), required = true),
          Field("bar" , `integer`, required = true))))
    }

    "generate schema for case class using collection of string" in {
      import `object`.Field

      case class Bar(foo: Iterable[String])

      Json.schema[Bar] shouldEqual `object`(
        Field("foo", `array`(`string`[String](None, None)), required = true))
    }

    "generate schema for case class using collection of integers" in {
      import `object`.Field

      case class Bar(foo: Iterable[Int])

      Json.schema[Bar] shouldEqual `object`(
        Field("foo", `array`(`integer`), required = true))
    }

    "generate schema for value class" in {
      Json.schema[Bar] shouldEqual `string`[String](None, None)
    }

    "generate schema for Map[String, _]" in {
      import `object`.Field

      case class Foo(name: String)

      Json.schema[Map[String, String]] shouldEqual `string-map`(`string`[String](None, None))

      Json.schema[Map[String, Int]] shouldEqual `string-map`(`integer`)

      Json.schema[Map[String, Foo]] shouldEqual `string-map`(`object`(Field("name", `string`[String](None, None), required = true)))
    }


    "generate schema for Map[Int, _]" in {
      import `object`.Field

      case class Foo(name: String)

      Json.schema[Map[Int, String]] shouldEqual `int-map`(`string`[String](None, None))

      Json.schema[Map[Int, Int]] shouldEqual `int-map`(`integer`)

      Json.schema[Map[Int, Foo]] shouldEqual `int-map`(`object`(Field("name", `string`[String](None, None), required = true)))
    }
  }

}

object SchemaMacroSpec {

  case class Bar(foo: String) extends AnyVal

}