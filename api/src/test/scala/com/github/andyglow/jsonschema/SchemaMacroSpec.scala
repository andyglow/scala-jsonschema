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

      val expected = `object`(
        Field("name", `string`[String](None, None), required = true),
        Field("bar" , `integer`, required = true))

      Json.schema[Foo1] shouldEqual expected
    }

    "generate schema for case class of optional primitive fields" in {
      import `object`.Field

      Json.schema[Foo2] shouldEqual `object`(
        Field("name", `string`[String](None, None), required = false),
        Field("bar" , `integer`, required = false))
    }

    "generate schema for case class of primitive fields with default values" in {
      import `object`.Field

      // for locally defined case class
      Json.schema[Foo3] shouldEqual `object`(
        Field("name"    , `string`[String](None, None), required = false),
        Field("bar"     , `integer`, required = false),
        Field("active"  , `boolean`, required = false))

      // for case class defined in companion
      Json.schema[Foo4] shouldEqual `object`(
        Field("name"    , `string`[String](None, None), required = false),
        Field("bar"     , `integer`, required = false),
        Field("active"  , `boolean`, required = false))
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

      Json.schema[Bar5] shouldEqual `object`(
        Field("foo", `object`(
          Field("name", `string`[String](None, None), required = true),
          Field("bar" , `integer`, required = true))))
    }

    "generate schema for case class using collection of string" in {
      import `object`.Field

      Json.schema[Bar6] shouldEqual `object`(
        Field("foo", `array`(`string`[String](None, None)), required = true))
    }

    "generate schema for case class using collection of integers" in {
      import `object`.Field

      Json.schema[Bar7] shouldEqual `object`(
        Field("foo", `array`(`integer`), required = true))
    }

    "generate schema for value class" in {
      Json.schema[Bar8] shouldEqual `string`[String](None, None)
    }

    "generate schema for Map[String, _]" in {
      import `object`.Field

      Json.schema[Map[String, String]] shouldEqual `string-map`(`string`[String](None, None))

      Json.schema[Map[String, Int]] shouldEqual `string-map`(`integer`)

      Json.schema[Map[String, Foo9]] shouldEqual `string-map`(`object`(Field("name", `string`[String](None, None), required = true)))
    }


    "generate schema for Map[Int, _]" in {
      import `object`.Field

      Json.schema[Map[Int, String]] shouldEqual `int-map`(`string`[String](None, None))

      Json.schema[Map[Int, Int]] shouldEqual `int-map`(`integer`)

      Json.schema[Map[Int, Foo9]] shouldEqual `int-map`(`object`(Field("name", `string`[String](None, None), required = true)))
    }
  }

}

object SchemaMacroSpec {

  case class Foo1(name: String, bar: Int)

  case class Foo2(name: Option[String], bar: Option[Int])

  case class Foo3(name: String = "xxx", bar: Int = 5, active: Boolean = true)

  case class Foo4(name: String = "xxx", bar: Int = 5, active: Boolean = true)

  case class Foo5(name: String, bar: Int)

  case class Bar5(foo: Foo5)

  case class Bar6(foo: Iterable[String])

  case class Bar7(foo: Iterable[Int])

  case class Bar8(foo: String) extends AnyVal

  case class Foo9(name: String)
}