package com.github.andyglow.jsonschema

import json.{Json, Schema}
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

      Json.schema[Foo3] shouldEqual `object`(
        Field("name"    , `string`[String](None, None), required = false),
        Field("bar"     , `integer`, required = false),
        Field("active"  , `boolean`, required = false))
    }

    "generate references for implicitly defined dependencies" in {
      import `object`.Field

      implicit val compoSchema: Schema[Compo1] = Json.schema[Compo1]

      val schema = Json.schema[Foo4]

      schema shouldEqual `object`(
        Field("component", `$ref`[Compo1](
          "com.github.andyglow.jsonschema.SchemaMacroSpec.Compo1",
          `string`[Compo1](None, None)),
          required = true))
    }

    "generate schema for Sealed Trait Enums" in {

      sealed trait Color

      object Color {

        case object Red extends Color

        case object Green extends Color

        case object Blue extends Color
      }

      // suppress compile warnings
      Color.Red
      Color.Green
      Color.Blue

      Json.schema[Color] shouldEqual `enum`(Set("Red", "Green", "Blue"))
    }

    "generate schema for Sealed Trait subclasses" in {
      import `object`.Field

      Json.schema[FooBar] shouldEqual `oneof`(Set(
        `object`(Field("foo", `number`[Double]())),
        `object`(Field("bar", `number`[Double]()))))
    }

    "generate schema for Map which Sealed Family for values" in {
      import `object`.Field

      Json.schema[Map[String, FooBar]] shouldEqual `string-map`(
        `oneof`(Set(
          `object`(Field("foo", `number`[Double]())),
          `object`(Field("bar", `number`[Double]())))))
    }

    "generate schema for Map which Sealed Values Family for values" in {

      Json.schema[Map[String, AnyFooBar]] shouldEqual `string-map`(
        `oneof`(Set(`string`[String](None, None), `integer`)))
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

  case class Compo1(value: String) extends AnyVal

  case class Foo1(name: String, bar: Int)

  case class Foo2(name: Option[String], bar: Option[Int])

  case class Foo3(name: String = "xxx", bar: Int = 5, active: Boolean = true)

  case class Foo4(component: Compo1)

  case class Foo5(name: String, bar: Int)

  case class Bar5(foo: Foo5)

  case class Bar6(foo: Iterable[String])

  case class Bar7(foo: Iterable[Int])

  case class Bar8(foo: String) extends AnyVal

  case class Foo9(name: String)
}

sealed trait FooBar
case class FooBar1(foo: Double) extends FooBar
case class FooBar2(bar: Double) extends FooBar

sealed trait AnyFooBar extends Any
case class AnyFooBar1(value: String) extends AnyVal with AnyFooBar
case class AnyFooBar2(value: Int) extends AnyVal with AnyFooBar
