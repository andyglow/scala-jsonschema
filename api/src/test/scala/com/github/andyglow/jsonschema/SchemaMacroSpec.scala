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
        Field("name"    , `string`[String](None, None), required = false, default = "xxx"),
        Field("bar"     , `integer`, required = false, default = 5),
        Field("active"  , `boolean`, required = false, default = true))
    }

    "generate schema for case class of array fields with default values" in {
      import `object`.Field

      Json.schema[Bar9] shouldEqual `object`(
        Field("seq"     , `array`(`string`[String](None, None)), required = false, default = Seq.empty[String]),
        Field("set"     , `set`(`integer`), required = false, default = Set(1, 5, 9)),
        Field("list"    , `array`(`boolean`), required = false, default = List(true, false)),
        Field("vector"  , `array`(`number`[Long]), required = false, default = Vector(9, 7)),
        Field("strMap"  , `string-map`(`number`[Double]), required = false, default = Map("foo" -> .12)),
        Field("intMap"  , `int-map`(`string`[String](None, None)), required = false, default = Map(1 -> "1", 2 -> "2")))
    }

    "generate references for implicitly defined dependencies" in {
      import `object`.Field

      implicit val compoSchema: Schema[Compo1] = Json.schema[Compo1]
      compoSchema.refName // need this to workaround compiler warning that compoSchema is not used

      val schema = Json.schema[Foo4]

      schema shouldEqual `object`(
        Field(
          "component",
          `ref`[Compo1](
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

  case class Bar9(
    seq: Seq[String] = Seq.empty,
    set: Set[Int] = Set(1, 5, 9),
    list: List[Boolean] = List(true, false),
    vector: Vector[Long] = Vector(9L, 7L),
    strMap: Map[String, Double] = Map("foo" -> .12),
    intMap: Map[Int, String] = Map(1 -> "1", 2 -> "2"))
}

sealed trait FooBar
case class FooBar1(foo: Double) extends FooBar
case class FooBar2(bar: Double) extends FooBar

sealed trait AnyFooBar extends Any
case class AnyFooBar1(value: String) extends AnyVal with AnyFooBar
case class AnyFooBar2(value: Int) extends AnyVal with AnyFooBar
