package com.github.andyglow.jsonschema

import json.{Json, Schema}
import json.schema.validation.Instance._
import json.Schema._
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

class SchemaMacroSpec extends AnyWordSpec {
  import SchemaMacroSpec._

  "Schema" should {

    "generate schema for case class of primitive fields" in {
      import `object`.Field

      val expected = `object`(
        Field("name", `string`),
        Field("bar" , `integer`))

      Json.schema[Foo1] shouldEqual expected
    }

    "generate schema for case class of optional primitive fields" in {
      import `object`.Field

      Json.schema[Foo2] shouldEqual `object`(
        Field("name", `string`, required = false),
        Field("bar" , `integer`, required = false))
    }

    "generate schema for case class of primitive fields with default values" in {
      import `object`.Field

      Json.schema[Foo3] shouldEqual `object`(
        Field("name"    , `string`, required = false, default = "xxx"),
        Field("bar"     , `integer`, required = false, default = 5),
        Field("active"  , `boolean`, required = false, default = true))
    }

    "generate schema for case class of array fields with default values" in {
      import `object`.Field

      Json.schema[Bar9] shouldEqual `object`(
        Field("set"     , `array`(`integer`, unique = true), required = false, default = Set(1, 5, 9)),
        Field("list"    , `array`(`boolean`), required = false, default = List(true, false)),
        Field("vector"  , `array`(`number`[Long]), required = false, default = Vector(9, 7)),
        Field("strMap"  , `dictionary`(`number`[Double]), required = false, default = Map("foo" -> .12)),
        Field("intMap"  , `dictionary`[Int, String, Map](`string`).withValidation(`patternProperties` := "^[0-9]+$"), required = false, default = Map(1 -> "1", 2 -> "2")))
    }

    "generate references for implicitly defined dependencies" in {
      import `object`.Field

      val compoSchema: Schema[Compo1] = Json.schema[Compo1]

      {
        implicit val _compoSchema: Schema[Compo1] = compoSchema

        // MUTE: local method _compoSchema in value <local SchemaMacroSpec> is never used
//        _compoSchema.refName

        val schema = Json.schema[Foo4]

        schema shouldEqual `object`(
          Field(
            "component",
            `def`[Compo1](
              "com.github.andyglow.jsonschema.SchemaMacroSpec.Compo1",
              `string`[Compo1]),
            required = true))
      }
    }

    "generate schema for scala.Enumeration" in {

      object WeekDay extends Enumeration {
        type T = Value
        val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
      }

      Json.schema[WeekDay.type] shouldEqual `enum`(Set("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
      Json.schema[WeekDay.Value] shouldEqual `enum`(Set("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
      Json.schema[WeekDay.T] shouldEqual `enum`(Set("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

      object Planet extends Enumeration {
        protected case class Val(mass: Double, radius: Double) extends super.Val {
          def surfaceGravity: Double = Planet.G * mass / (radius * radius)
          def surfaceWeight(otherMass: Double): Double = otherMass * surfaceGravity
        }
        import scala.language.implicitConversions
        implicit def valueToPlanetVal(x: Value): Val = x.asInstanceOf[Val]

        val G: Double = 6.67300E-11
        val Mercury = Val(3.303e+23, 2.4397e6)
        val Venus   = Val(4.869e+24, 6.0518e6)
        val Earth   = Val(5.976e+24, 6.37814e6)
        val Mars    = Val(6.421e+23, 3.3972e6)
        val Jupiter = Val(1.9e+27, 7.1492e7)
        val Saturn  = Val(5.688e+26, 6.0268e7)
        val Uranus  = Val(8.686e+25, 2.5559e7)
        val Neptune = Val(1.024e+26, 2.4746e7)
      }

      Json.schema[Planet.type] shouldEqual `enum`(Set("Mercury", "Venus", "Earth", "Mars", "Jupiter", "Saturn", "Uranus", "Neptune"))

      Json.schema[Map[WeekDay.type, String]] shouldEqual `dictionary`[WeekDay.type, String, Map](`string`).withValidation(`patternProperties` := "^(?:Mon|Tue|Wed|Thu|Fri|Sat|Sun)$")
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

      Json.schema[Map[Color, String]] shouldEqual `dictionary`[Color, String, Map](`string`).withValidation(`patternProperties` := "^(?:Red|Green|Blue)$")
    }

    "generate schema for Sealed Trait subclasses" in {
      import `object`.Field

      Json.schema[FooBar] shouldEqual `oneof`(Set(
        `object`(Field("foo", `number`[Double])),
        `object`(Field("bar", `number`[Double]))))
    }

    "generate schema for Multi Level Sealed Trait subclasses" in {
      import `object`.Field

      Json.schema[MultiLevelSealedTraitRoot] shouldEqual `oneof`(Set(
        `object`(Field("a", `integer`)),
        `object`(Field("b", `string`)),
        `object`(Field("c", `boolean`)),
        `object`(Field("d", `number`[Double])),
        `object`(Field("e", `array`[String, List](`string`)))))
    }

    "generate schema for Sealed Trait subclasses defined inside of it's companion object" in {
      import `object`.Field

      Json.schema[FooBarInsideCompanion] shouldEqual `oneof`(Set(
        `object`(Field("foo", `number`[Double])),
        `object`(Field("bar", `number`[Double]))))
    }


//    "generate schema for Map which Sealed Family for values" in {
//      import `object`.Field
//
//      Json.schema[Map[String, FooBar]] shouldEqual `string-map`[String, FooBar, Map](
//        `oneof`(Set(
//          `object`(Field("foo", `number`[Double]())),
//          `object`(Field("bar", `number`[Double]())))))
//    }

//    "generate schema for Map which Sealed Values Family for values" in {
//
//      Json.schema[Map[String, AnyFooBar]] shouldEqual `string-map`[String, AnyFooBar, Map](
//        `oneof`(Set(`string`[String](), `integer`)))
//    }

    "generate schema for case class that includes another case class" in {
      import `object`.Field

      Json.schema[Bar5] shouldEqual `object`(
        Field("foo", `object`(
          Field("name", `string`),
          Field("bar" , `integer`))))
    }

    "generate schema for case class using collection of string" in {
      import `object`.Field

      Json.schema[Bar6] shouldEqual `object`(
        Field("foo", `array`(`string`)))
    }

    "generate schema for case class using collection of integers" in {
      import `object`.Field

      Json.schema[Bar7] shouldEqual `object`(
        Field("foo", `array`(`integer`)))
    }

    "generate schema for value class" in {
      Json.schema[Bar8] shouldEqual `value-class`[Bar8, String](`string`)
    }

    "generate schema for Map[String, _]" in {
      import `object`.Field

      Json.schema[Map[String, String]] shouldEqual `dictionary`(`string`)

      Json.schema[Map[String, Int]] shouldEqual `dictionary`(`integer`)

      Json.schema[Map[String, Foo9]] shouldEqual `dictionary`[String, Foo9, Map](`object`(Field("name", `string`)))
    }

    "generate schema for Map[_: MapKeyPattern, _]" in {
      import `object`.Field

      Json.schema[Map[Long, Long]] shouldEqual `dictionary`[Long, Long, Map](`number`[Long]).withValidation(`patternProperties` := "^[0-9]+$")

      Json.schema[Map[Char, Long]] shouldEqual `dictionary`[Char, Long, Map](`number`[Long]).withValidation(`patternProperties` := "^.{1}$")

      Json.schema[Map[Int, String]] shouldEqual `dictionary`[Int, String, Map](`string`).withValidation(`patternProperties` := "^[0-9]+$")

      Json.schema[Map[Int, Int]] shouldEqual `dictionary`[Int, Int, Map](`integer`).withValidation(`patternProperties` := "^[0-9]+$")

      Json.schema[Map[Int, Foo9]] shouldEqual `dictionary`[Int, Foo9, Map](`object`(Field("name", `string`))).withValidation(`patternProperties` := "^[0-9]+$")
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

  case class Bar6(foo: List[String])

  case class Bar7(foo: List[Int])

  case class Bar8(foo: String) extends AnyVal

  case class Foo9(name: String)

  case class Bar9(
    set: Set[Int] = Set(1, 5, 9),
    list: List[Boolean] = List(true, false),
    vector: Vector[Long] = Vector(9L, 7L),
    strMap: Map[String, Double] = Map("foo" -> .12),
    intMap: Map[Int, String] = Map(1 -> "1", 2 -> "2"))

}

sealed trait FooBar
case class FooBar1(foo: Double) extends FooBar
case class FooBar2(bar: Double) extends FooBar

sealed trait MultiLevelSealedTraitRoot
sealed trait MultiLevelBranch1 extends MultiLevelSealedTraitRoot
sealed trait MultiLevelBranch2 extends MultiLevelSealedTraitRoot
case class MultiLevelBranch1Instance1(a: Int) extends MultiLevelBranch1
case class MultiLevelBranch1Instance2(b: String) extends MultiLevelBranch1
case class MultiLevelBranch2Instance1(c: Boolean) extends MultiLevelBranch2
case class MultiLevelBranch2Instance2(d: Double) extends MultiLevelBranch2
case class MultiLevelRootInstance1(e: List[String]) extends MultiLevelSealedTraitRoot

sealed trait FooBarInsideCompanion
object FooBarInsideCompanion {
  case class FooBarInsideCompanion1(foo: Double) extends FooBarInsideCompanion
  case class FooBarInsideCompanion2(bar: Double) extends FooBarInsideCompanion
}

sealed trait AnyFooBar extends Any
case class AnyFooBar1(value: String) extends AnyVal with AnyFooBar
case class AnyFooBar2(value: Int) extends AnyVal with AnyFooBar