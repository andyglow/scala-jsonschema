package com.github.andyglow.jsonschema

import json.{Json, Schema}
import json.schema.validation.Instance._
import json.Schema._
import json.schema.Version.Raw
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec
import SchemaMatchers._

class SchemaMacroSpec extends AnyWordSpec {
  import SchemaMacroSpec._

  "Schema" should {

    "generate schema for case class of primitive fields" in {
      import `object`.Field

      val expected = `object`(
        Field("name", `string`),
        Field("bar", `integer`)
      )

      Json.schema[Foo1] should matchSchema(expected)
    }

    "generate schema for case class of optional primitive fields" in {
      import `object`.Field

      Json.schema[Foo2] should matchSchema(
        `object`(
          Field("name", `string`, required = false),
          Field("bar", `integer`, required = false)
        )
      )
    }

    "generate schema for case class of primitive fields with default values" in {
      import `object`.Field

      Json.schema[Foo3] should matchSchema(
        `object`(
          Field("name", `string`, required = false, default = "xxx"),
          Field("bar", `integer`, required = false, default = 5),
          Field("active", `boolean`, required = false, default = true)
        )
      )
    }

    "generate schema for case class of array fields with default values" in {
      import `object`.Field

      Json.schema[Bar9] should matchSchema(
        `object`(
          Field("set", `array`(`integer`, unique = true), required = false, default = Set(1, 5, 9)),
          Field("list", `array`(`boolean`), required = false, default = List(true, false)),
          Field("vector", `array`(`number`[Long]), required = false, default = Vector(9, 7)),
          Field(
            "strMap",
            `dictionary`(`number`[Double]),
            required = false,
            default = Map("foo" -> .12)
          ),
          Field(
            "intMap",
            `dictionary`[Int, String, Map](`string`)
              .withValidation(`patternProperties` := "^[0-9]+$"),
            required = false,
            default = Map(1 -> "1", 2 -> "2")
          )
        )
      )
    }

    "generate references for implicitly defined dependencies" in {
      import `object`.Field

      val compoSchema: Schema[Compo1] = Json.schema[Compo1]

      {
        implicit val _compoSchema: Schema[Compo1] = compoSchema

        // MUTE: local method _compoSchema in value <local SchemaMacroSpec> is never used
//        _compoSchema.refName

        val schema = Json.schema[Foo4]

        schema should matchSchema(
          `object`(
            Field(
              "component",
              `def`[Compo1](
                "com.github.andyglow.jsonschema.SchemaMacroSpec.Compo1",
                `string`[Compo1]
              ),
              required = true
            )
          )
        )
      }
    }

    "generate schema for scala.Enumeration" in {

      object WeekDay extends Enumeration {
        type T = Value
        val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
      }

      Json
        .schema[WeekDay.type] should matchSchema(`enum`.of("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

      Json.schema[WeekDay.Value] should matchSchema(
        `enum`.of(
          "Mon",
          "Tue",
          "Wed",
          "Thu",
          "Fri",
          "Sat",
          "Sun"
        )
      )

      Json.schema[WeekDay.T] should matchSchema(`enum`.of("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

      object Planet extends Enumeration {
        protected case class PlanetVal(mass: Double, radius: Double) extends super.Val {
          def surfaceGravity: Double                   = Planet.G * mass / (radius * radius)
          def surfaceWeight(otherMass: Double): Double = otherMass * surfaceGravity
        }
        import scala.language.implicitConversions
        implicit def valueToPlanetVal(x: Value): PlanetVal = x.asInstanceOf[PlanetVal]

        val G: Double = 6.67300e-11
        val Mercury   = PlanetVal(3.303e+23, 2.4397e6)
        val Venus     = PlanetVal(4.869e+24, 6.0518e6)
        val Earth     = PlanetVal(5.976e+24, 6.37814e6)
        val Mars      = PlanetVal(6.421e+23, 3.3972e6)
        val Jupiter   = PlanetVal(1.9e+27, 7.1492e7)
        val Saturn    = PlanetVal(5.688e+26, 6.0268e7)
        val Uranus    = PlanetVal(8.686e+25, 2.5559e7)
        val Neptune   = PlanetVal(1.024e+26, 2.4746e7)
      }

      Json.schema[Planet.type] should matchSchema(
        `enum`.of(
          "Mercury",
          "Venus",
          "Earth",
          "Mars",
          "Jupiter",
          "Saturn",
          "Uranus",
          "Neptune"
        )
      )

      Json.schema[Map[WeekDay.type, String]] should matchSchema(
        `dictionary`[WeekDay.type, String, Map](
          `string`
        ).withValidation(`patternProperties` := "^(?:Mon|Tue|Wed|Thu|Fri|Sat|Sun)$")
      )
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

      Json.schema[Color] should matchSchema(`enum`.of("Red", "Green", "Blue"))

      Json.schema[Map[Color, String]] should matchSchema(
        `dictionary`[Color, String, Map](`string`)
          .withValidation(`patternProperties` := "^(?:Red|Green|Blue)$")
      )
    }

    "generate schema for Sealed Trait subclasses" in {
      import `object`.Field

      Json.schema[FooBar] should matchSchema(
        `oneof`(
          Set(`object`(Field("foo", `number`[Double])), `object`(Field("bar", `number`[Double])))
        )
      )
    }

    "generate schema for Multi Level Sealed Trait subclasses" in {
      import `object`.Field

      Json.schema[MultiLevelSealedTraitRoot] should matchSchema(
        `oneof`(
          Set(
            `object`(Field("a", `integer`)),
            `object`(Field("b", `string`)),
            `object`(Field("c", `boolean`)),
            `object`(Field("d", `number`[Double])),
            `object`(Field("e", `array`[String, List](`string`)))
          )
        )
      )
    }

    "generate schema for Sealed Trait subclasses defined inside of it's companion object" in {
      import `object`.Field

      Json.schema[FooBarInsideCompanion] should matchSchema(
        `oneof`(
          Set(`object`(Field("foo", `number`[Double])), `object`(Field("bar", `number`[Double])))
        )
      )
    }

    "generate schema for hybrid Sealed Trait family" in {
      import `object`.Field

      Json.schema[HybridSum] should matchSchema(
        `oneof`(
          Set(`object`(Field("id", `integer`), Field("name", `string`)), `enum`.of("V2", "V3"))
        )
      )
    }

    "generate schema for hybrid generic Sealed Trait family" in {
      import `object`.Field

      Json.schema[HybridGenericSum[Double]] should matchSchema(
        `oneof`(
          Set(`object`(Field("id", `integer`), Field("value", `number`[Double])), `enum`.of("V2"))
        )
      )
    }

    "generate schema for hybrid recursive Sealed Trait family" in {
      import `object`.Field

      Json.schema[HybridRecursiveSum] should matchSchema(
        `oneof`(
          Set(
            `object`(Field("err", `string`, required = false), Field("intVal", `integer`)),
            `object`(Field("tpe", `string`)),
            `object`(Field("error", `string`)),
            `object`(Field("value", `integer`)),
            `enum`.of("L1V3", "L2V1", "L2V2")
          )
        )
      )
    }

    "generate schema for Map which Sealed Values Family for values" in {

      Json.schema[Map[String, AnyFooBar]] should matchSchema(
        `dictionary`[String, AnyFooBar, Map](
          `oneof`.of(`value-class`(`string`), `value-class`(`integer`))
        )
      )
    }

    "generate schema for List which Sealed Values Family for values" in {

      Json.schema[List[AnyFooBar]] should matchSchema(
        `array`[AnyFooBar, List](
          `oneof`.of(`value-class`(`string`), `value-class`(`integer`))
        )
      )
    }

    "generate schema for case class that includes another case class" in {
      import `object`.Field

      Json.schema[Bar5] should matchSchema(
        `object`(
          Field("foo", `object`(Field("name", `string`), Field("bar", `integer`)))
        )
      )
    }

    "generate schema for case class using collection of string" in {
      import `object`.Field

      Json.schema[Bar6] should matchSchema(`object`(Field("foo", `array`(`string`))))
    }

    "generate schema for case class using collection of integers" in {
      import `object`.Field

      Json.schema[Bar7] should matchSchema(`object`(Field("foo", `array`(`integer`))))
    }

    "generate schema for value class" in {
      Json.schema[Bar8] should matchSchema(`value-class`[Bar8, String](`string`))
    }

    "generate schema for Map[String, _]" in {
      import `object`.Field

      Json.schema[Map[String, String]] should matchSchema(`dictionary`(`string`))

      Json.schema[Map[String, Int]] should matchSchema(`dictionary`(`integer`))

      Json.schema[Map[String, Foo9]] should matchSchema(
        `dictionary`[String, Foo9, Map](
          `object`(Field("name", `string`))
        )
      )

      Json.schema[Map[Bar8, Int]] should matchSchema(`dictionary`[Bar8, Int, Map](`integer`))

      import com.github.andyglow.json.Value._

      AsValue.schema(Json.schema[Map[Bar8, Int]], Raw) shouldEqual obj(
        "type" -> "object",
        "patternProperties" -> obj(
          "^.*$" -> obj("type" -> "integer")
        )
      )
    }

    "generate schema for Map[_: MapKeyPattern, _]" in {
      import `object`.Field

      Json.schema[Map[Long, Long]] should matchSchema(
        `dictionary`[Long, Long, Map](`number`[Long])
          .withValidation(`patternProperties` := "^[0-9]+$")
      )

      Json.schema[Map[Char, Long]] should matchSchema(
        `dictionary`[Char, Long, Map](`number`[Long])
          .withValidation(`patternProperties` := "^.{1}$")
      )

      Json.schema[Map[Int, String]] should matchSchema(
        `dictionary`[Int, String, Map](`string`)
          .withValidation(`patternProperties` := "^[0-9]+$")
      )

      Json.schema[Map[Int, Int]] should matchSchema(
        `dictionary`[Int, Int, Map](`integer`).withValidation(
          `patternProperties` := "^[0-9]+$"
        )
      )

      Json.schema[Map[Int, Foo9]] should matchSchema(
        `dictionary`[Int, Foo9, Map](
          `object`(Field("name", `string`))
        ).withValidation(`patternProperties` := "^[0-9]+$")
      )
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
    intMap: Map[Int, String] = Map(1 -> "1", 2 -> "2")
  )

}

sealed trait FooBar
case class FooBar1(foo: Double) extends FooBar
case class FooBar2(bar: Double) extends FooBar

sealed trait MultiLevelSealedTraitRoot
sealed trait MultiLevelBranch1                      extends MultiLevelSealedTraitRoot
sealed trait MultiLevelBranch2                      extends MultiLevelSealedTraitRoot
case class MultiLevelBranch1Instance1(a: Int)       extends MultiLevelBranch1
case class MultiLevelBranch1Instance2(b: String)    extends MultiLevelBranch1
case class MultiLevelBranch2Instance1(c: Boolean)   extends MultiLevelBranch2
case class MultiLevelBranch2Instance2(d: Double)    extends MultiLevelBranch2
case class MultiLevelRootInstance1(e: List[String]) extends MultiLevelSealedTraitRoot

sealed trait FooBarInsideCompanion
object FooBarInsideCompanion {
  case class FooBarInsideCompanion1(foo: Double) extends FooBarInsideCompanion
  case class FooBarInsideCompanion2(bar: Double) extends FooBarInsideCompanion
}

sealed trait AnyFooBar               extends Any
case class AnyFooBar1(value: String) extends AnyVal with AnyFooBar
case class AnyFooBar2(value: Int)    extends AnyVal with AnyFooBar

sealed trait HybridSum
object HybridSum {
  case class V1(id: Int, name: String) extends HybridSum
  case object V2                       extends HybridSum
  case object V3                       extends HybridSum
}

sealed trait HybridGenericSum[+T]
object HybridGenericSum {
  case class V1[T](id: Int, value: T) extends HybridGenericSum[T]
  case object V2                      extends HybridGenericSum[Nothing]
}

sealed trait HybridRecursiveSum
object HybridRecursiveSum {
  sealed trait Level1            extends HybridRecursiveSum
  case class L1V1(error: String) extends Level1
  case class L1V2(value: Int)    extends Level1
  case object L1V3               extends Level1

  sealed trait Level2          extends HybridRecursiveSum { def tpe: String   }
  case object L2V1             extends Level2             { def tpe = "l2-v1" }
  case object L2V2             extends Level2             { def tpe = "l2-v2" }
  case class L2V3(tpe: String) extends Level2

  case class Generic(err: Option[String], intVal: Int) extends HybridRecursiveSum
}
