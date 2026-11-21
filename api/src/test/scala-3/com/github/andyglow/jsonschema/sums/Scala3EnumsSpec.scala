package com.github.andyglow.jsonschema.sums

import com.github.andyglow.json.{ToValue, Value}
import com.github.andyglow.jsonschema.harness.CompileErrors.errorOf
import json.*
import json.Schema.*
import json.Schema.`object`.Field
import json.schema.{definition, description, discriminator, discriminatorKey, title, typeHint, Flag}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*

object Scala3EnumsSpec {
  enum Color { case Red, Green, Blue }

  enum Planet(val mass: Double) {
    case Mercury extends Planet(3.303e+23)
    case Venus   extends Planet(4.869e+24)
  }

  enum Shape {
    case Circle(r: Double)
    case Rect(w: Double, h: Double)
    case Point
  }

  enum Opt[+T] {
    case Has(v: T)
    case Nada
  }

  enum Tree {
    case Leaf
    case Node(l: Tree, r: Tree)
  }

  enum Labeled {
    @title("red") @description("the red one") case R
    @title("green") case G
  }

  @discriminator enum Event {
    case Click(x: Int)
    @discriminatorKey("key") case Key(code: String)
    @definition("scroll") case Scroll(dy: Int)
  }

  @typeHint[Int]
  enum Level   { case Low, High }
  object Level {
    given ToValue[Level] = ToValue.mk {
      case Low  => Value.num(1)
      case High => Value.num(2)
    }
  }

  enum NoHint   { case A, B }
  object NoHint {
    given ToValue[NoHint] = ToValue.mk(x => Value.num(x.ordinal))
  }

  enum Dir derives json.Schema { case N, S }
  case class Pixel(c: Color, d: Dir, s: Option[Shape])
}

/** Scala 3 adds native `enum`s (in v1 scope). They follow the sealed-family rules: all singleton cases -> a string enum of the case names; class cases -> a `oneOf`, singleton cases merged into one
  * enum.
  */
class Scala3EnumsSpec extends AnyFunSuite {
  import Scala3EnumsSpec.*

  test("an enum of singleton cases is a string enum of the case names, in declaration order") {
    val s = Json.schema[Color]
    s shouldBe `enum`.of("Red", "Green", "Blue")
    // a Set of 3 keeps insertion order, so this checks the declaration order
    s.asInstanceOf[`enum`[?]].values.toList shouldBe List(Value.str("Red"), Value.str("Green"), Value.str("Blue"))
  }

  test("a parameterized enum of singleton cases is a string enum too") {
    Json.schema[Planet] shouldBe `enum`.of("Mercury", "Venus")
  }

  test("an enum with class cases is a oneOf; its singleton cases become one enum") {
    Json.schema[Shape] shouldBe `oneof`.of[Shape](
      `object`(Field("r", `number`[Double])),
      `object`(Field("w", `number`[Double]), Field("h", `number`[Double])),
      `enum`.of("Point")
    )
  }

  test("a generic enum: class cases get the type argument") {
    Json.schema[Opt[Int]] shouldBe `oneof`.of[Opt[Int]](
      `object`(Field("v", `integer`)),
      `enum`.of("Nada")
    )
  }

  test("a recursive enum") {
    val sig = "com.github.andyglow.jsonschema.sums.Scala3EnumsSpec.Tree"
    Json.schema[Tree] shouldBe `def`[Tree](
      sig,
      `oneof`.of[Tree](
        `enum`.of("Leaf"),
        `object`(Field("l", `ref`[Tree](sig)), Field("r", `ref`[Tree](sig)))
      )
    )
  }

  test("enum case annotations: texts on consts with EnumsAsOneOf (a `given` flag), dropped otherwise") {
    Json.schema[Labeled] shouldBe `enum`.of("R", "G")
    locally {
      // a local given is visible in its whole block, hence the nested one
      given (Flag & Flag.EnumsAsOneOf) = null
      Json.schema[Labeled] shouldBe `oneof`.of[Labeled](
        const("R").withTitle("red").withDescription("the red one"),
        const("G").withTitle("green")
      )
    }
  }

  test("@discriminator, @discriminatorKey and @definition on enum class cases") {
    Json.schema[Event] shouldBe `oneof`
      .of[Event](
        `object`(Field("x", `integer`)).withDiscriminationKey("com.github.andyglow.jsonschema.sums.Scala3EnumsSpec.Event.Click"),
        `object`(Field("code", `string`)).withDiscriminationKey("key"),
        `def`[Event]("scroll", `object`(Field("dy", `integer`)).withDiscriminationKey("com.github.andyglow.jsonschema.sums.Scala3EnumsSpec.Event.Scroll"))
      )
      .discriminatedBy("type")
  }

  test("a user ToValue[Enum] drives the values, typed by @typeHint") {
    Json.schema[Level] shouldBe `enum`.of(1, 2)
  }

  test("a user ToValue[Enum] without @typeHint doesn't compile") {
    errorOf(assertCompiles("Json.schema[NoHint]")) should include("Use @typeHint annotation to specify correct type")
  }

  test("enums as fields; a `derives` enum is a definition") {
    Json.schema[Pixel] shouldBe `object`(
      Field("c", `enum`.of("Red", "Green", "Blue")),
      Field("d", `def`[Dir]("com.github.andyglow.jsonschema.sums.Scala3EnumsSpec.Dir", `enum`.of("N", "S"))),
      Field(
        "s",
        `oneof`.of[Shape](
          `object`(Field("r", `number`[Double])),
          `object`(Field("w", `number`[Double]), Field("h", `number`[Double])),
          `enum`.of("Point")
        ),
        required = false
      )
    )
  }
}
