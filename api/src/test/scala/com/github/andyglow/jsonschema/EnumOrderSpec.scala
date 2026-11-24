package com.github.andyglow.jsonschema

import com.github.andyglow.json.{ToValue, Value}
import com.github.andyglow.json.Value._
import json._
import json.schema.{typeHint, JsonSchemaProfile, Version}
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec._

// Enum values keep declaration order (issue: B7). Two to four values come out in order by luck (Scala's Set1..Set4 keep insertion order), so every enum here has at least six.
object EnumOrderSpec {

  sealed trait Weekday
  object Weekday {
    case object Mon extends Weekday
    case object Tue extends Weekday
    case object Wed extends Weekday
    case object Thu extends Weekday
    case object Fri extends Weekday
    case object Sat extends Weekday
    case object Sun extends Weekday
  }

  @typeHint[Int]
  sealed trait Level
  object Level {
    case object L1 extends Level
    case object L2 extends Level
    case object L3 extends Level
    case object L4 extends Level
    case object L5 extends Level
    case object L6 extends Level

    implicit val toValue: ToValue[Level] = ToValue mk {
      case L1 => 10
      case L2 => 20
      case L3 => 30
      case L4 => 40
      case L5 => 50
      case L6 => 60
    }
  }

  // a family of classes and objects: the objects merge into one enum inside the oneOf
  sealed trait Cmd
  object Cmd {
    final case class Run(arg: String) extends Cmd
    case object Start                 extends Cmd
    case object Stop                  extends Cmd
    case object Pause                 extends Cmd
    case object Resume                extends Cmd
    case object Reset                 extends Cmd
    case object Kill                  extends Cmd
  }

  object Planet extends Enumeration {
    val Mercury, Venus, Earth, Mars, Jupiter, Saturn, Uranus, Neptune = Value
  }

  val weekdays: List[Value] = List("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun").map(str(_))
}

class EnumOrderSpec extends AnyWordSpec {
  import EnumOrderSpec._

  private def rendered(s: Schema[_])(implicit profile: JsonSchemaProfile): Value = AsValue.schema(s, Version.Draft07(id = "x"))

  /** The values of every `enum` keyword in the rendered JSON, in document order. */
  private def enums(v: Value): List[List[Value]] = v match {
    case o: obj =>
      o.fields.toList.flatMap {
        case ("enum", arr(vs)) => List(vs.toList)
        case (_, x)            => enums(x)
      }
    case arr(xs) => xs.toList.flatMap(enums)
    case _       => Nil
  }

  private def enumOf(s: Schema[_])(implicit profile: JsonSchemaProfile): List[Value] = enums(rendered(s)) match {
    case List(vs) => vs
    case other    => fail(s"expected one enum, got $other in ${rendered(s)}")
  }

  "enum values" should {

    "keep declaration order for a sealed family of case objects" in {
      enumOf(Json.schema[Weekday]) shouldBe weekdays
    }

    "keep declaration order under the Legacy profile" in {
      enumOf(Json.schema[Weekday])(JsonSchemaProfile.Legacy) shouldBe weekdays
    }

    "keep declaration order for ToValue-driven values" in {
      enumOf(Json.schema[Level]) shouldBe List(10, 20, 30, 40, 50, 60).map(num(_))
    }

    "keep declaration order for the singletons of a mixed family" in {
      enumOf(Json.schema[Cmd]) shouldBe List("Start", "Stop", "Pause", "Resume", "Reset", "Kill").map(str(_))
    }

    "keep declaration order for scala.Enumeration" in {
      enumOf(Json.schema[Planet.Value]) shouldBe List("Mercury", "Venus", "Earth", "Mars", "Jupiter", "Saturn", "Uranus", "Neptune").map(str(_))
    }

    "keep declaration order for a Java enum" in {
      enumOf(Json.schema[java.time.DayOfWeek]) shouldBe java.time.DayOfWeek.values().toList.map(d => str(d.name))
      enumOf(Json.schema[java.util.concurrent.TimeUnit]) shouldBe java.util.concurrent.TimeUnit.values().toList.map(u => str(u.name))
    }

    "keep the order given to Schema.enum.of" in {
      enumOf(Schema.`enum`.of(Schema.`string`[String], weekdays.head, weekdays.tail: _*)) shouldBe weekdays
      enumOf(Schema.`enum`.of("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) shouldBe weekdays
    }

    "keep the derivation's order for a family compiled in another module (Scala 2: by name)" in {
      enumOf(Json.schema[testmodels.LibraryColor]) shouldBe EnumOrderExpected.libraryColor.map(str(_))
    }

    "match the order of the same enum as a Map key" in {
      val keyPattern = rendered(Json.schema[Map[Weekday, Int]]) match {
        case o: obj => o.value("patternProperties").asInstanceOf[obj].fields.head._1
        case other  => fail(other.toString)
      }
      keyPattern shouldBe "^(?:Mon|Tue|Wed|Thu|Fri|Sat|Sun)$"
    }
  }
}
