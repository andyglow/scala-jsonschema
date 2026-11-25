package com.github.andyglow.jsonschema.characterization

import com.github.andyglow.jsonschema.harness.{ErrorPhrases, Golden, Scala}
import com.github.andyglow.jsonschema.harness.CompileErrors.{errorOf, outcome}
import json._
import json.Schema._
import json.Schema.`object`.Field
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

// models first (SI-7046)
object SumsEnumsCharSpec {
  sealed trait NoKids

  object Colors extends Enumeration {
    val Red   = Value("custom-red")
    val Green = Value
    val Blue  = Value(10, "custom-blue")
  }
  case class HasColors(c: Colors.Value)

  object WeekDay extends Enumeration {
    type T = Value
    val Mon, Tue, Wed = Value
  }

  object Planet extends Enumeration {
    // `super.Val(nextId, noName)` is what `super.Val()` does (no names given). Scala.js warns on `super.Val()` and on a literal null: the values' runtime names need reflection there.
    // The schema doesn't: the derivation takes the names of the vals at compile time, and the test never prints a value
    private def noName: String = null
    protected case class PlanetVal(mass: Double) extends super.Val(nextId, noName)
    val Mercury = PlanetVal(3.303e+23)
    val Venus   = PlanetVal(4.869e+24)
  }

  case class HasEither(e: Either[Int, String])

  // Scala 2 quirks that Scala 3 doesn't reproduce
  object WithInner extends Enumeration {
    val A, B = Value
    case class Inner(x: Int)
  }
  object WithOtherValue extends Enumeration {
    val A      = Value
    val notOne = com.github.andyglow.json.Value.str("x")
  }

  // subclasses Scala 3 can't list: anonymous ones and those local to a block
  sealed trait AnonKid
  case class AK0(y: Int) extends AnonKid
  object AnonKid { def mk: AnonKid = new AnonKid {} }

  sealed abstract case class Email private (v: String)
  object Email { def mk(v: String): Email = new Email(v) {} }

  sealed trait LocalKid
  case class LK0(y: Int) extends LocalKid
  object LocalKid {
    def mk: LocalKid = {
      case class K(x: Int) extends LocalKid
      K(1)
    }
  }

  sealed case class SelfRoot(a: Int)
}

/** Characterization of the sum and enum families that need no other family (inventory T3, T6, T8, T13): runs on Scala 2 and 3. */
class SumsEnumsCharSpec extends AnyFunSuite {
  import SumsEnumsCharSpec._
  import Golden.check
  import ErrorPhrases._

  test("T3: a Java enum is a string enum of its constant names (bugs-1 #3)") {
    // 0.7: macro crash `ScalaReflectionException: Java enum MINUTES is not a class`
    import java.time.{DayOfWeek, Month}
    val days = DayOfWeek.values.toList.map(d => com.github.andyglow.json.Value.str(d.name))
    Json.schema[DayOfWeek] shouldEqual `enum`.of[DayOfWeek](`string`, days.head, days.tail: _*)
    check("char/types/java.util.concurrent.TimeUnit", Json.schema[java.util.concurrent.TimeUnit])
    check("char/types/java.time.DayOfWeek", Json.schema[DayOfWeek])
    check("char/types/java.time.Month", Json.schema[Month])
  }

  test("T6: a sealed trait without subclasses doesn't compile") {
    errorOf(assertCompiles("Json.schema[NoKids]")) should include(NoItems)
  }

  test("T6: a sealed case class without subclasses is an enum without items") {
    errorOf(assertCompiles("Json.schema[SelfRoot]")) should include(NoItems)
  }

  test("T8: scala.Enumeration uses val names, not the Value(name) strings") {
    val s = Json.schema[Colors.Value]
    check("char/types/Colors.Value", s)
    s shouldBe `enum`.of("Red", "Green", "Blue")
    check("char/types/HasColors", Json.schema[HasColors])
  }

  test("T8: scala.Enumeration as X.type, X.Value, an alias of X.Value, with a custom Val") {
    val weekDays = `enum`.of("Mon", "Tue", "Wed")
    Json.schema[WeekDay.type] shouldBe weekDays
    Json.schema[WeekDay.Value] shouldBe weekDays
    Json.schema[WeekDay.T] shouldBe weekDays
    Json.schema[Planet.Value] shouldBe `enum`.of("Mercury", "Venus")
  }

  test("T8: Scala 2 quirks not reproduced on Scala 3: a class inside an Enumeration, vals of another `Value` type") {
    // Scala 2 takes any type whose prefix is an Enumeration object for that enum, and any val whose type has a base class named `Value` for an enum value
    if (Scala.is2) {
      Json.schema[WithInner.Inner] shouldBe `enum`.of("A", "B")
      Json.schema[WithOtherValue.Value] shouldBe `enum`.of("A", "notOne")
    } else {
      Json.schema[WithInner.Inner] shouldBe `object`(Field("x", `integer`))
      Json.schema[WithOtherValue.Value] shouldBe `enum`.of("A")
    }
  }

  test("T13: Either (decided: keep)") {
    check("groupC/Either[Int,String]", Json.schema[Either[Int, String]])
    check("groupC/HasEither", Json.schema[HasEither])
  }

  test("T5: an anonymous subclass of a sealed family doesn't compile") {
    // the Email case is the `sealed abstract case class X private (..)` + `new X(v) {}` smart-constructor idiom
    for ((err, name) <- List(errorOf(assertCompiles("Json.schema[AnonKid]")) -> "AnonKid", errorOf(assertCompiles("Json.schema[Email]")) -> "Email")) {
      // the same text on both versions (B11, #471)
      err should include(name)
      err should include(NotSupported)
      err should include("local to a block or anonymous")
    }
  }

  test("T5: a case class local to a block as a sealed subclass: derived on Scala 2, a clear error on Scala 3") {
    // Scala 3 lists such a subclass as the sealed trait itself, so it can't be recovered
    val r = outcome(assertCompiles("Json.schema[LocalKid]"))
    if (Scala.is2) r shouldBe None
    else {
      r.getOrElse("") should include(NotSupported)
      r.getOrElse("") should include("LocalKid")
      r.getOrElse("") should include("local to a block or anonymous")
    }
  }
}
