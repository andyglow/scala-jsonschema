package com.github.andyglow.jsonschema

import com.github.andyglow.json.{ToValue, Value}
import com.github.andyglow.json.Value._
import com.github.andyglow.jsonschema.harness.{Golden, Scala, TriageA}
import com.github.andyglow.jsonschema.harness.CompileErrors.outcome
import json._
import json.Schema._
import json.schema.validation.Instance._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

object BoxedBigMathSpec {
  case class User(name: String)

  case class Boxed(
    i: java.lang.Integer,
    l: java.lang.Long,
    d: java.lang.Double,
    f: java.lang.Float,
    s: java.lang.Short,
    b: java.lang.Byte,
    z: java.lang.Boolean,
    oi: Option[java.lang.Integer]
  )
  case class BigMath(d: java.math.BigDecimal, i: java.math.BigInteger, ds: List[java.math.BigDecimal], od: Option[java.math.BigDecimal])

  // defaults: the Java types next to the Scala ones, same values
  case class JDefaults(
    i: java.lang.Integer = 5,
    l: java.lang.Long = 5L,
    d: java.lang.Double = 1.5,
    f: java.lang.Float = 0.1f,
    s: java.lang.Short = 3.toShort,
    z: java.lang.Boolean = true,
    bd: java.math.BigDecimal = new java.math.BigDecimal("12.50"),
    bi: java.math.BigInteger = java.math.BigInteger.TEN
  )
  case class SDefaults(
    i: Int = 5,
    l: Long = 5L,
    d: Double = 1.5,
    f: Float = 0.1f,
    s: Short = 3.toShort,
    z: Boolean = true,
    bd: BigDecimal = BigDecimal("12.50"),
    bi: BigInt = BigInt(10)
  )

  // values a Double can't hold
  case class JExtremes(
    l: java.lang.Long = Long.MaxValue,
    bd: java.math.BigDecimal = new java.math.BigDecimal("12345678901234567890.123"),
    bi: java.math.BigInteger = new java.math.BigInteger("123456789012345678901234567890")
  )
  case class SExtremes(
    l: Long = Long.MaxValue,
    bd: BigDecimal = BigDecimal("12345678901234567890.123"),
    bi: BigInt = BigInt("123456789012345678901234567890")
  )

  // not in J/SDefaults: core has no ToValue[Byte], so a Scala `Byte` default doesn't compile on Scala 2 (reported to the lead)
  case class JByteDefault(b: java.lang.Byte = 2.toByte)

  case class NullDefault(i: java.lang.Integer = null)
  case class NoneDefault(o: Option[java.lang.Integer] = None)
  case class SomeDefault(o: Option[java.lang.Integer] = Some(5))
  object OptionToValue {
    implicit val optIntegerTV: ToValue[Option[java.lang.Integer]] = ToValue.mk(_.fold[Value](Value.`null`)(i => num(i.intValue)))
  }

  def defaults(s: Schema[_]): List[(String, Option[Value])] = s match {
    case o: `object`[_] => o.fields.toList.map(f => f.name -> f.default)
    case other          => sys.error(s"not an object: $other")
  }
}

/** E4 (#473): boxed `java.lang` numbers/Boolean and `java.math.BigDecimal`/`BigInteger` render exactly as their Scala counterparts. The root equalities are also the guard for D2 (integer types): D2
  * changes the Scala Predefs, and the Java ones must follow.
  */
class BoxedBigMathSpec extends AnyFunSuite {
  import BoxedBigMathSpec._

  test("each Java type at the root is its Scala counterpart's schema") {
    Json.schema[java.lang.Integer] shouldEqual Json.schema[Int]
    Json.schema[java.lang.Long] shouldEqual Json.schema[Long]
    Json.schema[java.lang.Double] shouldEqual Json.schema[Double]
    Json.schema[java.lang.Float] shouldEqual Json.schema[Float]
    Json.schema[java.lang.Short] shouldEqual Json.schema[Short]
    Json.schema[java.lang.Byte] shouldEqual Json.schema[Byte]
    Json.schema[java.lang.Boolean] shouldEqual Json.schema[Boolean]
    Json.schema[java.math.BigDecimal] shouldEqual Json.schema[BigDecimal]
    Json.schema[java.math.BigInteger] shouldEqual Json.schema[BigInt]
  }

  test("today's nodes (number vs integer as for the Scala types)") {
    Json.schema[java.lang.Integer].jsonType shouldEqual "integer"
    Json.schema[java.lang.Boolean].jsonType shouldEqual "boolean"
    Seq(
      Json.schema[java.lang.Long],
      Json.schema[java.lang.Double],
      Json.schema[java.lang.Float],
      Json.schema[java.lang.Short],
      Json.schema[java.lang.Byte],
      Json.schema[java.math.BigDecimal],
      Json.schema[java.math.BigInteger]
    ).map(_.jsonType).distinct shouldEqual Seq("number")
  }

  test("as fields (golden)") {
    Golden.check("shared/boxed/Boxed", Json.schema[Boxed])
    Golden.check("shared/boxed/BigMath", Json.schema[BigMath])
  }

  test("defaults render as the Scala types' defaults") {
    defaults(Json.schema[JDefaults]) shouldEqual defaults(Json.schema[SDefaults])
    Json.schema[JDefaults] shouldEqual Json.schema[SDefaults]
    defaults(Json.schema[JByteDefault]) shouldEqual List("b" -> Some(num(2.toByte)))
  }

  test("defaults are exact at values a Double can't hold") {
    val expected = List(
      "l"  -> Some(num(Long.MaxValue)),
      "bd" -> Some(num(BigDecimal("12345678901234567890.123"))),
      "bi" -> Some(num(BigInt("123456789012345678901234567890")))
    )
    defaults(Json.schema[SExtremes]) shouldEqual expected
    defaults(Json.schema[JExtremes]) shouldEqual expected
    AsValue.schema(Json.schema[JExtremes], json.schema.Version.Draft07("x")) shouldEqual AsValue.schema(Json.schema[SExtremes], json.schema.Version.Draft07("x"))
  }

  test("a null default is JSON null") {
    defaults(Json.schema[NullDefault]) shouldEqual List("i" -> Some(Value.`null`))
  }

  test("Option of a boxed type: `= None` emits no default") {
    defaults(Json.schema[NoneDefault]) shouldEqual List("o" -> None)
  }

  test("Option of a boxed type: `= Some(5)` compiles on Scala 3, crashes the Scala 2 re-parse (as for Option[String])", TriageA) {
    val res = outcome(assertCompiles("{ import BoxedBigMathSpec.OptionToValue._; Json.schema[BoxedBigMathSpec.SomeDefault] }"))
    if (Scala.is3) res shouldBe None
    else res.getOrElse("") should include("TypecheckException")
  }

  test("Map keys: same pattern as the Scala integral, through Predef.dictS (Predef value)") {
    Json.schema[Map[java.lang.Integer, String]] shouldEqual Json.schema[Map[Int, String]]
    Json.schema[Map[java.lang.Long, String]] shouldEqual Json.schema[Map[Long, String]]
    Json.schema[Map[java.lang.Short, String]] shouldEqual Json.schema[Map[Short, String]]
    Json.schema[Map[java.lang.Byte, String]] shouldEqual Json.schema[Map[Byte, String]]
  }

  test("Map keys: same pattern as the Scala integral, through the macro's dictionary (derived value)") {
    Json.schema[Map[java.lang.Integer, User]] shouldEqual Json.schema[Map[Int, User]]
    Json.schema[Map[java.lang.Long, User]] shouldEqual Json.schema[Map[Long, User]]
    Json.schema[Map[java.lang.Short, User]] shouldEqual Json.schema[Map[Short, User]]
    Json.schema[Map[java.lang.Byte, User]] shouldEqual Json.schema[Map[Byte, User]]
  }

  test("numeric validations") {
    val vs = Seq(`minimum` := 1, `maximum` := 10)
    Json.schema[java.lang.Integer].withValidation(`minimum` := 1, `maximum` := 10).validations shouldEqual vs
    Json.schema[java.lang.Long].withValidation(`minimum` := 1, `maximum` := 10).validations shouldEqual vs
    Json.schema[java.lang.Short].withValidation(`minimum` := 1, `maximum` := 10).validations shouldEqual vs
    Json.schema[java.lang.Byte].withValidation(`minimum` := 1, `maximum` := 10).validations shouldEqual vs
    Json.schema[java.lang.Float].withValidation(`minimum` := 1, `maximum` := 10).validations shouldEqual vs
    Json.schema[java.lang.Double].withValidation(`minimum` := 1, `maximum` := 10).validations shouldEqual vs
    Json.schema[java.math.BigDecimal].withValidation(`minimum` := 1, `maximum` := 10).validations shouldEqual vs
    Json.schema[java.math.BigInteger].withValidation(`minimum` := 1, `maximum` := 10).validations shouldEqual vs
  }
}
