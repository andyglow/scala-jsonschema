package com.github.andyglow.json

import com.github.andyglow.json.Value._
import org.scalatest._
import matchers.should.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.matchers
import org.scalatest.propspec.AnyPropSpec

class JsonFormatterSpec extends AnyPropSpec {
  import JsonFormatter._

  private val primitiveExamples = Table(
    ("JS Input"           , "Expected"),
    (str("foo")           , """"foo""""),
    (num(5)               , """5"""),
    (num(5L)              , """5"""),
    (num(5.1)             , """5.1"""),
    (num(BigDecimal(5.1)) , """5.1"""),
    (num(BigDecimal(5))   , """5"""),
    (`null`               , """null"""),
    (`true`               , """true"""),
    (`false`              , """false"""),
    (bool(true)           , """true"""),
    (bool(false)          , """false""")
  )

  property("JsonFormatter should format primitive types") {
    forAll(primitiveExamples) {
      (js, expected) => format(js) shouldEqual expected
    }
  }

  property("JsonFormatter should format arrays") {
    format(arr("foo", "bar")) shouldEqual
      s"""[
         |  "foo",
         |  "bar"
         |]""".stripMargin
  }

  property("JsonFormatter should format objects") {
    format(obj(
      "x" -> "x",
      "y" -> "y")) shouldEqual
      s"""{
         |  "x": "x",
         |  "y": "y"
         |}""".stripMargin
  }

}
