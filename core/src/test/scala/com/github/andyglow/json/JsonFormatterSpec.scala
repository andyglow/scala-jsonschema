package com.github.andyglow.json

import com.github.andyglow.json.Value._
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.matchers.should.Matchers._
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

  property("JsonFormatter should format arrays with ordering forced. strings") {
    format(arr("bbb", "ccc", "aaa"), sorted = true) shouldEqual
      s"""[
         |  "aaa",
         |  "bbb",
         |  "ccc"
         |]""".stripMargin
  }

  property("JsonFormatter should format arrays with ordering forced. nums") {
    format(arr(8, 12, 1, 6), sorted = true) shouldEqual
      s"""[
         |  1,
         |  6,
         |  8,
         |  12
         |]""".stripMargin
  }

  property("JsonFormatter should format arrays with ordering forced. bools") {
    format(arr(true, false), sorted = true) shouldEqual
      s"""[
         |  false,
         |  true
         |]""".stripMargin
  }

  property("JsonFormatter should format arrays with ordering forced. arrays") {
    format(arr(arr(2, 1), arr(3, 1, 7)), sorted = true) shouldEqual
      s"""[
         |  [
         |    1,
         |    2
         |  ],
         |  [
         |    1,
         |    3,
         |    7
         |  ]
         |]""".stripMargin
  }

  property("JsonFormatter should format arrays with ordering forced. objs") {
    format(arr(obj("a" -> 1), obj("c" -> 3), obj("b" -> 2)), sorted = true) shouldEqual
      s"""[
         |  {
         |    "a": 1
         |  },
         |  {
         |    "b": 2
         |  },
         |  {
         |    "c": 3
         |  }
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

  property("JsonFormatter should format objects with ordering forced") {
    format(obj(
      "a" -> "a",
      "c" -> "c",
      "b" -> "b"), sorted = true) shouldEqual
      s"""{
         |  "a": "a",
         |  "b": "b",
         |  "c": "c"
         |}""".stripMargin
  }

  property("JsonFormatter should escape keys and string values") {
    format(obj(
      """"quoted-key"""" -> "\n\t'val\"")) shouldEqual
      s"""{
         |  "\\"quoted-key\\"": "\\n\\t'val\\""
         |}""".stripMargin
  }

}
