package com.github.andyglow.json

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets.UTF_8

import com.github.andyglow.testsupport._
import org.scalatest.matchers.should.Matchers._
import org.scalatest.funsuite.AnyFunSuite

class ParseJsonSpec extends AnyFunSuite {
  import Value._

  test("null") { ParseJson("null").value shouldBe `null` }
  test("positive integer") { ParseJson("5").value shouldBe num(5) }
  test("negative integer") { ParseJson("-5").value shouldBe num(-5) }
  test("zero") { ParseJson("0").value shouldBe num(0) }
  test("positive decimal") { ParseJson("0.5").value shouldBe num(.5) }
  test("negative decimal") { ParseJson("-0.5").value shouldBe num(-.5) }
  test("exponent") { ParseJson("1e2").value shouldBe num(100) }
  test("capital exponent") { ParseJson("1E2").value shouldBe num(100) }
  test("negative exponent") { ParseJson("25E-2").value shouldBe num(.25) }
  test("decimal with exponent") { ParseJson("1.5e+3").value shouldBe num(1500) }
  test("long") { ParseJson("9223372036854775807").value shouldBe num(Long.MaxValue) }
  test("big integer") {
    ParseJson("123456789012345678901234567890").value shouldBe num(BigInt("123456789012345678901234567890"))
  }
  test("decimal keeps precision") {
    ParseJson("0.12345678901234567890123456789012345678901").value shouldBe num(
      BigDecimal(new java.math.BigDecimal("0.12345678901234567890123456789012345678901"))
    )
  }
  test("string") { ParseJson("\"foo bar\"").value shouldBe str("foo bar") }
  test("string: simple escapes") {
    ParseJson(""""\" \\ \/ \b \f \n \r \t"""").value shouldBe str("\" \\ / \b \f \n \r \t")
  }
  // json unicode escapes are written with a doubled backslash so that scalac doesn't decode them itself
  test("string: unicode escape") { ParseJson("\"\\u0041\\u00e9\\u00E9\"").value shouldBe str("Aéé") }
  test("string: surrogate pair escape") { ParseJson("\"\\uD83D\\uDE00\"").value shouldBe str("😀") }
  test("string: non-ascii") { ParseJson("\"привіт 😀\"").value shouldBe str("привіт 😀") }
  test("boolean true") { ParseJson("true").value shouldBe `true` }
  test("boolean false") { ParseJson("false").value shouldBe `false` }
  test("surrounding whitespace") { ParseJson(" \t\r\n[ 1 ,\n2 ]\n ").value shouldBe arr(num(1), num(2)) }
  test("empty array") { ParseJson("[]").value shouldBe arr() }
  test("non empty array. integers") {
    ParseJson("[1,2,3]").value shouldBe arr(num(1), num(2), num(3))
  }
  test("non empty array. decimals") { ParseJson("[0.1,0.2]").value shouldBe arr(num(.1), num(.2)) }
  test("non empty array. strings") {
    ParseJson("[\"foo\", \"bar\"]").value shouldBe arr(str("foo"), str("bar"))
  }
  test("non empty array. booleans") {
    ParseJson("[true, false]").value shouldBe arr(`true`, `false`)
  }
  test("non empty array. nulls") { ParseJson("[null, null]").value shouldBe arr(`null`, `null`) }
  test("non empty array. mixed") {
    ParseJson("[1, 0.2, \"foo\", true, null]").value shouldBe arr(
      num(1),
      num(.2),
      str("foo"),
      `true`,
      `null`
    )
  }
  test("nested arrays") { ParseJson("[[], [[1]]]").value shouldBe arr(arr(), arr(arr(num(1)))) }
  test("empty object") { ParseJson("{}").value shouldBe obj() }
  test("non empty object. integers") {
    ParseJson("{\"a\": 1, \"b\": 2, \"c\": 3}").value shouldBe obj("a" -> 1, "b" -> 2, "c" -> 3)
  }
  test("non empty object. decimals") {
    ParseJson("{\"a\": 0.1, \"b\": 0.2}").value shouldBe obj("a" -> .1, "b" -> .2)
  }
  test("non empty object. strings") {
    ParseJson("{\"a\": \"foo\", \"b\": \"bar\"}").value shouldBe obj("a" -> "foo", "b" -> "bar")
  }
  test("non empty object. booleans") {
    ParseJson("{\"a\": true, \"b\": false}").value shouldBe obj("a" -> true, "b" -> false)
  }
  test("non empty object. nulls") {
    ParseJson("{\"a\": null, \"b\": null}").value shouldBe obj("a" -> `null`, "b" -> `null`)
  }
  test("non empty object. mixed") {
    ParseJson("{\"a\": 1, \"b\": 0.2, \"c\": \"foo\", \"d\": true, \"e\": null}").value shouldBe obj(
      "a" -> 1,
      "b" -> .2,
      "c" -> "foo",
      "d" -> true,
      "e" -> `null`
    )
  }
  test("object: escaped key") { ParseJson("""{"a\nb": 1}""").value shouldBe obj("a\nb" -> 1) }
  test("object: field order is preserved") {
    val keys = (1 to 20).map(i => s"k$i")
    ParseJson(keys.map(k => s""""$k": 0""").mkString("{", ",", "}")).value match {
      case o: obj => o.fields.map(_._1) shouldBe keys
      case other  => fail(s"expected object, got $other")
    }
  }
  test("object: last duplicated key wins") { ParseJson("""{"a": 1, "a": 2}""").value shouldBe obj("a" -> 2) }

  test("bytes: utf-8") { ParseJson("\"é\"".getBytes(UTF_8)).value shouldBe str("é") }
  test("bytes: byte order mark is ignored") {
    ParseJson(Array(0xef, 0xbb, 0xbf).map(_.toByte) ++ "[]".getBytes(UTF_8)).value shouldBe arr()
  }
  test("input stream") { ParseJson(new ByteArrayInputStream("""{"a": [1]}""".getBytes(UTF_8))).value shouldBe obj("a" -> arr(num(1))) }

  // rejected input
  Seq(
    ""                                                              -> "empty input",
    " "                                                             -> "whitespace only",
    ".5"                                                            -> "number without integer part",
    "-.5"                                                           -> "negative number without integer part",
    "1."                                                            -> "number without fraction digits",
    "01"                                                            -> "leading zero",
    "-01"                                                           -> "negative leading zero",
    "1e"                                                            -> "exponent without digits",
    "+1"                                                            -> "leading plus",
    "1_000"                                                         -> "digit separator",
    "NaN"                                                           -> "NaN",
    "[1,]"                                                          -> "trailing comma in array",
    "[1,,2]"                                                        -> "double comma in array",
    "[,1]"                                                          -> "leading comma in array",
    "[1 2]"                                                         -> "missing comma in array",
    "[1"                                                            -> "unclosed array",
    """{"a": 1,}"""                                                 -> "trailing comma in object",
    """{"a" 1}"""                                                   -> "missing colon",
    """{: 1}"""                                                     -> "missing key",
    """{a: 1}"""                                                    -> "unquoted key",
    """{"a": 1"""                                                   -> "unclosed object",
    "[1]]"                                                          -> "trailing garbage",
    "[][]"                                                          -> "two values",
    "\"abc"                                                         -> "unterminated string",
    "\"a\tb\""                                                      -> "unescaped control character",
    """"\x41""""                                                    -> "invalid escape",
    "\"\\u12G4\""                                                   -> "invalid unicode escape",
    "\"\\u12\""                                                     -> "incomplete unicode escape",
    "'a'"                                                           -> "single quotes",
    "tru"                                                           -> "truncated literal",
    "True"                                                          -> "capitalized literal",
    "\f[]"                                                          -> "form feed is not whitespace",
    "[" * (ParseJson.MaxDepth + 1) + "]" * (ParseJson.MaxDepth + 1) -> "too deep"
  ) foreach { case (json, label) =>
    test(s"reject: $label") {
      ParseJson(json).isFailure shouldBe true
    }
  }

  test("reject: invalid utf-8 bytes") {
    ParseJson(Array('"'.toByte, 0xff.toByte, '"'.toByte)).isFailure shouldBe true
  }

  test("nesting up to the limit is fine") {
    ParseJson("[" * ParseJson.MaxDepth + "]" * ParseJson.MaxDepth).isSuccess shouldBe true
  }

  test("error reports offset") {
    ParseJson("[1, 2,]").failed.get match {
      case err: JsonParseException => err.offset shouldBe 6
      case other                   => fail(s"unexpected error $other")
    }
  }
}
