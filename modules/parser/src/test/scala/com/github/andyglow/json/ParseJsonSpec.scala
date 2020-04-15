package com.github.andyglow.json

import org.scalatest.Matchers._
import org.scalatest._
import com.github.andyglow.testsupport._


class ParseJsonSpec extends FunSuite {
  import Value._

  test("null") { ParseJson("null").value shouldBe `null` }
  test("positive integer") { ParseJson("5").value shouldBe num(5) }
  test("negative integer") { ParseJson("-5").value shouldBe num(-5) }
  test("positive decimal") { ParseJson(".5").value shouldBe num(.5) }
  test("negative decimal") { ParseJson("-.5").value shouldBe num(-.5) }
  test("string") { ParseJson("\"foo bar\"").value shouldBe str("foo bar") }
  test("boolean true") { ParseJson("true").value shouldBe `true` }
  test("boolean false") { ParseJson("false").value shouldBe `false` }
  test("empty array") { ParseJson("[]").value shouldBe arr() }
  test("non empty array. integers") { ParseJson("[1,2,3]").value shouldBe arr(num(1), num(2), num(3)) }
  test("non empty array. decimals") { ParseJson("[.1,.2]").value shouldBe arr(num(.1), num(.2)) }
  test("non empty array. strings") { ParseJson("[\"foo\", \"bar\"]").value shouldBe arr(str("foo"), str("bar")) }
  test("non empty array. booleans") { ParseJson("[true, false]").value shouldBe arr(`true`, `false`) }
  test("non empty array. nulls") { ParseJson("[null, null]").value shouldBe arr(`null`, `null`) }
  test("non empty array. mixed") { ParseJson("[1, .2, \"foo\", true, null]").value shouldBe arr(num(1), num(.2), str("foo"), `true`, `null`) }
  test("empty object") { ParseJson("{}").value shouldBe obj() }
  test("non empty object. integers") { ParseJson("{\"a\": 1, \"b\": 2, \"c\": 3}").value shouldBe obj("a" -> 1, "b" -> 2, "c" -> 3) }
  test("non empty object. decimals") { ParseJson("{\"a\": .1, \"b\": .2}").value shouldBe obj("a" -> .1, "b" -> .2) }
  test("non empty object. strings") { ParseJson("{\"a\": \"foo\", \"b\": \"bar\"}").value shouldBe obj("a" -> "foo", "b" -> "bar") }
  test("non empty object. booleans") { ParseJson("{\"a\": true, \"b\": false}").value shouldBe obj("a" -> true, "b" -> false) }
  test("non empty object. nulls") { ParseJson("{\"a\": null, \"b\": null}").value shouldBe obj("a" -> `null`, "b" -> `null`) }
  test("non empty object. mixed") { ParseJson("{\"a\": 1, \"b\": .2, \"c\": \"foo\", \"d\": true, \"e\": null}").value shouldBe obj("a" -> 1, "b" -> .2, "c" -> "foo", "d" -> true, "e" -> `null`) }
}