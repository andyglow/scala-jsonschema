package com.github.andyglow.jsonschema

import java.util.UUID

import json._
import json.Schema.`dictionary`.KeyPattern
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

import scala.scalajs.js

/** The Map key patterns (#466) under ECMA-262, the regex dialect of JSON Schema, with the JavaScript engine's own `RegExp` (no flags, as a validator compiles a pattern). The JVM specs check them with
  * networknt, which uses the JDK's regexes; on Scala.js those validating tests are canceled, so this is where ECMA-262 is checked.
  */
class KeyPatternRegExpSpec extends AnyFunSuite {
  import MapKeySchemaSpec._

  private def matches(pattern: String, key: String): Boolean = new js.RegExp(pattern).test(key)

  private def check(pattern: String, valid: List[String], invalid: List[String]): Unit = {
    valid foreach { k => withClue(s"$pattern should match ${js.JSON.stringify(k)}: ")(matches(pattern, k) shouldBe true) }
    invalid foreach { k => withClue(s"$pattern should not match ${js.JSON.stringify(k)}: ")(matches(pattern, k) shouldBe false) }
  }

  test("integral keys") {
    check(implicitly[KeyPattern[Int]].pattern, List("0", "42", "-1", "-2147483648", "01"), List("", "-", "abc", "1.5", "+1", " 1"))
    implicitly[KeyPattern[Long]].pattern shouldBe implicitly[KeyPattern[Int]].pattern
  }

  test("Char keys: one character, a line terminator included") {
    check(implicitly[KeyPattern[Char]].pattern, List("a", "\n", "\r", 0x2028.toChar.toString, " ", "."), List("", "ab", "\n\n"))
  }

  test("String keys: any key, a line break included") {
    check(implicitly[KeyPattern[String]].pattern, List("", "a", "a\nb", "\r\n"), Nil)
  }

  test("UUID keys, either case") {
    val u = "123e4567-e89b-12d3-a456-426614174000"
    check(implicitly[KeyPattern[UUID]].pattern, List(u, u.toUpperCase), List("", "abc", u.replace('-', 'x'), u + "0", u.drop(1)))
  }

  test("Boolean keys") {
    check(implicitly[KeyPattern[Boolean]].pattern, List("true", "false"), List("True", "yes", "1", "truefalse", ""))
  }

  test("escaped enum key names match themselves only") {
    val pattern = Json.schema[Map[Size, Int]].validations.head.json.asInstanceOf[com.github.andyglow.json.Value.str].value
    check(pattern, List("N/A", "1+", "(none)", "a.b"), List("1", "11", "axb", "none", "N/A1"))
    check(KeyPattern.forEnum[Size](List("x*", "[y]", "{z}", "a|b", "^$", "\\")).pattern, List("x*", "[y]", "{z}", "a|b", "^$", "\\"), List("x", "xx", "y", "a", "b", ""))
  }
}
