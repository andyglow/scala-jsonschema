package com.github.andyglow.jsonschema

import java.io.File
import java.net.{Inet4Address, Inet6Address, InetAddress}
import java.nio.charset.Charset
import java.nio.file.Path
import java.util.{Currency, Locale}
import java.util.regex.Pattern

import com.github.andyglow.json.Value._
import json._
import json.Schema._
import json.Schema.`string`.Format
import json.schema.{JsonSchemaProfile, Version}
import json.schema.validation.Instance._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._
import petshop.Acceptance

import scala.util.matching.Regex

/** The string-like Predefs (design doc `docs/design/475-string-like.md`, #475): `Locale`, `Currency`, `java.util.regex.Pattern`, `scala.util.matching.Regex`, `InetAddress`, `Inet4Address`,
  * `Inet6Address`, `java.nio.file.Path`, `java.io.File`, `java.nio.charset.Charset`. Each is a core Predef, so it is inlined (never a definition), also as a field. The tree and rendering tests run on
  * both platforms (the Predefs only name the types, so they link on Scala.js although most of these classes aren't in its javalib); the validating ones use the acceptance harness' JSON Schema
  * validator (JVM; canceled on Scala.js) on the text the JDK and the codecs write (checked in `StringLikeSamplesSpec`, JVM only).
  */
class StringLikeSchemaSpec extends AnyFunSuite {
  import StringLikeSchemaSpec._
  import ProfileSpec.{body, drafts, prop, render}

  private val profiles = List(JsonSchemaProfile.Neutral, JsonSchemaProfile.Legacy)

  test("the trees: a string, with a format or a pattern where one fits") {
    Json.schema[Locale] shouldBe `string`[Locale]
    Json.schema[Currency] shouldBe `string`[Currency].withValidation(`pattern` := CurrencyPattern)
    Json.schema[Pattern] shouldBe `string`[Pattern](Format.`regex`)
    Json.schema[Regex] shouldBe `string`[Regex](Format.`regex`)
    Json.schema[InetAddress] shouldBe `string`[InetAddress]
    Json.schema[Inet4Address] shouldBe `string`[Inet4Address](Format.`ipv4`)
    Json.schema[Inet6Address] shouldBe `string`[Inet6Address](Format.`ipv6`)
    Json.schema[Path] shouldBe `string`[Path]
    Json.schema[File] shouldBe `string`[File]
    Json.schema[Charset] shouldBe `string`[Charset].withValidation(`pattern` := CharsetPattern)
  }

  test("as fields: inlined, never a definition") {
    Json.schema[StringLikes] shouldBe `object`(
      `object`.Field("locale", `string`[Locale]),
      `object`.Field("currency", `string`[Currency].withValidation(`pattern` := CurrencyPattern)),
      `object`.Field("pattern", `string`[Pattern](Format.`regex`)),
      `object`.Field("regex", `string`[Regex](Format.`regex`)),
      `object`.Field("ip", `string`[InetAddress]),
      `object`.Field("ip4", `string`[Inet4Address](Format.`ipv4`)),
      `object`.Field("ip6", `string`[Inet6Address](Format.`ipv6`)),
      `object`.Field("path", `string`[Path]),
      `object`.Field("file", `string`[File]),
      `object`.Field("charset", `string`[Charset].withValidation(`pattern` := CharsetPattern))
    )
  }

  for (p <- profiles) {
    implicit val profile: JsonSchemaProfile = p

    test(s"${p.name}: rendered the same on every draft") {
      drafts foreach { v =>
        body(render(Json.schema[Locale], v)) shouldBe obj("type" -> "string")
        body(render(Json.schema[Currency], v)) shouldBe obj("type" -> "string", "pattern" -> CurrencyPattern)
        body(render(Json.schema[Pattern], v)) shouldBe obj("type" -> "string", "format" -> "regex")
        body(render(Json.schema[Regex], v)) shouldBe obj("type" -> "string", "format" -> "regex")
        body(render(Json.schema[InetAddress], v)) shouldBe obj("type" -> "string")
        body(render(Json.schema[Inet4Address], v)) shouldBe obj("type" -> "string", "format" -> "ipv4")
        body(render(Json.schema[Inet6Address], v)) shouldBe obj("type" -> "string", "format" -> "ipv6")
        body(render(Json.schema[Path], v)) shouldBe obj("type" -> "string")
        body(render(Json.schema[File], v)) shouldBe obj("type" -> "string")
        body(render(Json.schema[Charset], v)) shouldBe obj("type" -> "string", "pattern" -> CharsetPattern)
        prop(render(Json.schema[StringLikes], v), "ip6") shouldBe obj("type" -> "string", "format" -> "ipv6")
        prop(render(Json.schema[StringLikes], v), "charset") shouldBe obj("type" -> "string", "pattern" -> CharsetPattern)
      }
    }

    // every draft: patterns and plain strings are always asserted
    for ((name, schema, valid, invalid) <- patterned) {
      test(s"${p.name}: $name accepts what the JDK and the codecs write") {
        Acceptance.assertValid(Acceptance.rendered(schema), valid.map(quoted))
      }
      test(s"${p.name}: $name rejects other text") {
        Acceptance.assertInvalid(Acceptance.rendered(schema), invalid.map(quoted) :+ "1")
      }
    }

    // a format is asserted on draft-04 / draft-07 only (networknt treats it as an annotation on 2020-12)
    for ((name, schema, valid, invalid) <- formatted) {
      test(s"${p.name}: $name accepts what the JDK writes") {
        Acceptance.assertValid(Acceptance.rendered(schema), valid.map(quoted))
      }
      test(s"${p.name}: $name rejects other text (where the format is asserted)") {
        val render = Acceptance.rendered(schema)
        Acceptance.drafts.filterNot(_.isInstanceOf[Version.Draft12]) foreach { d =>
          invalid.map(quoted) foreach { doc => Acceptance.errors(d, render(d), doc) should not be empty }
        }
      }
    }
  }
}

object StringLikeSchemaSpec {
  case class StringLikes(
    locale: Locale,
    currency: Currency,
    pattern: Pattern,
    regex: Regex,
    ip: InetAddress,
    ip4: Inet4Address,
    ip6: Inet6Address,
    path: Path,
    file: File,
    charset: Charset
  )

  val CurrencyPattern = "^[A-Z]{3}$"
  val CharsetPattern  = "^[A-Za-z0-9][A-Za-z0-9+:_.-]*$"

  /** A JSON string literal (the regex samples have backslashes). */
  def quoted(s: String): String = "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"") + "\""

  /** name, schema, texts the JDK / the codecs write, other texts. */
  val patterned: List[(String, Schema[_], List[String], List[String])] = List(
    ("Currency", Json.schema[Currency], List("USD", "EUR", "XAU"), List("usd", "US", "USDX", "")),
    ("Charset", Json.schema[Charset], List("UTF-8", "US-ASCII", "ISO-8859-1", "windows-1252", "x-UTF-16LE-BOM", "IBM01140"), List("", "UTF 8", "-utf8", "utf8;")),
    ("Locale", Json.schema[Locale], List("en_US", "en-US", "sr_RS_#Latn", "sr-Latn-RS", "", "und"), Nil),
    ("InetAddress", Json.schema[InetAddress], List("127.0.0.1", "0:0:0:0:0:0:0:1", "example.com"), Nil),
    ("Path", Json.schema[Path], List("/tmp/a b/c.txt", "rel/c.txt", "C:\\dir\\f.txt"), Nil),
    ("File", Json.schema[File], List("/tmp/a b/c.txt", "rel/c.txt"), Nil)
  )

  val formatted: List[(String, Schema[_], List[String], List[String])] = List(
    ("Pattern", Json.schema[Pattern], List("^a+\\d$", "a++", "(?<name>a)"), List("(", "[a-")),
    ("Regex", Json.schema[Regex], List("^a+\\d$", "[0-9]{3}"), List("(")),
    ("Inet4Address", Json.schema[Inet4Address], List("10.0.0.1", "127.0.0.1"), List("::1", "example.com", "10.0.0.256")),
    ("Inet6Address", Json.schema[Inet6Address], List("0:0:0:0:0:0:0:1", "::1", "2001:db8:0:0:0:0:2:1"), List("10.0.0.1", "example.com"))
  )
}
