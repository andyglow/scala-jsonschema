package com.github.andyglow.jsonschema.harness

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

import com.github.andyglow.json.Value._
import json.Schema
import json.Schema.{`not` => _, _}
import json.Schema.`object`.Field
import org.scalatest.exceptions.TestFailedException
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

import scala.collection.immutable.ListSet

class GoldenHarnessSpec extends AnyFunSuite {

  private def tmpDir(): Path = Files.createTempDirectory("golden-harness")

  private val schema = `object`[Any](Field("b", `string`), Field("a", `ref`[Any]("x"), required = false))

  test("a missing golden file fails, update mode creates it") {
    val dir = tmpDir()
    val err = intercept[TestFailedException] { Golden.checkIn(dir, "g/s", schema, update = false) }
    err.getMessage should include("golden file is missing")
    Files.exists(dir.resolve("g/s.json")) shouldBe false

    Golden.checkIn(dir, "g/s", schema, update = true)
    Files.exists(dir.resolve("g/s.json")) shouldBe true
    Golden.checkIn(dir, "g/s", schema, update = false)
  }

  test("a different golden file fails with a diff, update mode overwrites it") {
    val dir = tmpDir()
    Golden.checkIn(dir, "s", schema, update = true)
    val other = `object`[Any](Field("b", `string`), Field("a", `ref`[Any]("y"), required = false))
    val err   = intercept[TestFailedException] { Golden.checkIn(dir, "s", other, update = false) }
    err.getMessage should include("golden mismatch")
    err.getMessage should include("-  ")
    err.getMessage should include("+  ")
    Golden.checkIn(dir, "s", other, update = true)
    Golden.checkIn(dir, "s", other, update = false)
  }

  test("a version-specific golden file takes precedence") {
    val dir = tmpDir()
    Golden.checkIn(dir, "s", schema, update = true)
    val other = `object`[Any](Field("x", `string`))
    Files.write(dir.resolve("s@" + Golden.scalaBinaryVersion + ".json"), Golden.render(other).getBytes(UTF_8))
    Golden.checkIn(dir, "s", other, update = false)
    intercept[TestFailedException] { Golden.checkIn(dir, "s", schema, update = false) }
    Golden.scalaBinaryVersion should (be("2.12") or be("2.13") or be("3"))
  }

  test("CRLF golden files are accepted") {
    val dir = tmpDir()
    Files.write(dir.resolve("s.json"), Golden.render(schema).replace("\n", "\r\n").getBytes(UTF_8))
    Golden.checkIn(dir, "s", schema, update = false)
  }

  test("the dump keeps field order and ref names") {
    val dump = Golden.Printer.compact(Golden.SchemaDump(schema))
    dump.indexOf("\"name\":\"b\"") should be < dump.indexOf("\"name\":\"a\"")
    dump should include("\"kind\":\"ref\",\"sig\":\"x\"")
    Golden.render(schema) should not be Golden.render(`object`[Any](Field("a", `ref`[Any]("x"), required = false), Field("b", `string`)))
  }

  test("the dump is independent of Set iteration order") {
    val members = (1 to 10).toList.map(i => `object`[Any](Field("f" + i, `integer`)))
    // insertion-ordered Sets, so the two really iterate in different orders (two HashSets would iterate alike)
    val forward  = ListSet[Schema[_]](members: _*)
    val backward = ListSet[Schema[_]](members.reverse: _*)
    forward.toList should not be backward.toList
    Golden.render(`oneof`[Any](forward)) shouldBe Golden.render(`oneof`[Any](backward))
    Golden.render(`enum`[Any](`string`, (1 to 10).map(i => str("v" + i): com.github.andyglow.json.Value).toSet)) should include("\"v10\"")
  }

  test("the dump records free objects, metadata and validations") {
    Golden.render(`object`[Any](Field("a", `string`)).free) should include("\"free\": true")
    val out = Golden.render(`object`[Any](Field("a", `string`)).free.withTitle("T").withDescription("D"))
    out should include("\"free\": true")
    out should include("\"title\": \"T\"")
    out should include("\"description\": \"D\"")
    import json.schema.validation.Instance._
    Golden.render(`string`.withValidation(`minLength` := 3)) should include("\"name\": \"minLength\"")
  }

  test("canonical form sorts object keys and arrays") {
    val v = obj("b" -> arr(str("y"), str("x")), "a" -> num(1))
    Golden.Printer.compact(Golden.canonical(v)) shouldBe """{"a":1,"b":["x","y"]}"""
  }

  test("a renderer crash is recorded, not thrown") {
    // known renderer crash: allOf calls `head.jsonType`, which is `???` for a def
    var out = ""
    noException should be thrownBy { out = Golden.render(`allof`.of[Any](`def`[Any]("a", `string`), `def`[Any]("b", `integer`))) }
    out should include("\"renderError\"")
  }
}
