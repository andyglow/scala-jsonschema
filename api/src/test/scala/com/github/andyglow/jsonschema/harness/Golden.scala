package com.github.andyglow.jsonschema.harness

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}

import com.github.andyglow.json.{Escaped, Value}
import com.github.andyglow.json.Value._
import com.github.andyglow.jsonschema.AsValue
import json.Schema
import json.Schema._
import json.schema.JsonSchemaProfile
import json.schema.Version._
import org.scalatest.Assertions

import scala.util.control.NonFatal

/** Golden-file parity harness.
  *
  * `Golden.check("group/name", schema)` compares a schema against `api/src/test/resources/golden/group/name.json`. The golden file holds:
  *   - `"schema"`: a lossless dump of the `json.Schema` tree (every node, title/description/discriminationKey, validations, field name/required/default/description/rwMode, def/ref signatures, free
  *     objects). Field order is kept as it is in the Schema value, so field (and therefore `required`) order is checked. Members of Sets (oneOf/allOf members, enum values, validations) are sorted,
  *     because the Schema value has no order for them;
  *   - `"draft-04"`, `"draft-07"`, `"draft-2020-12"`: the rendered JSON, canonicalized (object keys sorted, arrays sorted by their canonical text), so the known renderer `toMap` order bug and Set
  *     iteration order can't make it flaky. A renderer crash is recorded as `{"renderError": "<exception>"}`.
  *
  * The output uses `\n` line endings everywhere.
  *
  * A version-specific `name@2.12.json` (or `@2.13`, `@3`) takes precedence over `name.json`; see [[fileFor]].
  *
  * A missing or different golden file fails the test. To (re)write golden files, run with `GOLDEN_UPDATE=1` in the environment (or `-Dgolden.update=true`), e.g.
  * `GOLDEN_UPDATE=1 sbt "api/testOnly *Golden*"`, then review the diff.
  *
  * Written in plain cross-version Scala so the same sources compile on Scala 2.12, 2.13 and 3.
  */
object Golden extends Assertions {

  private val Id = "golden"

  def updateMode: Boolean =
    sys.env.get("GOLDEN_UPDATE").exists(v => v == "1" || v == "true") || sys.props.get("golden.update").exists(_ == "true")

  /** Directory holding golden files (the source directory, so that update mode writes the real files). */
  lazy val dir: Path = {
    val candidates = sys.props.get("golden.dir").toList ++ List("api/src/test/resources/golden", "src/test/resources/golden")
    candidates.map(Paths.get(_)).find(p => Files.isDirectory(p)) getOrElse {
      fail(s"golden directory not found (looked for ${candidates.mkString(", ")} relative to ${Paths.get("").toAbsolutePath}); set -Dgolden.dir")
    }
  }

  /** The Legacy set: the golden files as they were in 0.7 (`JsonSchemaProfile.Legacy` must reproduce them byte for byte). Never rewritten by update mode, which only adds missing files; set
    * `GOLDEN_UPDATE_LEGACY=1` to rewrite them on purpose.
    */
  lazy val legacyDir: Path = dir.resolveSibling("golden-legacy")

  def legacyUpdateMode: Boolean = sys.env.get("GOLDEN_UPDATE_LEGACY").exists(v => v == "1" || v == "true")

  /** Everything that goes into a golden file, as text. The schema is dumped as it is; the renders use `profile`. */
  def render(schema: Schema[_])(implicit profile: JsonSchemaProfile): String = {
    val drafts: List[(String, () => Value)] = List(
      "draft-04"      -> (() => AsValue.schema(schema, Draft04())),
      "draft-07"      -> (() => AsValue.schema(schema, Draft07(Id))),
      "draft-2020-12" -> (() => AsValue.schema(schema, Draft12(Id)))
    )
    val rendered = drafts map { case (k, f) =>
      val v =
        try canonical(f())
        catch { case NonFatal(e) => obj("renderError" -> str(e.toString)) }
      k -> v
    }
    Printer.print(mkObj(("schema" -> SchemaDump(schema)) :: rendered)) + "\n"
  }

  /** Checks `schema` against both sets: `golden/` (the tree as derived, rendered under the default profile, Neutral) and `golden-legacy/` (the tree lowered by `JsonSchemaProfile.Legacy` and rendered
    * with Legacy as the implicit profile).
    */
  def check(name: String, schema: Schema[_]): Unit = {
    val legacyFile = fileFor(legacyDir, name)
    checkIn(legacyDir, name, schema.withProfile(JsonSchemaProfile.Legacy), legacyUpdateMode || (updateMode && !Files.exists(legacyFile)))(JsonSchemaProfile.Legacy)
    checkIn(dir, name, schema, updateMode)
  }

  /** `2.12`, `2.13` or `3`. (On Scala 3 the standard library still reports 2.13.x, hence the class probe.) */
  lazy val scalaBinaryVersion: String = {
    val isScala3 =
      try { Class.forName("scala.runtime.Scala3RunTime"); true }
      catch { case _: ClassNotFoundException => false }
    if (isScala3) "3" else scala.util.Properties.versionNumberString.split('.').take(2).mkString(".")
  }

  /** The golden file for `name`: a version-specific override `name@<scalaBinaryVersion>.json` if one exists, else `name.json`.
    *
    * Overrides record KNOWN, documented differences between Scala versions (see the harness report). The base file is the target behavior (the one the Scala 3 macro has to reproduce). Update mode
    * writes to whichever file is in use; create an override by copying the base file to `name@2.12.json` and then running update mode on 2.12.
    */
  def fileFor(dir: Path, name: String): Path = {
    val specific = dir.resolve(name + "@" + scalaBinaryVersion + ".json")
    if (Files.exists(specific)) specific else dir.resolve(name + ".json")
  }

  def checkIn(dir: Path, name: String, schema: Schema[_], update: Boolean)(implicit profile: JsonSchemaProfile): Unit = {
    val file   = fileFor(dir, name)
    val actual = render(schema)
    if (update) {
      val existing = if (Files.exists(file)) Some(read(file)) else None
      if (!existing.contains(actual)) {
        Files.createDirectories(file.getParent)
        Files.write(file, actual.getBytes(UTF_8))
        ()
      }
    } else if (!Files.exists(file)) {
      fail(s"golden file is missing: $file. Run with GOLDEN_UPDATE=1 to create it. Actual:\n$actual")
    } else {
      val expected = read(file)
      if (expected != actual) fail(s"golden mismatch: $file (run with GOLDEN_UPDATE=1 to accept)\n${diff(expected, actual)}")
    }
  }

  private def read(file: Path): String = new String(Files.readAllBytes(file), UTF_8).replace("\r\n", "\n")

  /** Line diff: the first differing lines with some context. */
  def diff(expected: String, actual: String): String = {
    val e    = expected.split("\n", -1).toVector
    val a    = actual.split("\n", -1).toVector
    val n    = math.max(e.length, a.length)
    val idx  = (0 until n).find(i => e.lift(i) != a.lift(i)).getOrElse(0)
    val from = math.max(0, idx - 3)
    val to   = math.min(n, idx + 12)
    val sb   = new StringBuilder(s"first difference at line ${idx + 1}\n")
    (from until to) foreach { i =>
      (e.lift(i), a.lift(i)) match {
        case (x, y) if x == y => sb.append("   ").append(x.getOrElse("")).append('\n')
        case (x, y)           =>
          x foreach { l => sb.append("-  ").append(l).append('\n') }
          y foreach { l => sb.append("+  ").append(l).append('\n') }
      }
    }
    sb.toString
  }

  /** An object with the given fields, in this order. */
  def mkObj(fields: List[(String, Value)]): obj = obj(fields.map { case (k, v) => obj.field.some(k, v): obj.field }: _*)

  /** Object keys sorted, arrays sorted by their compact canonical text. */
  def canonical(v: Value): Value = v match {
    case o: obj => mkObj(o.fields.toList.map { case (k, x) => k -> canonical(x) }.sortBy(_._1))
    case a: arr => arr(a.value.map(canonical).sortBy(Printer.compact))
    case x      => x
  }

  /** Deterministic JSON printer: 2-space indent, `\n`, keys in insertion order (callers sort where needed). */
  object Printer {
    def print(v: Value): String = {
      val sb = new StringBuilder
      go(v, sb, 0)
      sb.toString
    }

    def compact(v: Value): String = v match {
      case o: obj => o.fields.map { case (k, x) => "\"" + Escaped(k) + "\":" + compact(x) }.mkString("{", ",", "}")
      case a: arr => a.value.map(compact).mkString("[", ",", "]")
      case x      => scalar(x)
    }

    private def scalar(v: Value): String = v match {
      case s: str => "\"" + Escaped(s.value) + "\""
      case x      => x.toString
    }

    private def go(v: Value, sb: StringBuilder, indent: Int): Unit = {
      val i1 = " " * (indent + 2)
      v match {
        case o: obj if o.fields.isEmpty => sb.append("{}")
        case a: arr if a.value.isEmpty  => sb.append("[]")
        case o: obj                     =>
          sb.append("{\n")
          var first = true
          o.fields foreach { case (k, x) =>
            if (!first) sb.append(",\n")
            first = false
            sb.append(i1).append('"').append(Escaped(k)).append("\": ")
            go(x, sb, indent + 2)
          }
          sb.append('\n').append(" " * indent).append('}')
        case a: arr =>
          sb.append("[\n")
          var first = true
          a.value foreach { x =>
            if (!first) sb.append(",\n")
            first = false
            sb.append(i1)
            go(x, sb, indent + 2)
          }
          sb.append('\n').append(" " * indent).append(']')
        case x => sb.append(scalar(x))
      }
      ()
    }
  }

  /** Lossless dump of a `json.Schema` tree. */
  object SchemaDump {

    def apply(s: Schema[_]): Value = {
      val meta: List[(String, Value)] =
        s.title.toList.map(x => "title" -> (str(x): Value)) ++
          s.description.toList.map(x => "description" -> (str(x): Value)) ++
          s.discriminationKey.toList.map(x => "discriminationKey" -> (str(x): Value)) ++ {
            if (s.validations.isEmpty) Nil
            else
              List("validations" -> (sorted(s.validations.toList.map { v =>
                obj("name" -> str(v.validation.name), "value" -> v.json)
              }): Value))
          }
      val body: List[(String, Value)] = s match {
        case _: `boolean`     => List("kind" -> str("boolean"))
        case _: `integer`     => List("kind" -> str("integer"))
        case _: `number`[_]   => List("kind" -> str("number"))
        case `string`(format) => List("kind" -> str("string")) ++ format.toList.map(f => "format" -> (str(f.toString): Value))
        case `array`(c, u)    => List("kind" -> str("array"), "unique" -> bool(u), "items" -> apply(c))
        case `dictionary`(v)  => List("kind" -> str("dictionary"), "values" -> apply(v))
        case o: `object`[_]   =>
          List("kind" -> str("object")) ++
            (if (classOf[`object`.Free].isInstance(o)) List("free" -> (bool(true): Value)) else Nil) ++
            List("fields" -> arr(o.fields.map(field)))
        case `enum`(t, vs)   => List("kind" -> str("enum"), "type" -> apply(t), "values" -> sorted(vs.toList))
        case `oneof`(ms, df) =>
          List("kind" -> str("oneof"), "members" -> sorted(ms.toList.map(apply))) ++ df.toList.map(x => "discriminationField" -> (str(x): Value))
        case `allof`(ms)      => List("kind" -> str("allof"), "members" -> sorted(ms.toList.map(apply)))
        case `not`(t)         => List("kind" -> str("not"), "schema" -> apply(t))
        case `def`(sig, t)    => List("kind" -> str("def"), "sig" -> str(sig), "schema" -> apply(t))
        case `ref`(sig)       => List("kind" -> str("ref"), "sig" -> str(sig))
        case `value-class`(t) => List("kind" -> str("value-class"), "schema" -> apply(t))
        case `const`(v)       => List("kind" -> str("const"), "value" -> v)
        case _: `null`        => List("kind" -> str("null"))
        case `option`(t)      => List("kind" -> str("option"), "schema" -> apply(t))
        case f: `family`[_]   =>
          List("kind" -> str("family"), "members" -> arr(f.members.map(member))) ++
            f.discriminator.toList.map(d => "discriminator" -> obj("field" -> str(d.field), "phantom" -> bool(d.phantom))) ++
            (if (f.enumsAsOneOf) List("enumsAsOneOf" -> (bool(true): Value)) else Nil)
      }
      mkObj(body ++ meta)
    }

    private def field(f: `object`.Field[_]): Value = {
      val parts: List[(String, Value)] =
        List("name" -> str(f.name), "required" -> bool(f.required)) ++
          f.default.toList.map(d => "default" -> d) ++
          f.description.toList.map(d => "description" -> (str(d): Value)) ++
          List("rwMode" -> str(f.rwMode.toString), "schema" -> apply(f.tpe))
      mkObj(parts)
    }

    // members keep their declaration order (it is part of the fact)
    private def member(m: `family`.Member): Value = {
      val parts: List[(String, Value)] =
        List("name" -> str(m.name), "fullName" -> str(m.fullName), "sig" -> str(m.sig), "kind" -> str(m.kind.toString), "schema" -> apply(m.schema)) ++
          m.value.toList.map(v => "value" -> v) ++
          m.cls.toList.map(c => "class" -> (str(c.getName): Value)) ++
          m.title.toList.map(x => "title" -> (str(x): Value)) ++
          m.description.toList.map(x => "description" -> (str(x): Value)) ++
          m.discriminatorKey.toList.map(x => "discriminatorKey" -> (str(x): Value)) ++
          m.definition.toList.map(x => "definition" -> (str(x.toString): Value))
      mkObj(parts)
    }

    private def sorted(xs: List[Value]): arr = arr(xs.sortBy(x => Printer.compact(canonical(x))))
  }
}
