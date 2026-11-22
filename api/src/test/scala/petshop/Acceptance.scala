package petshop

import com.github.andyglow.json.JsonFormatter
import com.github.andyglow.jsonschema.AsValue
import json.schema.{JsonSchemaProfile, Version}
import org.scalatest.Assertions._

/** The profiles acceptance harness: a JSON document a library really wrote, validated against a schema rendered under a profile, by a JSON Schema validator (networknt json-schema-validator, a
  * test-only dependency of `api`, JVM only: see [[JsonValidator]]). Bridge modules get it through `api % "test->test"`.
  */
object Acceptance {

  val isScala3: Boolean = com.github.andyglow.jsonschema.harness.Golden.scalaBinaryVersion == "3"

  /** The drafts every case is checked on. */
  val drafts: List[Version] = List(Version.Draft04(), Version.Draft07("https://petshop.test/schema.json"), Version.Draft12("https://petshop.test/schema.json"))

  /** A render for each of [[drafts]] (a render needs the draft's concrete type for its `AsValueBuilder`). */
  def byDraft(d04: Version.Draft04 => String, d07: Version.Draft07 => String, d12: Version.Draft12 => String): Version => String = {
    case v: Version.Draft04 => d04(v)
    case v: Version.Draft07 => d07(v)
    case v: Version.Draft12 => d12(v)
    case v                  => fail(s"not one of the suite's drafts: $v")
  }

  /** The schema as rendered by `AsValue` under `profile`, for each of [[drafts]]. */
  def rendered(schema: json.Schema[_])(implicit profile: JsonSchemaProfile): Version => String =
    byDraft(
      v => JsonFormatter.format(AsValue.schema(schema, v)),
      v => JsonFormatter.format(AsValue.schema(schema, v)),
      v => JsonFormatter.format(AsValue.schema(schema, v))
    )

  /** The validator's errors for `doc` under `schema` (a rendered schema, as JSON text) on `draft`; empty when valid. JVM only: on Scala.js this cancels the test (see [[JsonValidator]]). */
  def errors(draft: Version, schema: String, doc: String): List[String] = JsonValidator.errors(draft, schema, doc)

  /** For each draft, how many of `docs` are valid under the schema `render(draft)` gives. */
  def validCount(render: Version => String, docs: List[String]): Map[String, Int] =
    drafts.map { d =>
      val schema = render(d)
      name(d) -> docs.count(errors(d, schema, _).isEmpty)
    }.toMap

  /** Fails unless every one of `docs` is valid under `render(draft)` on every draft, with the schema, the document and the errors in the message. */
  def assertValid(render: Version => String, docs: List[String]): Unit = drafts foreach { d =>
    val schema = render(d)
    docs foreach { doc =>
      val es = errors(d, schema, doc)
      if (es.nonEmpty) fail(s"${name(d)}: $doc is not valid: ${es.mkString("; ")}\nschema: $schema")
    }
  }

  /** Fails unless every one of `docs` is invalid under `render(draft)` on every draft. */
  def assertInvalid(render: Version => String, docs: List[String]): Unit = drafts foreach { d =>
    val schema = render(d)
    docs foreach { doc =>
      if (errors(d, schema, doc).isEmpty) fail(s"${name(d)}: $doc is valid, but it shouldn't be\nschema: $schema")
    }
  }

  /** The cross-check row of a profile: for each library in [[LibraryOutputs]], how many of its two documents the pet schema `render(draft)` accepts (the same on every draft, or this fails). */
  def crossCheckRow(render: Version => String): List[(String, Int)] = LibraryOutputs.all map { case (lib, docs) =>
    val counts = validCount(render, docs)
    if (counts.values.toSet.size != 1) fail(s"$lib: the drafts disagree: $counts")
    lib -> counts.values.head
  }

  /** Equal as JSON (object key order ignored). */
  def sameJson(a: String, b: String): Boolean = JsonValidator.sameJson(a, b)

  def name(v: Version): String = v match {
    case _: Version.Draft04 => "draft-04"
    case _: Version.Draft06 => "draft-06"
    case _: Version.Draft07 => "draft-07"
    case _: Version.Draft09 => "2019-09"
    case _: Version.Draft12 => "2020-12"
    case Version.Raw        => "raw"
  }
}
