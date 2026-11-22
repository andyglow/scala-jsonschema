package petshop

import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.networknt.schema.{InputFormat, JsonSchemaFactory, SpecVersion}
import json.schema.Version
import org.scalatest.Assertions._

/** The JSON Schema validator of the acceptance harness ([[Acceptance]]): networknt json-schema-validator, a Java library, so JVM only. The Scala.js counterpart cancels the tests that validate. */
object JsonValidator {

  private val mapper = new ObjectMapper

  def parse(json: String): JsonNode = mapper.readTree(json)

  /** The validator's errors for `doc` under `schema` (a rendered schema, as JSON text) on `draft`; empty when valid. */
  def errors(draft: Version, schema: String, doc: String): List[String] = {
    val flag = draft match {
      case _: Version.Draft04 => SpecVersion.VersionFlag.V4
      case _: Version.Draft06 => SpecVersion.VersionFlag.V6
      case _: Version.Draft07 => SpecVersion.VersionFlag.V7
      case _: Version.Draft09 => SpecVersion.VersionFlag.V201909
      case _: Version.Draft12 => SpecVersion.VersionFlag.V202012
      case Version.Raw        => fail("a raw render has no draft to validate with")
    }
    val it = JsonSchemaFactory.getInstance(flag).getSchema(schema).validate(doc, InputFormat.JSON).iterator() // no JavaConverters: deprecated on 2.13, the shim differs per version
    val es = List.newBuilder[String]
    while (it.hasNext) es += it.next().getMessage
    es.result().sorted
  }

  /** Equal as JSON (object key order ignored). */
  def sameJson(a: String, b: String): Boolean = parse(a) == parse(b)
}
