package petshop

import json.schema.Version
import org.scalatest.Assertions._

import scala.scalajs.js

/** The JSON Schema validator of the acceptance harness ([[Acceptance]]). The JVM one is networknt json-schema-validator, a Java library. There is none on Scala.js: a test that validates is canceled
  * here (reported as canceled, not passed), and the tests that compare a library's output with the recorded one still run.
  */
object JsonValidator {

  def errors(draft: Version, schema: String, doc: String): List[String] =
    cancel(s"no JSON Schema validator on Scala.js (networknt json-schema-validator is JVM only); ${Acceptance.name(draft)} validation runs on the JVM")

  /** Equal as JSON (object key order ignored). Numbers are JS numbers here, so `1` and `1.0` are equal (the JVM's Jackson tells them apart). */
  def sameJson(a: String, b: String): Boolean = same(js.JSON.parse(a), js.JSON.parse(b))

  private def same(a: js.Dynamic, b: js.Dynamic): Boolean =
    if (js.Array.isArray(a) || js.Array.isArray(b)) {
      js.Array.isArray(a) && js.Array.isArray(b) && {
        val x = a.asInstanceOf[js.Array[js.Dynamic]]
        val y = b.asInstanceOf[js.Array[js.Dynamic]]
        x.length == y.length && x.indices.forall(i => same(x(i), y(i)))
      }
    } else if (isObject(a) || isObject(b)) {
      isObject(a) && isObject(b) && {
        val x = a.asInstanceOf[js.Dictionary[js.Dynamic]]
        val y = b.asInstanceOf[js.Dictionary[js.Dynamic]]
        x.keySet == y.keySet && x.keys.forall(k => same(x(k), y(k)))
      }
    } else (a: Any) == (b: Any)

  private def isObject(v: js.Dynamic): Boolean = v != null && js.typeOf(v) == "object"
}
