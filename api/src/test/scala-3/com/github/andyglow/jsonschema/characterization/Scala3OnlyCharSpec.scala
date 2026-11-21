package com.github.andyglow.jsonschema.characterization

import com.github.andyglow.json.Value._
import com.github.andyglow.jsonschema.harness.TriageA
import json._
import json.Schema._
import json.Schema.`object`.Field
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

/** Expected Scala 3 values for TRIAGE-A behaviors that don't compile on Scala 2 at all.
  *
  * NOT COMPILED YET: written against the Scala 2 API by hand. The s3/build scaffolding skips test files that call the macro
  * on Scala 3 until the Scala 3 derivation exists; then this file runs.
  */
class Scala3OnlyCharSpec extends AnyFunSuite {
  import FieldsAnnotationsCharSpec._

  test("F4: `= Some(x)` emits the default via ToValue", TriageA) {
    import OptionToValue._
    Json.schema[SomeDefault] shouldEqual `object`[SomeDefault](Field.fromJson("b", `string`, required = false, default = Some(str("x"))))
  }

  test("F4: `= Option.empty[String]` emits no default", TriageA) {
    Json.schema[OptionEmptyTypedDefault] shouldEqual `object`[OptionEmptyTypedDefault](Field("a", `string`, required = false))
  }
}
