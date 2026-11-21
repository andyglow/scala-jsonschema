package com.github.andyglow.jsonschema.golden

import com.github.andyglow.jsonschema._
import com.github.andyglow.jsonschema.harness.Golden
import com.github.andyglow.jsonschema.model._
import json.Json
import org.scalatest.funsuite.AnyFunSuite

/** Golden files for the shared models that are plain products (case classes, Option fields, defaults, value classes, generics, collections of built-ins), split out of [[GoldenSharedModelsSpec]] so
  * they run on Scala 3 before the sum/enum/collection families are ported. (SchemaMacroSpec's product models stay there: a companion object can't move out of its class's file, and SchemaMacroSpec
  * needs the sum families.)
  */
class GoldenProductModelsSpec extends AnyFunSuite {
  import Golden.check

  test("model package (products)") {
    check("shared/model.Credentials", Json.schema[Credentials])
    check("shared/model.Notes", Json.schema[Notes])
    check("shared/model.Foo", Json.schema[Foo])
  }

  test("GenericSchemaMacroSpec models") {
    check("shared/GenericCC[Int]", Json.schema[GenericCC[Int]])
    check("shared/DoubleGenericCC[String]", Json.schema[DoubleGenericCC[String]])
    check("shared/DoubleGenericCCWithDefaults[Long]", Json.schema[DoubleGenericCCWithDefaults[Long]])
    check("shared/DoubleGenericCCWithPredefinedDefaults[Long]", Json.schema[DoubleGenericCCWithPredefinedDefaults[Long]])
  }

  test("CRLFSourceSpec models") {
    check("shared/CRLFSourceSpec.CC", Json.schema[CRLFSourceSpec.CC])
  }
}
