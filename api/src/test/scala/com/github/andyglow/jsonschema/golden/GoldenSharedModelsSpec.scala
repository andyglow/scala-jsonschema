package com.github.andyglow.jsonschema.golden

import com.github.andyglow.jsonschema._
import com.github.andyglow.jsonschema.harness.Golden
import com.github.andyglow.jsonschema.model._
import json.Json
import json.schema.Flag
import org.scalatest.funsuite.AnyFunSuite

/** Golden files for the models the existing specs share (they live in those specs' companion objects or at the top level of the test package). The plain products are in [[GoldenProductModelsSpec]].
  * The existing specs check these models with order-insensitive matchers; the golden files pin the exact Schema tree (incl. field order and ref names) and the rendered JSON.
  */
class GoldenSharedModelsSpec extends AnyFunSuite {
  import Golden.check

  // SchemaMacroSpec
  test("SchemaMacroSpec models") {
    import SchemaMacroSpec._
    check("shared/SchemaMacroSpec.Foo1", Json.schema[Foo1])
    check("shared/SchemaMacroSpec.Foo2", Json.schema[Foo2])
    check("shared/SchemaMacroSpec.Foo3", Json.schema[Foo3])
    check("shared/SchemaMacroSpec.Foo4", Json.schema[Foo4])
    check("shared/SchemaMacroSpec.Bar5", Json.schema[Bar5])
    check("shared/SchemaMacroSpec.Bar6", Json.schema[Bar6])
    check("shared/SchemaMacroSpec.Bar7", Json.schema[Bar7])
    check("shared/SchemaMacroSpec.Bar8", Json.schema[Bar8])
    check("shared/SchemaMacroSpec.Bar9", Json.schema[Bar9])
    check("shared/SchemaMacroSpec.Compo1", Json.schema[Compo1])
  }

  test("sum types (SchemaMacroSpec top-level families)") {
    check("shared/FooBar", Json.schema[FooBar])
    check("shared/MultiLevelSealedTraitRoot", Json.schema[MultiLevelSealedTraitRoot])
    check("shared/FooBarInsideCompanion", Json.schema[FooBarInsideCompanion])
    check("shared/AnyFooBar", Json.schema[AnyFooBar])
    check("shared/HybridSum", Json.schema[HybridSum])
    check("shared/HybridGenericSum[Int]", Json.schema[HybridGenericSum[Int]])
    check("shared/HybridRecursiveSum", Json.schema[HybridRecursiveSum])
  }

  test("SumTypeModels") {
    import SumTypeModels._
    check("shared/SumTypeModels.FooBarInsideCompanionWithAnnotations", Json.schema[FooBarInsideCompanionWithAnnotations])
  }

  test("SumTypeModels, EnumsAsOneOf") {
    import SumTypeModels._
    // NOTE: keep the flag in its own scope. On Scala 2 a local implicit declared LATER in the same block already applies
    // to the calls before it.
    implicit val flag: Flag with Flag.EnumsAsOneOf = null
    check("shared/SumTypeModels.FooBarInsideCompanionWithAnnotations.EnumsAsOneOf", Json.schema[FooBarInsideCompanionWithAnnotations])
  }

  test("model package (sum types)") {
    check("shared/model.Active", Json.schema[Active])
    check("shared/model.BetaFeature", Json.schema[BetaFeature])
    check("shared/model.Role", Json.schema[Role])
  }

  test("DefinitionsSpec models") {
    import DefinitionsSpec._
    check("shared/DefinitionsSpec.User", Json.schema[User])
    check("shared/DefinitionsSpec.Pet", Json.schema[Pet])
  }

  test("DiscriminatorSpec models") {
    import DiscriminatorSpec._
    check("shared/DiscriminatorSpec.case0", Json.schema[case0.Root])
    check("shared/DiscriminatorSpec.case1", Json.schema[case1.Root])
    check("shared/DiscriminatorSpec.case3", Json.schema[case3.Root])
    check("shared/DiscriminatorSpec.case4", Json.schema[case4.Root])
    check("shared/DiscriminatorSpec.case5", Json.schema[case5.Root])
  }

  test("EnumSpec models") {
    import EnumSpec._
    check("shared/EnumSpec.case0", Json.schema[case0])
    check("shared/EnumSpec.case1", Json.schema[case1])
    check("shared/EnumSpec.case2", Json.schema[case2])
    check("shared/EnumSpec.case4", Json.schema[case4])
    check("shared/EnumSpec.case6", Json.schema[case6])
    check("shared/EnumSpec.case8", Json.schema[case8])
  }

  test("EnumSpec models, EnumsAsOneOf") {
    import EnumSpec._
    implicit val flag: Flag with Flag.EnumsAsOneOf = null
    check("shared/EnumSpec.case0.EnumsAsOneOf", Json.schema[case0])
    check("shared/EnumSpec.case8.EnumsAsOneOf", Json.schema[case8])
  }

  test("RecursiveTypesSpec models") {
    import RecursiveTypesSpec._
    check("shared/RecursiveTypesSpec.case1.SList", Json.schema[case1.SList])
    check("shared/RecursiveTypesSpec.case2.Node", Json.schema[case2.Node])
    check("shared/RecursiveTypesSpec.case3.User", Json.schema[case3.User])
  }
}
