package com.github.andyglow.jsonschema.skeleton

import com.github.andyglow.jsonschema.harness.CompileErrors.errorOf
import com.github.andyglow.jsonschema.harness.ErrorPhrases
import com.github.andyglow.jsonschema.harness.TriageB
import json.*
import json.Schema.*
import json.Schema.`object`.Field
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*

object Scala3ResolutionSpec {
  case class Leaf(a: Int)

  case class Derived1(x: Int, leaf: Leaf) derives json.Schema
  case class UsesDerived(d: Derived1, ds: Option[Derived1])
  case class RecDerived(value: Int, next: Option[RecDerived]) derives json.Schema

  object GivenSelf {
    given Schema[Leaf] = Json.schema[Leaf]
  }

  case class HasLeaf(leaf: Leaf, opt: Option[Leaf])
  case class Linked(value: Int, next: Option[Linked])

  // TRIAGE-B: covariance. An implicit Schema[OpenImpl] must not be used for `Open` (it would satisfy Schema[Open]).
  trait Open
  case class OpenImpl(z: Int) extends Open
  case class HasOpen(o: Open)
  object OpenImplImplicit { implicit val implS: Schema[OpenImpl] = Json.schema[OpenImpl] }

  // TRIAGE-B: refs and defs agree through an alias
  case class ViaAlias(next: Option[Alias])
  type Alias = ViaAlias

  case class Empty()
  case class GenericDefault[T](xs: List[T] = Nil)
  case class GenericValue[T](v: T) extends AnyVal
  case class HasGenericValue(g: GenericValue[Int])
  case class SomeDefault(o: Option[Int] = Some(3))

  // aligned with the Scala 2 macro's s2fix changes (lead-approved): every type is dealiased, signatures are decoded
  case class HasStrings(s: String, o: Option[String])
  object StringSchema { implicit val stringSchema: Schema[String] = `string`[String] }
  case class Box[T](v: T)
  type IntBox = Box[Int]
  case class `odd name`(x: Int)
  trait T1
  trait T2
  object LeafSchema { implicit val leafS: Schema[Leaf] = `object`[Leaf](Field("a", `integer`)) }

  // B1 (lead decision 2026-11-20): an implicit Schema for an abstract type is never a definition named after the type parameter
  case class GBox[T](value: T) derives json.Schema
  case class Person(name: String) derives json.Schema
  case class VId(v: Int) extends AnyVal derives json.Schema
  case class TwoBoxes(a: GBox[Int], b: GBox[String])
  case class GRec[T](v: T, next: Option[GRec[T]]) derives json.Schema
  case class TwoRecs(a: GRec[Int], b: GRec[String])
  case object Lone
  // a candidate that fails to expand, like refined's macro for an empty range
  trait Bounded
  object BoundedPredef { inline given boundedP: json.schema.Predef[Bounded] = scala.compiletime.error("There are no values inside of specified range [2.0 .. 7.0]") }
  case class Ex(s: String)
  object ExDef       { given exS: Schema[Ex] = Json.schema[Ex].toDefinition("custom.Ex") }
  object PlainGivens {
    given intS: Schema[Int]       = `integer`
    given stringS: Schema[String] = `string`[String]
  }
  class PathOuter {
    case class Inner(x: Int = 1)
    def innerSchema: Schema[Inner] = Json.schema[Inner]
  }
  case class PBox[T](v: T)
  object PBox { implicit def pboxS[T](implicit t: Schema[T]): Schema[PBox[T]] = Json.schema[PBox[T]] }
}

/** Scala 3-only behavior of the skeleton: `derives`, `given`s, auto derivation, and TRIAGE-B fixes that the Scala 2 macro doesn't have yet. */
class Scala3ResolutionSpec extends AnyFunSuite {
  import Scala3ResolutionSpec.*

  private val leaf    = `object`[Leaf](Field("a", `integer`))
  private val leafSig = "com.github.andyglow.jsonschema.skeleton.Scala3ResolutionSpec.Leaf"

  test("derives json.Schema is a user implicit Schema: a definition named after the type, at the source (lead decision: B1 (b))") {
    val d1 = `def`[Derived1]("com.github.andyglow.jsonschema.skeleton.Scala3ResolutionSpec.Derived1", `object`[Derived1](Field("x", `integer`), Field("leaf", leaf)))
    summon[Schema[Derived1]] shouldEqual d1
    Json.schema[UsesDerived] shouldEqual `object`[UsesDerived](Field("d", d1), Field("ds", d1, required = false))
    // a root with a derives given is a root implicit Schema (Scala 2 quirk 3: already a definition, kept)
    Json.schema[Derived1] shouldEqual d1
    // a value class is defined by its underlying schema, like an implicit Schema of a value class in Scala 2
    summon[Schema[VId]] shouldEqual `def`[VId]("com.github.andyglow.jsonschema.skeleton.Scala3ResolutionSpec.VId", `integer`)
  }

  test("derives json.Schema on a recursive type") {
    val sig = "com.github.andyglow.jsonschema.skeleton.Scala3ResolutionSpec.RecDerived"
    summon[Schema[RecDerived]] shouldEqual `def`[RecDerived](sig, `object`(Field("value", `integer`), Field("next", `ref`[RecDerived](sig), required = false)))
  }

  test("a given defined with Json.schema doesn't pick itself up (and doesn't deadlock at runtime)") {
    GivenSelf.given_Schema_Leaf shouldEqual leaf
  }

  test("a local implicit defined with Json.schema doesn't pick itself up (Scala 2: forward-reference error)") {
    implicit val local: Schema[Leaf] = Json.schema[Leaf]
    local shouldEqual leaf
  }

  test("auto: `import Json.auto.given` derives nested types inline, like plain derivation") {
    import Json.auto.given
    summon[json.schema.Predef[HasLeaf]].schema shouldEqual Json.schema[HasLeaf]
    summon[json.schema.Predef[HasLeaf]].schema shouldEqual `object`[HasLeaf](Field("leaf", leaf), Field("opt", leaf, required = false))
    summon[json.schema.Predef[Linked]].schema shouldEqual Json.schema[Linked]
    // built-ins keep their own Predef: auto declines for types it can't derive
    summon[json.schema.Predef[Int]].schema shouldEqual `integer`
    summon[json.schema.Predef[List[Int]]].schema shouldEqual `array`[Int, List](`integer`)
  }

  test("auto: the Scala 2 form `import Json.auto._` works too, and a user Schema still becomes a definition") {
    import Json.auto._
    import GivenSelf.given
    summon[json.schema.Predef[HasLeaf]].schema shouldEqual `object`[HasLeaf](Field("leaf", `def`[Leaf](leafSig, leaf)), Field("opt", `def`[Leaf](leafSig, leaf), required = false))
  }

  test("an implicit Schema[Sub] is not used for its supertype", TriageB) {
    import OpenImplImplicit.*
    implS shouldEqual `object`[OpenImpl](Field("z", `integer`))
    errorOf(assertCompiles("Json.schema[HasOpen]")) should include("Scala3ResolutionSpec.Open " + ErrorPhrases.NotSupported)
  }

  test("refs and defs agree on names through a type alias", TriageB) {
    val sig = "com.github.andyglow.jsonschema.skeleton.Scala3ResolutionSpec.ViaAlias"
    Json.schema[ViaAlias] shouldEqual `def`[ViaAlias](sig, `object`(Field("next", `ref`[ViaAlias](sig), required = false)))
  }

  test("a zero-field case class is an empty object", TriageB) {
    Json.schema[Empty] shouldEqual `object`.fromList[Empty](Nil)
  }

  test("a generic default `List[T] = Nil` uses the substituted type", TriageB) {
    Json.schema[GenericDefault[Int]] shouldEqual `object`[GenericDefault[Int]](
      Field.fromJson[List[Int]]("xs", `array`[Int, List](`integer`), required = false, default = Some(com.github.andyglow.json.Value.arr()))
    )
  }

  test("a generic value class gets its type argument", TriageB) {
    Json.schema[HasGenericValue] shouldEqual `object`[HasGenericValue](Field("g", `value-class`[GenericValue[Int], Int](`integer`)))
  }

  test("every type is dealiased: an implicit Schema[String] is the definition java.lang.String, at the root too") {
    import StringSchema.*
    val s = `def`[String]("java.lang.String", `string`)
    Json.schema[HasStrings] shouldEqual `object`[HasStrings](Field("s", s), Field("o", s, required = false))
    Json.schema[String] shouldEqual s
    Json.schema[IntBox] shouldEqual `object`[Box[Int]](Field("v", `integer`))
  }

  test("Json.sig: decoded names, aliases kept, compound types") {
    Json.sig[`odd name`].signature shouldEqual "com.github.andyglow.jsonschema.skeleton.Scala3ResolutionSpec.odd name"
    Json.sig[IntBox].signature shouldEqual "com.github.andyglow.jsonschema.skeleton.Scala3ResolutionSpec.IntBox"
    Json.sig[T1 with T2].signature shouldEqual "com.github.andyglow.jsonschema.skeleton.Scala3ResolutionSpec.T1 with com.github.andyglow.jsonschema.skeleton.Scala3ResolutionSpec.T2"
    Json.sig[Int @unchecked].signature shouldEqual "scala.Int"
    errorOf(assertCompiles("Json.sig[AnyRef { def x: Int }]")) should include(ErrorPhrases.UnnamedType)
  }

  test("objectSchema on a root with an implicit Schema in scope: compile error") {
    import LeafSchema.*
    leafS shouldEqual leaf
    errorOf(assertCompiles("Json.objectSchema[Leaf]()")) should include(ErrorPhrases.NotAnObjectSchema)
  }

  test("objectSchema on a case object gets past the precondition, like Scala 2, and isn't supported") {
    errorOf(assertCompiles("Json.objectSchema[Lone.type]()")) should include(ErrorPhrases.NotSupported)
  }

  test("an implicit that fails to expand: its reason is part of the 'not supported' error (Scala 2 reports the reason; refined)") {
    val e = errorOf(assertCompiles("{ import BoundedPredef.given; Json.schema[Bounded] }"))
    e should include("Scala3ResolutionSpec.Bounded " + ErrorPhrases.NotSupported)
    e should include("There are no values inside of specified range [2.0 .. 7.0]")
    // a plain miss adds nothing
    errorOf(assertCompiles("Json.schema[HasOpen]")) shouldNot include("an implicit for it failed")
  }

  test("a case class local to a block, with a default") {
    case class Local(x: Int, y: String = "d")
    Json.schema[Local] shouldEqual `object`[Local](Field("x", `integer`), Field.fromJson[String]("y", `string`, required = false, default = Some(com.github.andyglow.json.Value.str("d"))))
  }

  test("an implicit Schema[T] for a type parameter: a definition instance keeps its name, anything else is inlined (Scala 2: def named T)") {
    val ex = `def`[Ex]("custom.Ex", `object`[Ex](Field("s", `string`)))
    import ExDef.given
    summon[Schema[GBox[Ex]]] shouldEqual `object`[GBox[Ex]](Field("value", ex))
    PBox.pboxS[Ex] shouldEqual `object`[PBox[Ex]](Field("v", ex))
    // T has its own derives: its definition keeps its name, so it renders as a $ref to Person
    summon[Schema[GBox[Person]]] shouldEqual `object`[GBox[Person]](
      Field("value", `def`[Person]("com.github.andyglow.jsonschema.skeleton.Scala3ResolutionSpec.Person", `object`[Person](Field("name", `string`))))
    )
    import PlainGivens.given
    summon[Schema[GBox[Int]]] shouldEqual `object`[GBox[Int]](Field("value", `integer`))
    PBox.pboxS[Int] shouldEqual `object`[PBox[Int]](Field("v", `integer`))
  }

  test("generic derives: Json.schema[GBox[Int]] derives inline when there is no Schema[Int] (summon needs one: built-ins are Predefs)") {
    Json.schema[GBox[Int]] shouldEqual `object`[GBox[Int]](Field("value", `integer`))
    errorOf(assertCompiles("summon[Schema[GBox[Int]]]")) should include("json.Schema[Int]")
  }

  test("generic derives: GBox[Int] and GBox[String] side by side are two definitions with their own names") {
    import PlainGivens.given
    val box = "com.github.andyglow.jsonschema.skeleton.Scala3ResolutionSpec.GBox"
    Json.schema[TwoBoxes] shouldEqual `object`[TwoBoxes](
      Field("a", `def`[GBox[Int]](s"$box[scala.Int]", `object`[GBox[Int]](Field("value", `integer`)))),
      Field("b", `def`[GBox[String]](s"$box[scala.Predef.String]", `object`[GBox[String]](Field("value", `string`))))
    )
  }

  private val grec                               = "com.github.andyglow.jsonschema.skeleton.Scala3ResolutionSpec.GRec"
  private def grecDef(sig: String, v: Schema[?]) = `def`[Any](sig, `object`[Any](Field("v", v), Field("next", `ref`[Any](sig), required = false)))

  test("recursive generic derives: the given's provisional definition name is replaced by the consumer's concrete one, refs included (BACKLOG #9)") {
    import PlainGivens.given
    Json.schema[TwoRecs] shouldEqual `object`[TwoRecs](
      Field("a", grecDef(s"$grec[scala.Int]", `integer`)),
      Field("b", grecDef(s"$grec[scala.Predef.String]", `string`))
    )
    // alone, the given's own (provisional) name stays: one definition, its refs agree
    summon[Schema[GRec[Int]]] shouldEqual grecDef(s"$grec[$grec.T]", `integer`)
  }

  test("a default of a path-dependent class goes through the path to its companion (Scala 2: doesn't compile)") {
    val o        = new PathOuter
    val expected = `object`.fromList[Any](List(Field.fromJson[Int]("x", `integer`, required = false, default = Some(com.github.andyglow.json.Value.num(1)))))
    Json.schema[o.Inner] shouldEqual expected
    o.innerSchema shouldEqual expected
  }

  test("an Option field with a Some(..) default needs a ToValue[Option[X]], like Scala 2 (skeleton D2)") {
    errorOf(assertCompiles("Json.schema[SomeDefault]")) should include(ErrorPhrases.NoToValueForDefault)
  }
}
