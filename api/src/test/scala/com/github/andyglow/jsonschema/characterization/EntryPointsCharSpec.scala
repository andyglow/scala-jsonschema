package com.github.andyglow.jsonschema.characterization

import com.github.andyglow.jsonschema.harness.{ErrorPhrases, Golden, Scala, TriageA}
import com.github.andyglow.jsonschema.harness.CompileErrors.errorOf
import json._
import json.Schema._
import json.schema._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

object EntryPointsCharSpec {
  case class Foo(a: Int, b: String)
  case class HasFoo(f: Foo, l: List[Foo], o: Option[Foo])
  sealed trait Sum
  case class S1(x: Int)    extends Sum
  case class S2(y: String) extends Sum
  case class HasSum(s: Sum)
  case class Wrapper(v: Int) extends AnyVal
  class Plain(val x: Int)
  trait JustTrait
  case object Lonely

  @title("Nested Title") @description("Nested Description")
  case class Titled(a: Int)
  case class HasTitled(t: Titled)

  // derived where no implicit is in scope
  object Plain {
    val foo: Schema[Foo] = Json.schema[Foo]
    val sum: Schema[Sum] = Json.schema[Sum]
  }
  object FooImplicit    { implicit val fooS: Schema[Foo] = Plain.foo           }
  object SumImplicit    { implicit val sumS: Schema[Sum] = Plain.sum           }
  object StringImplicit { implicit val strS: Schema[String] = `string`[String] }
  case class HasString(s: String, l: List[String])

  case class Bar(a: Int)
  case class HasBar(b: Bar)
  // Scala 2 detects "self references" by reading the previous and the current source line (isSelfRef). The KEEP THESE
  // TWO LINES ADJACENT: the second derivation then mistakes `barSchema` for a self reference and inlines Bar.
  object AdjacentImplicits {
    implicit val barSchema: Schema[Bar]       = Json.schema[Bar]
    implicit val hasBarSchema: Schema[HasBar] = Json.schema[HasBar]
  }
  object SeparatedImplicits {
    implicit val barSchema: Schema[Bar] = Json.schema[Bar]

    implicit val hasBarSchema: Schema[HasBar] = Json.schema[HasBar]
  }
}

/** Characterization of the entry points and implicit resolution on Scala 2 (inventory §2). */
class EntryPointsCharSpec extends AnyFunSuite {
  import EntryPointsCharSpec._
  import Golden.check

  import ErrorPhrases._

  test("E2: objectSchema rejects value classes, non-case classes, traits; case objects aren't supported") {
    errorOf(assertCompiles("Json.objectSchema[Wrapper]()")) should include(ObjectSchemaVC)
    errorOf(assertCompiles("Json.objectSchema[Plain]()")) should include(ObjectSchemaNonCC)
    errorOf(assertCompiles("Json.objectSchema[JustTrait]()")) should include(ObjectSchemaNonCC)
    errorOf(assertCompiles("Json.objectSchema[Lonely.type]()")) should include(NotSupported)
  }

  test("E2: objectSchema decorations must be literal and must name a field") {
    errorOf(assertCompiles("{ val k = \"a\"; Json.objectSchema[Foo](k -> \"d\") }")) should include(ConstantStrings)
    errorOf(assertCompiles("Json.objectSchema[Foo](\"zz\" -> \"d\")")) should include(UnknownField + "zz")
  }

  test("E2: objectSchema accepts `->`, tuple and Tuple2 decorations") {
    val arrow = Json.objectSchema[Foo]("a" -> "A", "b" -> "B")
    check("char/entry/objectSchema.Foo", arrow)
    Json.objectSchema[Foo](("a", "A"), ("b", "B")) shouldBe arrow
    Json.objectSchema[Foo](Tuple2("a", "A"), Tuple2("b", "B")) shouldBe arrow
  }

  test("R5: an implicit Schema[Foo] in scope makes Json.schema[Foo] a def of it") {
    import FooImplicit._
    check("char/entry/implicit.Foo", Json.schema[Foo])
  }

  test("R2: an implicit Schema of a case class used in a field, a List and an Option") {
    import FooImplicit._
    check("char/entry/implicit.HasFoo", Json.schema[HasFoo])
  }

  test("R2: an implicit Schema of a sum type used in a field") {
    import SumImplicit._
    check("char/entry/implicit.HasSum", Json.schema[HasSum])
  }

  test("N3: a field-level implicit Schema[String] becomes the definition java.lang.String") {
    import StringImplicit._
    check("char/entry/implicit.HasString", Json.schema[HasString])
    check("char/entry/implicit.String", Json.schema[String])
  }

  test("R4: a typed implicit val derives its own type structurally; other implicits become defs", TriageA) {
    val barDef = `def`[Bar]("com.github.andyglow.jsonschema.characterization.EntryPointsCharSpec.Bar", `object`[Bar](`object`.Field("a", `integer`)))
    SeparatedImplicits.barSchema shouldBe `object`[Bar](`object`.Field("a", `integer`))
    SeparatedImplicits.hasBarSchema shouldBe `object`[HasBar](`object`.Field("b", barDef))
    AdjacentImplicits.barSchema shouldBe SeparatedImplicits.barSchema
    // Scala 2: false positive of the source-text self-reference check on adjacent lines, Bar is inlined
    if (Scala.is3) AdjacentImplicits.hasBarSchema shouldBe SeparatedImplicits.hasBarSchema
    else AdjacentImplicits.hasBarSchema shouldBe `object`[HasBar](`object`.Field("b", `object`[Bar](`object`.Field("a", `integer`))))
  }

  test("R8: a nested case class reached via a field gets no title/description") {
    check("char/entry/Titled", Json.schema[Titled])
    check("char/entry/HasTitled", Json.schema[HasTitled])
  }

  test("E3: Json.sig gives the DECODED name of a backticked class; renderers make it a valid definition name") {
    // 0.7 on Scala 2: the encoded fullName (`odd$u0020name`). Owner decision 2026-11-20: Json.sig stays the readable
    // (decoded) name, as Scala 3's fullName is, and only the renderers replace what a draft doesn't allow
    Json.sig[`odd name`].signature shouldBe "com.github.andyglow.jsonschema.characterization.odd name"
    check("char/entry/HasOdd", Json.schema[HasOdd])
  }

  test("E3: Json.sig for local, nested and backticked names") {
    case class LocalCC(x: Int)
    List(
      Json.sig[LocalCC].signature,
      Json.sig[Titled].signature,
      Json.sig[Lonely.type].signature,
      Json.sig[List[Option[Foo]]].signature,
      Json.sig[(Int, String)].signature,
      Json.sig[Either[Int, String]].signature
    ) shouldBe List(
      // a class local to a test method: the owner chain skips the method
      "com.github.andyglow.jsonschema.characterization.EntryPointsCharSpec.LocalCC",
      "com.github.andyglow.jsonschema.characterization.EntryPointsCharSpec.Titled",
      "com.github.andyglow.jsonschema.characterization.EntryPointsCharSpec.Lonely",
      "scala.List[scala.Option[com.github.andyglow.jsonschema.characterization.EntryPointsCharSpec.Foo]]",
      "scala.Tuple2[scala.Int,scala.Predef.String]",
      "scala.Either[scala.Int,scala.Predef.String]"
    )
  }
}
